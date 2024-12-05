module Таблиця

// Build your own Excel 365 in an hour with F# by Tomas Petricek!
// Watch the video of the talk here: https://www.youtube.com/watch?v=Bnm71YEt_lI

module Elmish =

    open System
    open Fable.Core
    open Browser
    open Browser.Types

    // ------------------------------------------------------------------------------------------------
    // Virtual Dom bindings
    // ------------------------------------------------------------------------------------------------

    type IVirtualdom =
        abstract h: arg1: string * arg2: obj * arg3: obj[] -> obj
        abstract diff: tree1:obj * tree2:obj -> obj
        abstract patch: node:obj * patches:obj -> Node
        abstract create: e:obj -> Node

    [<Global("virtualDom")>]
    let Virtualdom: IVirtualdom = jsNative

    // ------------------------------------------------------------------------------------------------
    // F# representation of DOM and rendering using VirtualDom
    // ------------------------------------------------------------------------------------------------

    type АтрібутДом =
        | ОбробникПодій of (Event -> unit)
        | Атрібут of string
        | Властивість of string

    type ВузелДом =
        | Текст of string
        | Елемент of тег:string * атрібути:(string * АтрібутДом)[] * діти : ВузелДом[]

    let створитиДерево тег арги діти =
        let атріби = ResizeArray<_>()
        let властив = ResizeArray<_>()
        for к, зн in арги do
            match к, зн with
            | "style", Атрібут зн
            | "style", Властивість зн ->
                    let args = зн.Split(';') |> Array.map (fun а ->
                        let sep = а.IndexOf(':')
                        if sep > 0 then а.Substring(0, sep), box (а.Substring(sep+1))
                        else а, box "" )
                    властив.Add ("style", JsInterop.createObj args)
            | "class", Атрібут зн
            | "class", Властивість зн ->
                    атріби.Add (к, box зн)
            | к, Атрібут зн ->
                    атріби.Add (к, box зн)
            | к, Властивість зн ->
                    властив.Add (к, box зн)
            | к, ОбробникПодій ф ->
                    властив.Add (к, box ф)
        let атріби = JsInterop.createObj атріби
        let властив = JsInterop.createObj (Seq.append ["attributes", атріби] властив)
        let елем = Virtualdom.h(тег, властив, діти)
        елем

    let rec відобразити вузел =
        match вузел with
        | Текст(с) ->
                box с
        | Елемент(tag, attrs, children) ->
                створитиДерево tag attrs (Array.map відобразити children)

    // ------------------------------------------------------------------------------------------------
    // Helpers for dynamic property access & for creating HTML elements
    // ------------------------------------------------------------------------------------------------

    type Dynamic() =
        [<Emit("$0[$1]")>]
        static member (?) (d:Dynamic, s:string) : Dynamic = jsNative

    let текст s = Текст(s)
    let (=>) k v = k, Властивість(v)
    let (=!>) k f = k, ОбробникПодій(fun e -> f e)

    type El() =
        static member (?) (_:El, n:string) = fun a b ->
            Елемент(n, Array.ofList a, Array.ofList b)

    let h = El()

    // ------------------------------------------------------------------------------------------------
    // Entry point - create event and update on trigger
    // ------------------------------------------------------------------------------------------------

    type Кмд<'Msg> = (('Msg -> unit) -> unit) list

    type SingleObservable<'T>() =
        let mutable слухач: IObserver<'T> option = None
        member _.Trigger v =
            match слухач with
            | Some слух -> слух.OnNext v
            | None -> ()
        interface IObservable<'T> with
            member _.Subscribe w =
                слухач <- Some w
                { new IDisposable with
                    member _.Dispose() = () }

    let апка ід (ініц: unit -> 'Model * Кмд<'Msg>) оновити відображення =
        let подія = new Event<'Msg>()
        let гачок e = подія.Trigger(e)
        let модель, кмди = ініц()
        let mutable стан = модель
        let mutable дерево = відображення стан гачок |> відобразити
        let mutable контейнер = Virtualdom.create(дерево)
        document.getElementById(ід).appendChild(контейнер) |> ignore

        let обробитиПодію под =
            let модель, кмди = оновити под стан
            let новеДерево = відображення модель гачок |> відобразити
            let латки = Virtualdom.diff(дерево, новеДерево)
            контейнер <- Virtualdom.patch(контейнер, латки)
            дерево <- новеДерево
            стан <- модель
            for кмд in кмди do
                кмд гачок

        подія.Publish.Add(обробитиПодію)
        for кмд in кмди do
            кмд гачок

module Парсек =
    type ПотікПарсеру<'T> = int * list<'T>
    type Парсер<'T, 'R> = Парсер of (ПотікПарсеру<'T> -> option<ПотікПарсеру<'T> * 'R>)

    /// Returned by the `slot` function to create a parser slot that is filled later
    type ВстановлювачПарсеру<'T, 'R> =
      { Set : Парсер<'T, 'R> -> unit }

    /// Ignore the result of the parser
    let ігнорувати (Парсер п) = Парсер(fun вхід ->
      п вхід |> Option.map (fun (i, r) -> i, ()))

    /// Creates a delayed parser whose actual parser is set later
    let слот () =
      let mutable слот = None
      { Set = fun (Парсер p) -> слот <- Some p },
      Парсер(fun вхід ->
        match слот with
        | Some слот -> слот вхід
        | None -> failwith "Slot not initialized")

    /// If the input matches the specified prefix, produce the specified result
    let префікс (префікс:list<'C>) result = Парсер(fun (offset, вхід) ->
      let rec цикл (word:list<'C>) вхід =
        match word, вхід with
        | c::word, i::вхід when c = i -> цикл word вхід
        | [], input -> Some(input)
        | _ -> None

      match цикл префікс вхід with
      | Some(вхід) -> Some((offset+List.length префікс, вхід), result)
      | _ -> None)

    /// Parser that succeeds when either of the two arguments succeed
    let (<|>) (Парсер p1) (Парсер p2) = Парсер(fun вхід ->
      match p1 вхід with
      | Some(вхід, рез) -> Some(вхід, рез)
      | _ -> p2 вхід)

    /// Run two parsers in sequence and return the result as a tuple
    let (<*>) (Парсер p1) (Парсер p2) = Парсер(fun input ->
      match p1 input with
      | Some(input, res1) ->
          match p2 input with
          | Some(input, res2) -> Some(input, (res1, res2))
          | _ -> None
      | _ -> None)

    /// Transforms the result of the parser using the specified function
    let map f (Парсер p) = Парсер(fun input ->
      p input |> Option.map (fun (input, res) -> input, f res))

    /// Run two parsers in sequence and return the result of the second one
    let (<*>>) p1 p2 = p1 <*> p2 |> map snd

    /// Run two parsers in sequence and return the result of the first one
    let (<<*>) p1 p2 = p1 <*> p2 |> map fst

    /// Succeed without consuming input
    let unit res = Парсер(fun input -> Some(input, res))

    /// Parse using the first parser and then call a function to produce
    /// next parser and parse the rest of the input with the next parser
    let зв'язати f (Парсер p) = Парсер(fun input ->
      match p input with
      | Some(input, res) ->
          let (Парсер g) = f res
          match g input with
          | Some(input, res) -> Some(input, res)
          | _ -> None
      | _ -> None)

    /// Parser that tries to use a specified parser, but returns None if it fails
    let опціонально (Парсер п) = Парсер(fun вхід ->
      match п вхід with
      | None -> Some(вхід, None)
      | Some(input, res) -> Some(input, Some res) )

    /// Parser that succeeds if the input matches a predicate
    let пред п = Парсер(function
      | offs, c::input when п c -> Some((offs+1, input), c)
      | _ -> None)

    /// Parser that succeeds if the predicate returns Some value
    let вибір п = Парсер(function
      | offs, c::input -> п c |> Option.map (fun c -> (offs + 1, input), c)
      | _ -> None)

    /// Parse zero or more repetitions using the specified parser
    let нульАбоБільше (Парсер п) =
      let rec loop acc input =
        match п input with
        | Some(input, res) -> loop (res::acc) input
        | _ -> Some(input, List.rev acc)
      Парсер(loop [])

    /// Parse one or more repetitions using the specified parser
    let одинАбоБільше п =
      (п <*> (нульАбоБільше п))
      |> map (fun (c, cs) -> c::cs)


    let будьЯкийПробіл = нульАбоБільше (пред (fun t -> t = ' '))

    let символ ток = пред (fun t -> t = ток)

    let розділений роз п =
      п <*> нульАбоБільше (роз <*> п)
      |> map (fun (a1, args) -> a1::(List.map snd args))

    let розділенийПотім роз п1 п2 =
      п1 <*> нульАбоБільше (роз <*> п2)
      |> map (fun (a1, args) -> a1::(List.map snd args))

    let розділенийАбоПустий роз п =
      опціонально (розділений роз п)
      |> map (fun l -> defaultArg l [])

    let цифра = пред (fun t -> t <= '9' && t >= '0')

    let ціле = одинАбоБільше цифра |> map (fun nums ->
      nums |> List.fold (fun res n -> res * 10 + (int n - int '0')) 0)

    let літера = пред (fun t ->
      (t <= 'Z' && t >= 'A') || (t <= 'z' && t >= 'a'))

    let запустити (Парсер(f)) вхід =
      match f (0, List.ofSeq вхід) with
      | Some((i, _), рез) when i = Seq.length вхід -> Some рез
      | _ -> None

module Обчислювач =
    open Парсек

    // ----------------------------------------------------------------------------
    // DOMAIN MODEL
    // ----------------------------------------------------------------------------

    type Позиція = char * int

    type Вираз =
      | Посилання of Позиція
      | Число of int
      | Бінарна of Вираз * char * Вираз

    // ----------------------------------------------------------------------------
    // Парсер
    // ----------------------------------------------------------------------------

    // Basics: operators (+, -, *, /), cell reference (e.g. A10), number (e.g. 123)
    let оператор = символ '+' <|> символ '-' <|> символ '*' <|> символ '/'
    let посилання = літера <*> ціле |> map Посилання
    let число = ціле |> map Число

    // Nested operator uses need to be parethesized, for example (1 + (3 * 4)).
    // <expr> is a binary operator without parentheses, number, reference or
    // nested brackets, while <term> is always bracketed or primitive. We need
    // to use `expr` recursively, which is handled via mutable slots.
    let встановлювачВираз, вираз = слот ()
    let дужк = символ '(' <*>> будьЯкийПробіл <*>> вираз <<*> будьЯкийПробіл <<*> символ ')'
    let терм = число <|> посилання <|> дужк
    let бінарний = терм <<*> будьЯкийПробіл <*> оператор <<*> будьЯкийПробіл <*> терм |> map (fun ((л,оп), п) -> Бінарна(л, оп, п))
    let виразВспом = бінарний <|> терм
    встановлювачВираз.Set виразВспом

    // Formula starts with `=` followed by expression
    // Equation you can write in a cell is either number or a formula
    let формула = символ '=' <*>> будьЯкийПробіл <*>> вираз
    let рівняння = будьЯкийПробіл <*>> (формула <|> число) <<*> будьЯкийПробіл

    // Run the parser on a given input
    let розібрати вхід = запустити рівняння вхід

    // ----------------------------------------------------------------------------
    // обчилювач
    // ----------------------------------------------------------------------------

    let rec обчислити пройдене (комірки:Map<Позиція, string>) вираз =
      match вираз with
      | Число чис ->
          Some чис

      | Бінарна(л, оп, п) ->
          let ops = dict [ '+', (+); '-', (-); '*', (*); '/', (/) ]
          обчислити пройдене комірки л |> Option.bind (fun л ->
            обчислити пройдене комірки п |> Option.map (fun п ->
              ops.[оп] л п ))

      | Посилання поз when Set.contains поз пройдене ->
          None

      | Посилання поз ->
          комірки.TryFind поз |> Option.bind (fun value ->
            розібрати value |> Option.bind (fun розібране ->
              обчислити (Set.add поз пройдене) комірки розібране))

open Elmish
open Обчислювач

// ----------------------------------------------------------------------------
// Домена модель
// ----------------------------------------------------------------------------

type Подія =
  | ОновитиЗначення of Позиція * string
  | ПочатокРедагування of Позиція

type Стан =
  { Рядки : int list
    ЄАктивним : Позиція option
    Стовпчики : char list
    Комірки : Map<Позиція, string> }

type Переміщення =
    | ПереміститиВ of Позиція
    | Некоректне

type Напрямок = Вверх | Вниз | Вліво | Вправо

let КнопкиНапрямку : Map<string, Напрямок> = Map.ofList [
  ("ArrowLeft", Вліво)
  ("ArrowUp", Вверх)
  ("ArrowRight", Вправо)
  ("ArrowDown", Вниз)
]

// ----------------------------------------------------------------------------
// обробка подій
// ----------------------------------------------------------------------------

let оновлення пов стан =
  match пов with
  | ПочатокРедагування(поз) ->
      { стан with ЄАктивним = Some поз }, []

  | ОновитиЗначення(поз, значення) ->
      let новіКомірки =
          if значення = ""
              then Map.remove поз стан.Комірки
              else Map.add поз значення стан.Комірки
      { стан with Комірки = новіКомірки }, []

// ----------------------------------------------------------------------------
// відображення
// ----------------------------------------------------------------------------

let взятиНапрямок (ke: Browser.Types.KeyboardEvent) : Option<Напрямок> =
    Map.tryFind ke.key КнопкиНапрямку

let взятиПозицію ((стов, ряд): Позиція) (напрямок: Напрямок) : Позиція =
    match напрямок with
    | Вверх -> (стов, ряд - 1)
    | Вниз -> (стов, ряд + 1)
    | Вліво -> (char((int стов) - 1), ряд)
    | Вправо -> (char((int стов) + 1), ряд)

let взятиПереміщення (стан: Стан) (напрямок: Напрямок) : Переміщення =
    match стан.ЄАктивним with
    | None -> Некоректне
    | (Some позиція) ->
        let (стов, ряд) = взятиПозицію позиція напрямок
        if List.contains стов стан.Стовпчики && List.contains ряд стан.Рядки
            then ПереміститиВ (стов, ряд)
            else Некоректне

let взятиПодіюНатисканняКнопки стан гачок = fun (ke: Browser.Types.Event) ->
    match взятиНапрямок (ke :?> _) with
    | None -> ()
    | Some напрямок ->
        match взятиПереміщення стан напрямок with
        | Некоректне -> ()
        | ПереміститиВ позиція -> гачок(ПочатокРедагування(позиція))

let відмалюватиРедактор (гачок:Подія -> unit) поз стан значення =
  h?td [ "class" => "selected" ] [
    h?input [
      "autofocus" => "true"
      "onkeydown" =!> (взятиПодіюНатисканняКнопки стан гачок)
      "oninput" =!> (fun e -> гачок (ОновитиЗначення (поз, (e.target :?> Browser.Types.HTMLInputElement).value)))
      "value" => значення ] []
  ]

let відмалюватиВідображення гачок поз (значення:option<_>) =
  h?td
    [ "style" => (if значення.IsNone then "background:#ffb0b0" else "background:white")
      "onclick" =!> (fun _ -> гачок(ПочатокРедагування(поз)) ) ]
    [ Текст (Option.defaultValue "#ERR" значення) ]

let відмалюватиКомірку гачок поз стан =
  let значення = Map.tryFind поз стан.Комірки
  if стан.ЄАктивним = Some поз then
    відмалюватиРедактор гачок поз стан (Option.defaultValue "" значення)
  else
    let значення =
      match значення with
      | Some значення ->
          розібрати значення |> Option.bind (обчислити Set.empty стан.Комірки) |> Option.map string
      | _ -> Some ""
    відмалюватиВідображення гачок поз значення

let відображення стан гачок =
  let пусті = h?td [] []
  let заголовок htext = h?th [] [Текст htext]
  let заголовки = стан.Стовпчики |> List.map (fun заг -> заголовок (string заг))
  let заголовки = пусті::заголовки

  let комірки н =
    let комірки = стан.Стовпчики |> List.map (fun заг -> відмалюватиКомірку гачок (заг, н) стан)
    заголовок (string н) :: комірки
  let рядки = стан.Рядки |> List.map (fun р -> h?tr [] (комірки р))

  h?table [] [
    h?tr [] заголовки
    h?tbody [] рядки
  ]

// ----------------------------------------------------------------------------
// точка входу
// ----------------------------------------------------------------------------

let початкові () =
  { Стовпчики = ['A' .. 'K']
    Рядки = [1 .. 15]
    ЄАктивним = None
    Комірки = Map.empty },
  []

апка "main" початкові оновлення відображення
