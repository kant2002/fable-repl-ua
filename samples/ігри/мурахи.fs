// Ф# Колонія мурах. Редакція Фабле

// Ported from: https://github.com/robertpi/F--Ant-Colony/ which is a folk of: https://github.com/Rickasaurus/F--Ant-Colony

// Original notice:

//
// This is Richard Minerich's F# Ant Colony Silverlight Ediiton
// Visit my Blog at http://RichardMinerich.com
// This code is free to be used for anything you like as long as I am properly acknowledged.
//
// The basic Silverlight used here is based on Phillip Trelford's Missile Command Example
// http://www.trelford.com/blog/post/MissileCommand.aspx
//

module Мурахи

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Browser

module Типи =

    let xSize = 50
    let ySize = 50
    let розмірГнізда = 5
    let maxTotalFoodPerSquare = 200
    let minGeneratedFoodPerSquare = 20
    let maxGeneratedFoodPerSquare = 100
    let максЇжіМурахаМожеНести = 5
    let chanceOfFood = 0.04

    let maxCellPheromoneQuantity = 255
    let maxAntDropPheromoneQunatity = 50
    let pheromoneDispersalRate = 1

    let percentFoodToWin = 0.5
    let maxWorldCycles = 1500

    type УІД = { X: int; Y: int }

    let уід (x, y) = { X = x; Y = y}

    type КолірМурахи =
        | Чорний
        | Червоний

    type ТипКлітиниСвіту =
            | FieldCell
            | КлітинаГнізда of КолірМурахи

    type Мураха =
        { Колір : КолірМурахи
          FoodCarried : int }
        with
            member м.IsFullOfFood = м.FoodCarried >= максЇжіМурахаМожеНести
            member м.HasFood = м.FoodCarried > 0
            member м.MaxPheromonesToDrop = maxAntDropPheromoneQunatity

    and КлітинаСвіту =
        { Ід : УІД
          Їжа : int
          Мураха : option<Мураха>
          ТипКлітини : ТипКлітиниСвіту
          Феромони : Map<КолірМурахи, int> }
        with
            member к.IsFullOfFood = к.Їжа >= maxTotalFoodPerSquare
            member к.ЄЇжа = к.Їжа > 0
            member к.МіститьМураху = к.Мураха.IsSome
            member к.ЄФеромони color = not (к.Феромони.[color] = 0)
            member к.MaxPheromones = maxCellPheromoneQuantity
            member к.MaxFood = maxTotalFoodPerSquare

    and Світ = Map<УІД, КлітинаСвіту>

    and ДіяМурахи =
        | Нічого
        | Перемістити of КлітинаСвіту
        | ВзятиЇжу of КлітинаСвіту
        | КинутиЇжу of КлітинаСвіту
        | КинутиФеромон of КлітинаСвіту * int

    type Гніздо(ix, iy, розмірх, розміру) =
        member internal t.МінХ = ix
        member internal t.МінУ = iy
        member internal t.МаксХ = ix + розмірх
        member internal t.МаксУ = iy + розміру
        member internal t.ЧиУМежах x y = x >= t.МінХ && x <= t.МаксХ && y >= t.МінУ && y <= t.МаксУ
        member t.Дістанція клітина =
                let cx, cy = t.МінХ + ((t.МаксХ - t.МінХ) / 2), t.МінУ + ((t.МаксУ - t.МінУ) / 2)
                let x, y = клітина.Ід.X, клітина.Ід.Y
                let ступінь x = x * x
                sqrt (ступінь(double cx - double x) + ступінь(double cy - double y))
        member t.КількістьЇжі (світ: Світ) =
                Map.fold (fun s (к: УІД) з -> if t.ЧиУМежах к.X к.Y then s + з.Їжа else s) 0 світ


    type ІПоведінкаМурахи =
        abstract member Ім'я : string
        abstract member Поведінка : Мураха -> КлітинаСвіту -> КлітинаСвіту list -> Гніздо -> ДіяМурахи

    type ЗмінаСвіту = Світ -> Світ

module Помічники =

    open System
    open System.Reflection

    module Array =
        let randomPermute a =
            let n = Array.length a
            if n > 0 then
                let rand = new Random()
                let rec aux = function
                    | 0 -> a
                    | k ->
                        let i = rand.Next(k+1)
                        let tmp = a.[i]
                        a.[i] <- a.[k]
                        a.[k] <- tmp
                        aux (k-1)
                aux (n-1)
            else a

    module Seq =
        let randomPermute a =
            a |> Seq.toArray |> Array.randomPermute |> Array.toSeq

    module List =

        let private r = Random(int DateTime.Now.Ticks)
        let random l =
            let index = r.Next(0, List.length l) in
                l.[index]

module Світ =

    open System

    open Типи
    open Помічники

    let ГніздоЧорнихМурах = new Гніздо( 0, 0, розмірГнізда - 1, розмірГнізда - 1 )
    let ГніздоЧервонихМурах = new Гніздо( 1 + xSize - розмірГнізда, 1 + ySize - розмірГнізда, розмірГнізда - 1, розмірГнізда - 1)

    let (|УЧорномуГнізді|УЧервономуГнізді|Ніде|) (x,y) =
        if ГніздоЧорнихМурах.ЧиУМежах x y then УЧорномуГнізді
        elif ГніздоЧервонихМурах.ЧиУМежах x y then УЧервономуГнізді
        else Ніде

    let взятиГніздоМурахи мураха =
        match мураха.Колір with
        | КолірМурахи.Чорний -> ГніздоЧорнихМурах
        | КолірМурахи.Червоний -> ГніздоЧервонихМурах

    let пустийНабірФермнів =
        seq { let кольори = [| КолірМурахи.Чорний; КолірМурахи.Червоний |]
              for колір in кольори do
                yield колір, 0 }
        |> Map.ofSeq

    let клітинаЗаЗамовчанням id = {Ід = id; Їжа = 0; Мураха = None; ТипКлітини = FieldCell; Феромони = пустийНабірФермнів }
    let чорнаМурахаЗаЗамовчанням = Some { Колір = КолірМурахи.Чорний; FoodCarried = 0 }
    let червонаМурахаЗаЗамовчанням = Some { Колір = КолірМурахи.Червоний; FoodCarried = 0 }

    let buildWorldInitialWorld () =
        let слч = new System.Random() in
            seq { for x in 0 .. xSize do
                    for y in 0 .. ySize do
                        let уід = уід (x, y)
                        let клітиназазамовчанням = клітинаЗаЗамовчанням уід
                        match x, y with
                        | УЧорномуГнізді -> yield уід, { клітиназазамовчанням with Мураха = чорнаМурахаЗаЗамовчанням; ТипКлітини = КлітинаГнізда(КолірМурахи.Чорний) }
                        | УЧервономуГнізді ->   yield уід, { клітиназазамовчанням with Мураха = червонаМурахаЗаЗамовчанням; ТипКлітини = КлітинаГнізда(КолірМурахи.Червоний) }
                        | Ніде ->     if chanceOfFood > слч.NextDouble()
                                            then yield уід, { клітиназазамовчанням with Їжа = слч.Next(minGeneratedFoodPerSquare, maxGeneratedFoodPerSquare) }
                                            else yield уід, клітиназазамовчанням
                }
            |> Map.ofSeq

    let getAntViews (світ: Світ) =
        let взятиКлітинуСвіту x y = Map.tryFind (уід (x,y)) світ
        let worldFold state (uid: УІД) cell =
                let x, y = (uid.X, uid.Y)
                match cell.Мураха with
                | None -> state
                | Some(ant) ->
                    let visibleCells = [ взятиКлітинуСвіту x (y - 1); взятиКлітинуСвіту x (y + 1); взятиКлітинуСвіту (x - 1) y; взятиКлітинуСвіту (x + 1) y ]
                                        |> List.choose id
                    state @ [ant, cell, visibleCells, взятиГніздоМурахи ant]
        Map.fold worldFold [] світ

    let взятиДіїМурах (поведінкаЧорних: ІПоведінкаМурахи) (поведінкаЧервоних: ІПоведінкаМурахи) (views: (Мураха * КлітинаСвіту * КлітинаСвіту list * Гніздо) list) =
        let взятиПоведінкуМурахи мураха =
            match мураха.Колір with
            | КолірМурахи.Чорний -> поведінкаЧорних
            | КолірМурахи.Червоний -> поведінкаЧервоних
        let transformView (мураха, клітина, antView, гніздо) =
            let поведінка = взятиПоведінкуМурахи мураха in
            клітина, поведінка.Поведінка мураха клітина antView гніздо
        List.map transformView views

    let побудуватиТранзакцію (очікуваніКлітини: КлітинаСвіту list) дії =
        let предікат (світ: Світ) =
            List.forall (fun (cell: КлітинаСвіту) -> (Map.find cell.Ід світ) = cell) очікуваніКлітини
        let дія (iworld: Світ) =
            List.fold (fun (cworld: Світ) (ід, дія) -> Map.add ід (дія cworld.[ід]) cworld) iworld дії
        предікат, дія

    let getWorldChangeTransactions дії =
        seq { for source, дія in дії do
                let мураха = Option.get source.Мураха
                match дія with
                | Нічого -> ()
                | Перемістити (ціль) ->
                    if Option.isSome ціль.Мураха then ()
                    else yield побудуватиТранзакцію
                                    [ source; ціль ]
                                    [ source.Ід, (fun стараклітина -> { стараклітина with Мураха = None });
                                        ціль.Ід, (fun oldtarget -> { oldtarget with Мураха = source.Мураха }) ]
                | ВзятиЇжу (ціль) ->
                    if ціль.Їжа <= 0 then ()
                    else
                        let foodToGet = min (ціль.Їжа) (максЇжіМурахаМожеНести - мураха.FoodCarried)
                        yield побудуватиТранзакцію
                                    [ source; ціль ]
                                    [ ціль.Ід, (fun oldtarget -> { oldtarget with Їжа = oldtarget.Їжа - foodToGet });
                                        source.Ід, (fun oldcell -> { oldcell with Мураха = Some { мураха with FoodCarried = мураха.FoodCarried + foodToGet } } ) ]
                | КинутиЇжу (ціль) ->
                    if ціль.Їжа >= maxTotalFoodPerSquare then ()
                    else
                        let foodToDrop = min (maxTotalFoodPerSquare - ціль.Їжа) (мураха.FoodCarried)
                        let transaction =
                            побудуватиТранзакцію
                                    [ source; ціль ]
                                    [ ціль.Ід, (fun oldtarget -> { oldtarget with Їжа = oldtarget.Їжа + foodToDrop });
                                        source.Ід, (fun oldcell -> { source with Мураха = Some { мураха with FoodCarried = мураха.FoodCarried - foodToDrop } }) ]
                        yield transaction
                | КинутиФеромон (ціль, кількість) ->
                    let новеЗначення = max (ціль.Феромони.[мураха.Колір] + кількість) maxCellPheromoneQuantity
                    yield побудуватиТранзакцію
                                [ ціль ]
                                [ ціль.Ід, (fun oldtarget -> { oldtarget with Феромони = oldtarget.Феромони.Add(мураха.Колір, новеЗначення ) } ) ] }

    let деградуватиФеромони (світ: Світ) =
        світ
        |> Map.map (fun уід клітина -> { клітина with Феромони = клітина.Феромони |> Map.map (fun ключ кількість -> max (кількість - 1) 0) } )

    let applyWorldTransactions (старийСвіт: Світ) зміни =
        let foldAction (світ: Світ) (пред, дія) =
            if пред світ
            then дія світ
            else світ
        Seq.fold foldAction старийСвіт зміни

    let уід2ху (уід: УІД) = уід.X, уід.Y

    let worldCycle bPlayer rPlayer світ : Світ =
        світ
        |> getAntViews
        |> взятиДіїМурах bPlayer rPlayer
        |> Seq.randomPermute
        |> getWorldChangeTransactions
        |> applyWorldTransactions світ
        |> деградуватиФеромони

module Полотно =

    // Get the canvas context for drawing
    let полотно = document.getElementsByTagName("canvas").[0] :?> HTMLCanvasElement
    let контекст = полотно.getContext_2d()

    // Format RGB color as "rgb(r,g,b)"
    let ($) s n = s + n.ToString()
    let rgb r g b = "rgb(" $ r $ "," $ g $ "," $ b $ ")"

    // Fill rectangle with given color
    let filled (колір: string) rect =
        let кнт = контекст
        кнт.fillStyle <- !^ колір
        кнт.fillRect rect

    let намалюватиПляму (колір: string) розмір (x, y) =
        контекст.beginPath()
        контекст.arc(x, y, розмір, 0., 2. * System.Math.PI, false )
        контекст.fillStyle <- !^ колір
        контекст.fill()

    let взятиРозміриВікна () =
        полотно.width, полотно.height


    let зображення (src:string) =
        let зображення = document.getElementsByTagName("img").[0] :?> HTMLImageElement
        if зображення.src.IndexOf(src) = -1 then зображення.src <- src
        зображення

    let updateInput ім'я текст =
        let image = document.getElementsByName(ім'я).[0] :?> HTMLDivElement
        image.innerHTML <- текст
        image


module Сімуляція =
    open Типи
    open Світ
    open Полотно

    let намалюватиМураху x y колірМурахи =
        let колір =
            match колірМурахи with
            | КолірМурахи.Чорний -> rgb 0 0 0
            | КолірМурахи.Червоний -> rgb 255 0 0
        намалюватиПляму колір 4. (x, y)

    let намалюватиЇжу їжа x y =
        let радіус = ((float їжа / float maxTotalFoodPerSquare) * 3.) + 1.
        let колір = rgb 0 255 0
        намалюватиПляму колір радіус (x, y)

    let зробитиГрадіент кількість max =
        let inverseGrediant = 1. - (float кількість / float max)
        let levelDiff = 200. - 111. // difference between the "full pheromone color and background"
        levelDiff * inverseGrediant
    let намалюватиФеромони x y колірМурахи amount =
        let opacityFudge = зробитиГрадіент amount maxCellPheromoneQuantity
        let рівень = int opacityFudge + 111
        // console.log(sprintf "level: %d" level)
        let колір =
            match колірМурахи with
            | КолірМурахи.Чорний -> rgb рівень рівень рівень
            | КолірМурахи.Червоний -> rgb рівень opacityFudge рівень
        намалюватиПляму колір 4. (x, y)

    let намалюватиОновлення (ширина, висота) (світ: Світ) =
        let оновитиКлітину уід клітина =
            let wm, hm = ширина / float (xSize + 1), висота / float (ySize + 1)
            let offset x y = (x + 0.5) * wm, (y + 0.5) * hm
            let x, y = уід2ху уід
            let ox, oy = offset (float x) (float y)
            клітина.Феромони |> Map.iter (fun колір кількість -> if кількість > 0 then намалюватиФеромони ox oy колір кількість)
            if клітина.Їжа > 0 then намалюватиЇжу клітина.Їжа ox oy
            if клітина.Мураха.IsSome then намалюватиМураху ox oy клітина.Мураха.Value.Колір
        світ
        |> Map.iter оновитиКлітину


module HardishAI =

    open Помічники
    open Типи

    let слч = System.Random(int System.DateTime.Now.Ticks)

    type ТестоваПоведінкаМурахи() =
        interface ІПоведінкаМурахи with
            member x.Ім'я = "Rick's Hardish"
            member x.Поведінка я тут місця гніздо =

                let цеМійДім вузел = вузел.ТипКлітини = ТипКлітиниСвіту.КлітинаГнізда(я.Колір)
                let locationsWithoutAnts = місця |> List.filter  (fun вузел -> вузел.Мураха = None)

                let (|МаєЇжу|МаєМаксимальноЇжу|НемаєЇжі|) (мураха: Мураха) =
                    if мураха.FoodCarried = 0 then НемаєЇжі
                    elif мураха.FoodCarried = максЇжіМурахаМожеНести then МаєМаксимальноЇжу
                    else МаєЇжу

                let (|NearHome|_|) (locations: КлітинаСвіту list) =
                    let домашніВузли = locations |> List.filter (fun node -> цеМійДім node)
                    if List.isEmpty домашніВузли then None
                    else Some домашніВузли

                let (|ВдаліВідДому|БіляДому|) (locations: КлітинаСвіту list) =
                    let homeLocations, awayLocations = locations |> List.partition (fun node -> цеМійДім node)
                    if List.isEmpty homeLocations then ВдаліВідДому awayLocations
                    else БіляДому homeLocations

                let (|CanDrop|CantDrop|) (місця: КлітинаСвіту list) =
                    let dropFoodLocations = місця |> List.filter (fun node -> not (node.IsFullOfFood))
                    if List.isEmpty dropFoodLocations then CantDrop
                    else CanDrop dropFoodLocations

                let (|HasUnownedFood|_|) (місця: КлітинаСвіту list) =
                    let місцяЇжі = місця |> List.filter (fun node -> node.ЄЇжа && not (цеМійДім node))
                    if List.isEmpty місцяЇжі then None
                    else Some місцяЇжі

                let (|ЄФеромониІБезМурахи|_|) (місця: КлітинаСвіту list) =
                    let місцяФеромонів = місця |> List.filter (fun node -> node.Мураха = None) |> List.filter (fun node -> node.ЄФеромони я.Колір)
                    if List.isEmpty місцяФеромонів then None
                    else Some місцяФеромонів

                let (|БезМурахи|_|) (місця: КлітинаСвіту list) =
                    let пустьМісця = місця |> List.filter (fun node -> node.Мураха = None)
                    if List.length пустьМісця > 0 then
                        Some (пустьМісця)
                    else None

                let (|ShortestDistanceWithNoAnt|_|)  (locations: КлітинаСвіту list) =
                    let noAnts = locations |> List.filter (fun node -> node.Мураха = None)
                    if List.length noAnts > 0 then Some (noAnts |> List.minBy (fun node -> гніздо.Дістанція node))
                    else None

                let maxFood = List.maxBy (fun node -> node.Їжа)
                let minPhero = List.minBy (fun node -> node.Феромони.[я.Колір])
                let noAnts = List.filter (fun node -> node.Мураха = None)

                // [snippet:Simple Pheromone-Using Ant Colony AI]
                match я with
                | МаєЇжу
                | МаєМаксимальноЇжу ->
                    match місця with
                    | БіляДому homeCells ->
                        match homeCells with
                        | CanDrop dropCells -> КинутиЇжу dropCells.Head
                        | БезМурахи noAntCells -> Перемістити (List.random noAntCells)
                        | _ -> Нічого
                    | ВдаліВідДому allCells ->
                        match тут.Феромони.[я.Колір] with
                        | n when n < 20 -> КинутиФеромон (тут, 100 - n)
                        | _ ->
                            match allCells with
                            | БезМурахи noAnts when слч.Next(0, 3) = 0 -> Перемістити (List.random noAnts)
                            | ShortestDistanceWithNoAnt node -> Перемістити node
                            | _ -> Нічого
                | НемаєЇжі ->
                    match місця with
                    | БезМурахи noAnts when слч.Next(0, 3) = 0 -> Перемістити (List.random noAnts)
                    | HasUnownedFood foodCells -> ВзятиЇжу (maxFood foodCells)
                    | ЄФеромониІБезМурахи pheroCells -> Перемістити (minPhero pheroCells)
                    | БезМурахи noAntCells -> Перемістити (List.random noAntCells)
                    | _ -> Нічого


module AntsEverywhereExmampleAI =
    open Типи

    let randomGen = new System.Random()

    let getRandomVal min max =
        lock randomGen (fun () -> randomGen.Next(min, max))

    type ТестоваПоведінкаМурахи() =
        interface ІПоведінкаМурахи with
            member x.Ім'я = "Frank_Levine"
            member x.Поведінка я тут місця гніздо =

                // This Ant's basic strategy is this:
                // If you have food and are near the nest
                //      drop the food
                // If you can't carry anymore food (bur are not near the nest)
                //      head back to the nest with the following exception
                //          if the current cell (here) has <40 phereomones, replenish the supply back to 100
                // If you're not dropping off food or heading home, you're foraging
                //      The logic for foraging is:
                //      If you see food, take it (this applies even when you have food but aren't full)
                //      If you see pheromones, move to the pheromone that is farthest from the nest
                //          if all pheromones are closer to the nest than you, then make a random move
                //      Otherwise you'e in the middle of nowhere, wanter randomly
                //
                // Special note on 'Traffic Control':  Inbound ants always yield to outbound ants
                //                                     This seems reasonable since the inbound ants
                //                                     Know where they're going and the outbound ones
                //                                     Are dependent on the pheromone trail



                //
                // вспоміжні функції
                let єКлітиною (cell: КлітинаСвіту) = cell.ТипКлітини = ТипКлітиниСвіту.КлітинаГнізда(я.Колір)

                // how do I negate a function?!?  this seems a bit heavy-handed
                let неЄКлітиною (cell: КлітинаСвіту) =
                    if єКлітиною cell then
                        false
                    else
                        true

                // nest cells that can receive food
                let nestCells = місця |> List.filter єКлітиною
                                        |> List.filter (fun c -> c.IsFullOfFood = false)

                // all empty neighbors, sorted so we can get at the closest and farthest ones from the nest
                // first = closest to nest
                // last = farthest from nest
                let emptyNeighbors = місця |> List.filter (fun c -> c.МіститьМураху = false)
                                            |> List.sortBy (fun c -> гніздо.Дістанція(c))

                // all empty neighbors with my pheromones
                let emptyNeighborsWithP = emptyNeighbors |> List.filter( fun c -> c.ЄФеромони(я.Колір))
                                                        |> List.sortBy( fun c -> гніздо.Дістанція(c))
                                                        |> List.toArray

                // all neighbors with food, ordered by the amount of food decending
                let neighborsWithFood = місця |> List.filter (неЄКлітиною)
                                                |> List.filter (fun c -> c.ЄЇжа)
                                                |> List.sortBy (fun c -> c.Їжа)
                                                |> List.rev

                // functions to make the code below more readable
                // NullMove does nothing (like when you're boxed in)
                // RandomMove is... Random
                let NullMove = fun() -> Перемістити тут

                let RandomMove = fun () ->
                    let i = getRandomVal 0 emptyNeighbors.Length
                    Перемістити (List.item i emptyNeighbors)


                // maximum amount of pheromone to leave on a cell
                let МАКС_ФЕРО = 100;

                // when returning to the nest, add more pheromones when the cell
                // has less than this number
                let REFRESH_THRESHOLD = 50;



                // active pattern to determine the ant's high-level state
                let (|ShouldDropFood|Forage|ReturnToNest|) (ant: Мураха) =
                    let haveAvailableNestCells = (nestCells.IsEmpty = false)
                    match ant with
                        | a when a.HasFood && haveAvailableNestCells -> ShouldDropFood
                        | a when a.IsFullOfFood -> ReturnToNest
                        | _ -> Forage

                // active pattern to decide if we need to refresh pheromones
                let (|NeedsRefresh|NoRefresh|) (cell: КлітинаСвіту) =
                    match cell.Феромони.[я.Колір] with
                        | x when x < REFRESH_THRESHOLD ->
                            let amt = МАКС_ФЕРО - x     // amt is the number of pheromones required to bring this cell back to 100
                            NeedsRefresh amt
                        | _ -> NoRefresh    // there are enough for now

                // gets the relative distance to the nest
                // relativeDist > 0 --> cell is farther from the nest than 'here'
                // relativeDist < 0 --> cell is closer to the nest than 'here'
                let relativeDist (cell: КлітинаСвіту) =
                    let dHere = гніздо.Дістанція(тут)
                    let dCell = гніздо.Дістанція(cell)
                    dCell - dHere

                // function to get the last thing from an array
                let останній (arr: 'a[]) =
                    arr.[arr.Length-1]

                // the ant parameter isn't used, but I don't know how to make a
                // parameterless active pattern
                let (|AdjacentToFood|AdjacentToPheromone|NoMansLand|) (ant: Мураха) =
                    if neighborsWithFood.Length > 0 then
                        AdjacentToFood
                    elif emptyNeighborsWithP.Length > 0 && relativeDist (останній emptyNeighborsWithP) > 0. then
                        // remember emptyNeighborsWithP is sorted
                        AdjacentToPheromone (останній emptyNeighborsWithP)
                    else
                        NoMansLand

                // The Actual logic...

                if emptyNeighbors.IsEmpty then
                    NullMove()
                else
                    match я with
                    | ShouldDropFood -> КинутиЇжу nestCells.Head
                    | ReturnToNest ->
                        match тут with
                        | NeedsRefresh amt -> КинутиФеромон (тут, amt)
                        | NoRefresh -> Перемістити emptyNeighbors.Head
                    | Forage ->
                        match я with
                        | AdjacentToFood -> ВзятиЇжу neighborsWithFood.Head
                        | AdjacentToPheromone pheroCell -> Перемістити pheroCell
                        | NoMansLand -> RandomMove()

open Полотно
open Типи
open Світ
open Сімуляція

let origin =
    // Sample is running in an iframe, so get the location of parent
    let topLocation = window.top.location
    topLocation.origin + topLocation.pathname

let форматуватиКарткуРахунку ім'яЧорного їжаЧорного ім'яЧервоного їжаЧервоного =
    sprintf "Чорний (%s): %05d проти Червоного (%s): %05d" ім'яЧорного їжаЧорного ім'яЧервоного їжаЧервоного

let форматуватиЗалишок залишок =
    sprintf "Залишилося циклів: %05d" залишок


let maxCycles = 1000
let світ = ref (buildWorldInitialWorld())
let foodToWin = int <| double (Map.fold (fun s k v -> s + v.Їжа) 0 світ.Value) * percentFoodToWin
let цикли = ref 0

let чорнийШІ = new HardishAI.ТестоваПоведінкаМурахи() :> ІПоведінкаМурахи
let червонийШІ = new AntsEverywhereExmampleAI.ТестоваПоведінкаМурахи() :> ІПоведінкаМурахи

let render (w,h) =
    цикли.Value <- цикли.Value + 1

    let рахунокЧорних = ГніздоЧорнихМурах.КількістьЇжі світ.Value
    let рахунокЧервоних = ГніздоЧервонихМурах.КількістьЇжі світ.Value

    let залишок = maxCycles - цикли.Value

    let рядокРахунка = форматуватиКарткуРахунку чорнийШІ.Ім'я рахунокЧорних червонийШІ.Ім'я рахунокЧервоних
    updateInput "score" рядокРахунка |> ignore

    let рядокЗалишку = форматуватиЗалишок залишок
    updateInput "secondline" рядокЗалишку |> ignore


    (0., 0., w, h) |> filled (rgb 200 200 200)
    намалюватиОновлення (w,h) світ.Value
    світ.Value <- worldCycle чорнийШІ червонийШІ світ.Value

    if рахунокЧорних > foodToWin || рахунокЧервоних > foodToWin || цикли.Value > maxCycles then
        if рахунокЧорних > рахунокЧервоних then Some чорнийШІ.Ім'я
        elif рахунокЧервоних > рахунокЧорних then Some червонийШІ.Ім'я
        else None
    else None

let w, h = взятиРозміриВікна()

let rec оновити () =
    let результат = render (w,h)
    match результат with
    | None ->
        window.setTimeout(оновити, 1000 / 30) |> ignore
    | Some переможець ->
        updateInput "secondline" (sprintf "Переможець: %s" переможець) |> ignore

оновити ()
