module Ozmo

// Phil Trelford's classic Ozmo game ported to Fable!
// Shows how to handle keyboard events and use HTML5 canvas.
// You can also get it (as a JavaScript app) from the Windows Store.

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Browser

module Клавіатура =

    let mutable натиснутіКнопки = Set.empty

    let код x = if натиснутіКнопки.Contains(x) then 1 else 0

    let стрілки () =
        (код "ArrowRight" - код "ArrowLeft", код "ArrowUp" - код "ArrowDown")

    let оновити (e : KeyboardEvent, натиснута) =
        let кнопка = e.key
        let оп = if натиснута then Set.add else Set.remove
        натиснутіКнопки <- оп кнопка натиснутіКнопки

    let ініц () =
        window.addEventListener("keydown", fun e -> оновити(e :?> _, true))
        window.addEventListener("keyup", fun e -> оновити(e :?> _, false))

// Main

/// Scale to make it fit in a 1920*1080 screen
let масштаб = 0.8

/// The width of the canvas
let ширина = 900. * масштаб
/// The height of the canvas
let висота = 668. * масштаб
/// Висота полу - нижня чорна частина
let висотаПолу = 100. * масштаб
/// Висота атмосфери - жовтий градієнт
let висотаАтмос = 300. * масштаб

Клавіатура.ініц()

let полотно = document.getElementsByTagName("canvas").[0] :?> HTMLCanvasElement
let кткст = полотно.getContext_2d()
полотно.width <- ширина
полотно.height <- висота

/// Намалювати градієнт між двома У здвигами та двома кольорами
let намалюватиСтк (кткст:CanvasRenderingContext2D)
    (полотно:HTMLCanvasElement) (y0,y1) (c0,c1) =
    let стк = кткст.createLinearGradient(0.,y0,0.,y1)
    стк.addColorStop(0.,c0)
    стк.addColorStop(1.,c1)
    кткст.fillStyle <- !^ стк
    кткст.fillRect(0.,y0, полотно.width, y1- y0)


/// Draw background of the Ozmo game
let намалюватиФн кткст полотно =
    намалюватиСтк кткст полотно
        (0.,висотаАтмос) ("yellow","orange")
    намалюватиСтк кткст полотно
        (висотаАтмос, полотно.height-висотаПолу)
        ("grey","white")
    кткст.fillStyle <- !^ "black"
    кткст.fillRect
        ( 0.,полотно.height-висотаПолу,
          полотно.width,висотаПолу )

/// Draw the specified text (when game finishes)
let намалюватиТекст(текст,x,y) =
    кткст.fillStyle <- !^ "white"
    кткст.font <- "bold 40pt";
    кткст.fillText(текст, x, y)


type Крапля =
    { X:float; Y:float;
      vx:float; vy:float;
      Радіус:float; кольор:string }

let намалюватиКраплю (кткст:CanvasRenderingContext2D)
    (полотно:HTMLCanvasElement) (крапля:Крапля) =
    кткст.beginPath()
    кткст.arc
        ( крапля.X, полотно.height - (крапля.Y + висотаПолу + крапля.Радіус),
          крапля.Радіус, 0., 2. * System.Math.PI, false )
    кткст.fillStyle <- !^ крапля.кольор
    кткст.fill()
    кткст.lineWidth <- 3.
    кткст.strokeStyle <- !^ крапля.кольор
    кткст.stroke()


/// Apply key effects on Player's blob - changes X speed
let напрям (dx,dy) (крапля:Крапля) =
    { крапля with vx = крапля.vx + (float dx)/4.0 }

/// Apply gravity on falling blobs - gets faster every step
let гравітація (крапля:Крапля) =
    if крапля.Y > 0. then { крапля with vy = крапля.vy - 0.1 }
    else крапля

/// Bounde Player's blob off the wall if it hits it
let bounce (крапля:Крапля) =
    let n = ширина
    if крапля.X < 0. then
        { крапля with X = -крапля.X; vx = -крапля.vx }
    elif (крапля.X > n) then
        { крапля with X = n - (крапля.X - n); vx = -крапля.vx }
    else крапля


/// Move blob by one step - adds X and Y
/// velocities to the X and Y coordinates
let перемістити (крапля:Крапля) =
    { крапля with
        X = крапля.X + крапля.vx
        Y = max 0.0 (крапля.Y + крапля.vy) }

/// Apply step on Player's blob. Composes above functions.
let крок напр крапля =
    крапля |> напрям напр |> перемістити |> bounce

/// Перевіряє чи дві краплі зтикаються
let зіткнення (a:Крапля) (b:Крапля) =
    let dx = (a.X - b.X)*(a.X - b.X)
    let dy = (a.Y - b.Y)*(a.Y - b.Y)
    let діст = sqrt(dx + dy)
    діст < abs(a.Радіус - b.Радіус)

/// Видаляє усі падаючи краплі які зіткнулися із краплею гравця
let absorb (крапля:Крапля) (краплі:Крапля list) =
    краплі
    |> List.filter (fun drop ->
        зіткнення крапля drop |> not )


// Game helpers
// =============

let ріст = "black"
let зменшення = "white"

let новаКрапля колір =
    { X = JS.Math.random()*ширина*0.8 + (ширина*0.1)
      Y=600.; Радіус=10.; vx=0.; vy = 0.0
      кольор=колір }

let новийРіст () = новаКрапля ріст
let новеЗменшення () = новаКрапля зменшення

/// Update drops and countdown in each step
let updateDrops краплі countdown =
    if countdown > 0 then
        краплі, countdown - 1
    elif floor(JS.Math.random()*8.) = 0. then
        let крапля =
            if floor(JS.Math.random()*3.) = 0. then новийРіст()
            else новеЗменшення()
        крапля::краплі, 8
    else краплі, countdown


/// Count growing and shrinking drops in the list
let підрахуватиКраплі краплі =
    let кількість колір =
        краплі
        |> List.filter (fun крапля -> крапля.кольор = колір)
        |> List.length
    кількість ріст, кількість зменшення

// Asynchronous game loop
// ========================

let rec гра () = async {
    let крапля =
        { X = 300.; Y=0.; Радіус=50.;
          vx=0.; vy=0.; кольор="black" }
    return! оновити крапля [новийРіст ()] 0 }

and закінчилася () = async {
    намалюватиТекст ("COMPLETED",320.,300.)
    do! Async.Sleep 10000
    return! гра () }

/// Keeps current state for Player's blob, falling
/// drops and the countdown since last drop was generated
and оновити калюжа краплі countdown = async {
    // Update the drops & countdown
    let краплі, countdown = updateDrops краплі countdown

    // Count drops, apply physics and count them again
    let доРосту, доЗменшування = підрахуватиКраплі краплі
    let краплі =
        краплі
        |> List.map (гравітація >> перемістити)
        |> absorb калюжа
    let післяРосту, післяЗменшення = підрахуватиКраплі краплі
    let краплі = краплі |> List.filter (fun blob -> blob.Y > 0.)

    // Calculate new player's size based on absorbed drops
    let радіус = калюжа.Радіус + float (доРосту - післяРосту) *4.
    let радіус = радіус - float (доЗменшування - післяЗменшення) * 4.
    let радіус = max 5.0 радіус

    // Update radius and apply keyboard events
    let калюжа = { калюжа with Радіус = радіус }
    let калюжа = калюжа |> крок (Клавіатура.стрілки())

    // Render the new game state
    намалюватиФн кткст полотно
    for крапля in краплі do намалюватиКраплю кткст полотно крапля
    намалюватиКраплю кткст полотно калюжа

    // If the game completed, switch state
    // otherwise sleep and update recursively!
    if калюжа.Радіус > 150. then
        return! закінчилася()
    else
        do! Async.Sleep(int (1000. / 60.))
        return! оновити калюжа краплі countdown }

гра () |> Async.StartImmediate
