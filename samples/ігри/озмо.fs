модуль Ozmo

// Phil Trelford's classic Ozmo game ported to Fable!
// Shows how to handle keyboard events and use HTML5 canvas.
// You can also get it (as a JavaScript app) from the Windows Store.

відкрити Fable.Core
відкрити Fable.Core.JsInterop
відкрити Browser.Types
відкрити Browser

модуль Клавіатура =

    нехай змінливий натиснутіКнопки = Set.empty

    нехай код x = якщо натиснутіКнопки.Contains(x) тоді 1 інакше 0

    нехай стрілки () =
        (код "ArrowRight" - код "ArrowLeft", код "ArrowUp" - код "ArrowDown")

    нехай оновити (e : KeyboardEvent, натиснута) =
        нехай кнопка = e.key
        нехай оп = якщо натиснута тоді Set.add інакше Set.remove
        натиснутіКнопки <- оп кнопка натиснутіКнопки

    нехай ініц () =
        window.addEventListener("keydown", фун e -> оновити(e :?> _, true))
        window.addEventListener("keyup", фун e -> оновити(e :?> _, false))

// Main

/// Scale to make it fit у a 1920*1080 screen
нехай масштаб = 0.8

/// The width з the canvas
нехай ширина = 900. * масштаб
/// The height з the canvas
нехай висота = 668. * масштаб
/// Висота полу - нижня чорна частина
нехай висотаПолу = 100. * масштаб
/// Висота атмосфери - жовтий градієнт
нехай висотаАтмос = 300. * масштаб

Клавіатура.ініц()

нехай полотно = document.getElementsByTagName("canvas").[0] :?> HTMLCanvasElement
нехай кткст = полотно.getContext_2d()
полотно.width <- ширина
полотно.height <- висота

/// Намалювати градієнт між двома У здвигами та двома кольорами
нехай намалюватиСтк (кткст:CanvasRenderingContext2D)
    (полотно:HTMLCanvasElement) (y0,y1) (c0,c1) =
    нехай стк = кткст.createLinearGradient(0.,y0,0.,y1)
    стк.addColorStop(0.,c0)
    стк.addColorStop(1.,c1)
    кткст.fillStyle <- !^ стк
    кткст.fillRect(0.,y0, полотно.width, y1- y0)


/// Draw background з the Ozmo game
нехай намалюватиФн кткст полотно =
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
нехай намалюватиТекст(текст,x,y) =
    кткст.fillStyle <- !^ "white"
    кткст.font <- "bold 40pt";
    кткст.fillText(текст, x, y)


тип Крапля =
    { X:float; Y:float;
      vx:float; vy:float;
      Радіус:float; кольор:string }

нехай намалюватиКраплю (кткст:CanvasRenderingContext2D)
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
нехай напрям (dx,dy) (крапля:Крапля) =
    { крапля із vx = крапля.vx + (float dx)/4.0 }

/// Apply gravity on falling blobs - gets faster every step
нехай гравітація (крапля:Крапля) =
    якщо крапля.Y > 0. тоді { крапля із vy = крапля.vy - 0.1 }
    інакше крапля

/// Bounde Player's blob off the wall якщо it hits it
нехай bounce (крапля:Крапля) =
    нехай n = ширина
    якщо крапля.X < 0. тоді
        { крапля із X = -крапля.X; vx = -крапля.vx }
    інякщо (крапля.X > n) тоді
        { крапля із X = n - (крапля.X - n); vx = -крапля.vx }
    інакше крапля


/// Move blob by one step - adds X and Y
/// velocities to the X and Y coordinates
нехай перемістити (крапля:Крапля) =
    { крапля із
        X = крапля.X + крапля.vx
        Y = max 0.0 (крапля.Y + крапля.vy) }

/// Apply step on Player's blob. Composes above functions.
нехай крок напр крапля =
    крапля |> напрям напр |> перемістити |> bounce

/// Перевіряє чи дві краплі зтикаються
нехай зіткнення (a:Крапля) (b:Крапля) =
    нехай dx = (a.X - b.X)*(a.X - b.X)
    нехай dy = (a.Y - b.Y)*(a.Y - b.Y)
    нехай діст = sqrt(dx + dy)
    діст < abs(a.Радіус - b.Радіус)

/// Видаляє усі падаючи краплі які зіткнулися із краплею гравця
нехай absorb (крапля:Крапля) (краплі:Крапля list) =
    краплі
    |> List.filter (фун drop ->
        зіткнення крапля drop |> not )


// Game helpers
// =============

нехай ріст = "black"
нехай зменшення = "white"

нехай новаКрапля колір =
    { X = JS.Math.random()*ширина*0.8 + (ширина*0.1)
      Y=600.; Радіус=10.; vx=0.; vy = 0.0
      кольор=колір }

нехай новийРіст () = новаКрапля ріст
нехай новеЗменшення () = новаКрапля зменшення

/// Update drops and countdown у each step
нехай updateDrops краплі countdown =
    якщо countdown > 0 тоді
        краплі, countdown - 1
    інякщо floor(JS.Math.random()*8.) = 0. тоді
        нехай крапля =
            якщо floor(JS.Math.random()*3.) = 0. тоді новийРіст()
            інакше новеЗменшення()
        крапля::краплі, 8
    інакше краплі, countdown


/// Count growing and shrinking drops у the list
нехай підрахуватиКраплі краплі =
    нехай кількість колір =
        краплі
        |> List.filter (фун крапля -> крапля.кольор = колір)
        |> List.length
    кількість ріст, кількість зменшення

// Asynchronous game loop
// ========================

нехай rec гра () = async {
    нехай крапля =
        { X = 300.; Y=0.; Радіус=50.;
          vx=0.; vy=0.; кольор="black" }
    return! оновити крапля [новийРіст ()] 0 }

and закінчилася () = async {
    намалюватиТекст ("COMPLETED",320.,300.)
    do! Async.Sleep 10000
    return! гра () }

/// Keeps current state для Player's blob, falling
/// drops and the countdown since last drop was generated
and оновити калюжа краплі countdown = async {
    // Update the drops & countdown
    нехай краплі, countdown = updateDrops краплі countdown

    // Count drops, apply physics and count them again
    нехай доРосту, доЗменшування = підрахуватиКраплі краплі
    нехай краплі =
        краплі
        |> List.map (гравітація >> перемістити)
        |> absorb калюжа
    нехай післяРосту, післяЗменшення = підрахуватиКраплі краплі
    нехай краплі = краплі |> List.filter (фун blob -> blob.Y > 0.)

    // Calculate новий player's size based on absorbed drops
    нехай радіус = калюжа.Радіус + float (доРосту - післяРосту) *4.
    нехай радіус = радіус - float (доЗменшування - післяЗменшення) * 4.
    нехай радіус = max 5.0 радіус

    // Update radius and apply keyboard events
    нехай калюжа = { калюжа із Радіус = радіус }
    нехай калюжа = калюжа |> крок (Клавіатура.стрілки())

    // Render the новий game state
    намалюватиФн кткст полотно
    для крапля у краплі зробити намалюватиКраплю кткст полотно крапля
    намалюватиКраплю кткст полотно калюжа

    // If the game completed, switch state
    // otherwise sleep and update recursively!
    якщо калюжа.Радіус > 150. тоді
        return! закінчилася()
    інакше
        do! Async.Sleep(int (1000. / 60.))
        return! оновити калюжа краплі countdown }

гра () |> Async.StartImmediate
