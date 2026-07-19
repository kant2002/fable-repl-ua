модуль Mario

відкрити Fable.Core
відкрити Fable.Core.JsInterop
відкрити Browser.Types
відкрити Browser

модуль Keyboard =

    нехай змінливий keysPressed = Set.empty

    /// Returns 1 якщо key із given code is pressed
    нехай code x =
        якщо keysPressed.Contains(x) тоді 1 інакше 0

    /// Update the state з the set для given key event
    нехай update (e : KeyboardEvent, pressed) =
        нехай key = e.key
        нехай op =  якщо pressed тоді Set.add інакше Set.remove
        keysPressed <- op key keysPressed

    /// Returns pair із -1 для left or down and +1
    /// для right or up (0 якщо no or both keys are pressed)
    нехай arrows () =
        (code "ArrowRight" - code "ArrowLeft", code "ArrowUp" - code "ArrowDown")

    нехай initKeyboard () =
        document.addEventListener("keydown", фун e -> update(e :?> _, true))
        document.addEventListener("keyup", фун e -> update(e :?> _, false))

модуль Physics =

    тип MarioModel =
        { x:float; y:float;
          vx:float; vy:float;
          dir:string }


    // If the Up key is pressed (y > 0) and Mario is on the ground,
    // тоді create Mario із the y velocity 'vy' set to 5.0
    нехай jump (_,y) m =
        якщо y > 0 && m.y = 0. тоді { m із vy = 5. } інакше m

    // If Mario is у the air, тоді his "up" velocity is decreasing
    нехай gravity m =
        якщо m.y > 0. тоді { m із vy = m.vy - 0.1 } інакше m

    // Apply physics - move Mario according to the current velocities
    нехай physics m =
        { m із x = m.x + m.vx; y = max 0. (m.y + m.vy) }

    // When Left/Right keys are pressed, change 'vx' and direction
    нехай walk (x,_) m =
        нехай dir = якщо x < 0 тоді "left" інякщо x > 0 тоді "right" інакше m.dir
        { m із vx = float x; dir = dir }


    нехай marioStep dir mario =
        mario
        |> physics
        |> walk dir
        |> gravity
        |> jump dir

модуль Canvas =

    // Get the canvas context для drawing
    нехай canvas = document.getElementsByTagName("canvas").[0] :?> HTMLCanvasElement
    нехай context = canvas.getContext_2d()

    // Format RGB color as "rgb(r,g,b)"
    нехай ($) s n = s + n.ToString()
    нехай rgb r g b = "rgb(" $ r $ "," $ g $ "," $ b $ ")"

    /// Fill rectangle із given color
    нехай filled (color: string) rect =
        нехай ctx = context
        ctx.fillStyle <- !^ color
        ctx.fillRect rect

    /// Move element to a specified X Y position
    нехай position (x,y) (img : HTMLImageElement) =
        img?style?left <- x.ToString() + "px"
        img?style?top <- (canvas.offsetTop + y).ToString() + "px"

    нехай getWindowDimensions () =
        canvas.width, canvas.height

    /// Get the first <img /> element and set `src` (зробити
    /// nothing якщо it is the right one to keep animation)
    нехай image (src:string) =
        нехай image = document.getElementsByTagName("img").[0] :?> HTMLImageElement
        якщо image.src.IndexOf(src) = -1 тоді image.src <- src
        image

відкрити Canvas
відкрити Physics

нехай origin =
    // Sample is running у an iframe, so get the location з parent
    нехай topLocation = window.top.location
    topLocation.origin + topLocation.pathname

нехай render (w,h) (mario: MarioModel) =
    (0., 0., w, h) |> filled (rgb 174 238 238)
    (0., h-50., w, 50.) |> filled (rgb 74 163 41)
    // Select and position Mario
    // (walking is represented as an animated gif)
    нехай verb =
        якщо mario.y > 0. тоді "jump"
        інякщо mario.vx <> 0. тоді "walk"
        інакше "stand"
    origin + "img/mario/mario" + verb + mario.dir + ".gif"
    |> image
    |> position (w/2.-16.+mario.x,  h-50.-31.-mario.y)

Keyboard.initKeyboard()

нехай w, h = getWindowDimensions()

нехай rec update mario () =
    нехай mario = mario |> Physics.marioStep (Keyboard.arrows())
    render (w,h) mario
    window.setTimeout(update mario, 1000 / 60) |> ignore

нехай mario = { x=0.; y=0.; vx=0.; vy=0.; dir="right" }
update mario ()
