модуль Mandelbrot

// You can draw a rectangle to zoom у an area (feature added by Avi Avni)

відкрити Fable.Core
відкрити Fable.Core.JsInterop
відкрити Browser.Types
відкрити Browser

тип Complex = { r : double; i : double }
тип Color = { r : uint8; g : uint8; b : uint8; a : uint8 }

нехай maxIter = 255

нехай height = 1024
нехай width = 1024

нехай змінливий minX = -2.0
нехай змінливий maxX = 2.0
нехай змінливий minY = -1.5
нехай змінливий maxY = 3.5
нехай змінливий rectX = 0.0
нехай змінливий rectY = 0.0
нехай змінливий rectW = 0.0
нехай змінливий rectH = 0.0

нехай iteratePoint (s : Complex) (p : Complex) : Complex =
    { r = s.r + p.r*p.r - p.i*p.i; i = s.i + 2.0 * p.i * p.r }

нехай getIterationCount (p : Complex) =
    нехай змінливий z = p
    нехай змінливий i = 0
    while i < maxIter && (z.r*z.r + z.i*z.i < 4.0) зробити
      z <- iteratePoint p z
      i <- i + 1
    i

нехай getCoord (x : int, y : int) : Complex =
    нехай p = { r = float x * (maxX - minX) / float width + minX
            ; i = float y * (maxY - minY) / float height + minY }
    p

нехай getCoordColor (x : int, y : int) : Color =
    нехай p = getCoord (x, y)
    нехай i = getIterationCount p
    { r = uint8 (255/(i%5)); g = uint8 (255/(i%3)); b = uint8 (255/(i%7)); a = 255uy }

нехай showSet() =
    нехай canvas = document.getElementsByTagName("canvas").[0] :?> HTMLCanvasElement
    нехай ctx = canvas.getContext_2d()

    нехай img = ctx.createImageData(float width, float height)
    для y = 0 to height-1 зробити
        для x = 0 to width-1 зробити
            нехай index = (x + y * width) * 4
            нехай color = getCoordColor (x, y)
            img.data.[index+0] <- color.r
            img.data.[index+1] <- color.g
            img.data.[index+2] <- color.b
            img.data.[index+3] <- color.a
    ctx.putImageData(img, 0., 0.)

    ctx.fillStyle <- !^"rgba(200,0,0,0.5)"
    ctx.fillRect (rectX, rectY, rectW, rectH)


document.addEventListener("mousedown", фун de ->
    нехай de = de :?> MouseEvent
    rectX <- de.clientX
    rectY <- de.clientY
    rectW <- 0.0
    rectH <- 0.0
    showSet())

document.addEventListener("mousemove", фун de ->
    нехай de = de :?> MouseEvent
    якщо de.buttons = 1.0 тоді
        rectW <- de.clientX - rectX
        rectH <- de.clientY - rectY
        showSet())

document.addEventListener("mouseup", фун de ->
    нехай de = de :?> MouseEvent
    нехай p1 = getCoord (int rectX, int rectY)
    нехай p2 = getCoord (int (rectX + rectW), int (rectY + rectH))
    minX <- min p1.r p2.r
    maxX <- max p1.r p2.r
    minY <- min p1.i p2.i
    maxY <- max p1.i p2.i
    rectX <- 0.0
    rectY <- 0.0
    rectW <- 0.0
    rectH <- 0.0
    showSet())

showSet()
