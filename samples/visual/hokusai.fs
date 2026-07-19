модуль Hokusai

(*
Hokusai and Julia: rendering fractals using HTML5 canvas
This demo is based on Tomas Petricek's F# Advent Calendar post that explores
Japanese art and renders The Great Wave by Hokusai using the Julia fractal.
*)

відкрити Fable.Core
відкрити Browser.Types
відкрити Browser

тип Complex =
  | Complex з float * float
  /// Calculate the absolute value з a complex number
  статичний член Abs(Complex(r, i)) =
    нехай num1, num2 = abs r, abs i
    якщо (num1 > num2) тоді
      нехай num3 = num2 / num1
      num1 * sqrt(1.0 + num3 * num3)
    інякщо num2 = 0.0 тоді
      num1
    інакше
      нехай num4 = num1 / num2
      num2 * sqrt(1.0 + num4 * num4)
  /// Add real and imaginary components pointwise
  статичний член (+) (Complex(r1, i1), Complex(r2, i2)) =
    Complex(r1+r2, i1+i2)

модуль Complexмодуль =
  /// Calculates nth power з a complex number
  нехай Pow(Complex(r, i), power) =
    нехай num = Complex.Abs(Complex(r, i))
    нехай num2 = atan2 i r
    нехай num3 = power * num2
    нехай num4 = num ** power
    Complex(num4 * cos(num3), num4 * sin(num3))

/// Constant that generates nice fractal
нехай c = Complex(-0.70176, -0.3842)

/// Generates sequence для given coordinates
нехай iterate x y =
  нехай rec loop current = seq {
    поступатися current
    yield! loop (Complexмодуль.Pow(current, 2.0) + c) }
  loop (Complex(x, y))

нехай countIterations max x y =
  iterate x y
  |> Seq.take (max - 1)
  |> Seq.takeWhile (фун v -> Complex.Abs(v) < 2.0)
  |> Seq.length

// Transition between colors у 'count' steps
нехай (--) clr count = clr, count
нехай (-->) ((r1, g1, b1), count) (r2, g2, b2) = [
  для c у 0 .. count - 1 ->
    нехай k = c / count |> byte
    нехай mid v1 v2 =
      (v1 + (v2 - v1) * k)
    (mid r1 r2, mid g1 g2, mid b1 b2) ]

// Palette із colors used by Hokusai
нехай palette =
  [| // 3x sky color & transition to light blue
     yield! (245uy, 219uy, 184uy) --3--> (245uy, 219uy, 184uy)
     yield! (245uy, 219uy, 184uy) --4--> (138uy, 173uy, 179uy)
     // to dark blue and тоді medium dark blue
     yield! (138uy, 173uy, 179uy) --4--> (2uy, 12uy, 74uy)
     yield! (2uy, 12uy, 74uy)     --4--> (61uy, 102uy, 130uy)
     // to wave coloruy,  тоді light blue & back to wave
     yield! (61uy, 102uy, 130uy)  -- 8--> (249uy, 243uy, 221uy)
     yield! (249uy, 243uy, 221uy) --32--> (138uy, 173uy, 179uy)
     yield! (138uy, 173uy, 179uy) --32--> (61uy, 102uy, 130uy)
  |]

// Specifies what range з the set to draw
нехай w = -0.4, 0.4
нехай h = -0.95, -0.35

// Create bitmap that matches the size з the canvas
нехай width = 400.0
нехай height = 300.0


/// Set pixel value у ImageData to a given color
нехай setPixel (img:ImageData) x y width (r, g, b) =
  нехай index = (x + y * int width) * 4
  img.data.[index+0] <- r
  img.data.[index+1] <- g
  img.data.[index+2] <- b
  img.data.[index+3] <- 255uy

/// Dynamic operator that returns HTML element by ID
нехай (?) (doc:Document) name :'R =
  doc.getElementById(name) :?> 'R

/// Render fractal asynchronously із sleep after every line
нехай render () = async {
  // Get <canvas> element & create image для drawing
  нехай canv : HTMLCanvasElement = document?canvas
  нехай ctx = canv.getContext_2d()
  нехай img = ctx.createImageData(float width, float height)

  // For each pixel, transform to the specified range
  // and get color using countInterations and palette
  для x у 0 .. int width - 1 зробити
    для y у 0 .. int height - 1 зробити
      нехай x' = (float x / width * (snd w - fst w)) + fst w
      нехай y' = (float y / height * (snd h - fst h)) + fst h
      нехай it = countIterations palette.Length x' y'
      setPixel img x y width (palette.[it])

    // Insert non-blocking waiting & update the fractal
    do! Async.Sleep(1)
    ctx.putImageData(img, 0.0, 0.0) }

/// Setup button event handler to start the rendering
нехай go : HTMLButtonElement = document?go
go.addEventListener("click", фун _ ->
  render() |> Async.StartImmediate)
