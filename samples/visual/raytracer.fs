// Source: http://www.tryfsharp.org/create/cpoulain/shared/raytracer.fsx
// slightly modified to avoid some allocations

модуль RayTracer

[<Struct>]
тип Vector =
    { X: float; Y: float; Z: float }
    статичний член (*) (k, v: Vector) = { X = k * v.X; Y = k * v.Y; Z = k * v.Z }
    статичний член (-) (v1: Vector, v2: Vector) = { X = v1.X - v2.X; Y = v1.Y - v2.Y; Z = v1.Z - v2.Z }
    статичний член (+) (v1: Vector, v2: Vector) = { X = v1.X + v2.X; Y = v1.Y + v2.Y; Z = v1.Z + v2.Z }
    статичний член Dot (v1: Vector, v2: Vector) = v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z
    статичний член Mag (v: Vector) = sqrt (v.X * v.X + v.Y * v.Y + v.Z * v.Z)
    статичний член Norm (v: Vector) =
        нехай mag = Vector.Mag v
        нехай div = якщо mag = 0.0 тоді infinity інакше 1.0/mag
        div * v
    статичний член Cross (v1: Vector, v2: Vector) =
        { X = v1.Y * v2.Z - v1.Z * v2.Y
        ; Y = v1.Z * v2.X - v1.X * v2.Z
        ; Z = v1.X * v2.Y - v1.Y * v2.X }

[<Struct>]
тип Color =
    { R: float; G: float; B: float }
    статичний член Scale (k, v: Color) = { R = k * v.R; G = k * v.G; B = k * v.B }
    статичний член (+) (v1: Color, v2: Color) = { R = v1.R + v2.R; G = v1.G + v2.G; B = v1.B + v2.B }
    статичний член (*) (v1: Color, v2: Color) = { R = v1.R * v2.R; G = v1.G * v2.G; B = v1.B * v2.B }
    статичний член White = { R = 1.0; G = 1.0; B = 1.0 }
    статичний член Grey = { R = 0.5; G = 0.5; B = 0.5 }
    статичний член Black = { R = 0.0; G = 0.0; B = 0.0 }
    статичний член Background = Color.Black
    статичний член DefaultColor = Color.Black

тип Camera (pos: Vector, lookAt: Vector) =
    нехай forward = Vector.Norm (lookAt - pos)
    нехай down = { X = 0.0; Y = -1.0; Z = 0.0 }
    нехай right = 1.5 * Vector.Norm (Vector.Cross (forward, down))
    нехай up = 1.5 * Vector.Norm (Vector.Cross (forward, right))
    член c.Pos     = pos
    член c.Forward = forward
    член c.Up      = up
    член c.Right   = right

[<Struct>]
тип Ray =
    { Start: Vector;
      Dir: Vector }

тип Surface =
    абстрактний Diffuse: Vector -> Color
    абстрактний Specular: Vector -> Color
    абстрактний Reflect: Vector -> float
    абстрактний Roughness : float

[<Struct>]
тип Intersection =
    { Thing: SceneObject;
      Ray: Ray;
      Dist: float }

and SceneObject =
    абстрактний Surface: Surface
    абстрактний Intersect: Ray -> float
    абстрактний Normal: Vector -> Vector

тип Light =
    { Pos : Vector;
      Color : Color }

тип Scene =
    { Things : SceneObject[];
      Lights : Light[];
      Camera : Camera }

модуль RayTracer =

    нехай maxDepth = 5

    нехай NearestIntersection ray scene =
        нехай змінливий acc = None
        для x у scene.Things зробити
            нехай dist = x.Intersect ray
            якщо acc.IsNone || dist < acc.Value.Dist тоді
                acc <- Some { Thing = x; Ray = ray; Dist = dist }
        acc

    нехай TestRay ray scene =
        співстав NearestIntersection ray scene із
        | None -> None
        | Some isect ->
            якщо isect.Dist = infinity
            тоді None
            інакше Some isect.Dist

    нехай rec TraceRay ray scene (depth: int) =
        співстав NearestIntersection ray scene із
        | None -> Color.Background
        | Some isect ->
            якщо isect.Dist = infinity
            тоді Color.Background
            інакше Shade isect scene depth

    and Shade isect scene depth =
        нехай d = isect.Ray.Dir
        нехай pos = isect.Dist * d + isect.Ray.Start
        нехай normal = isect.Thing.Normal (pos)
        нехай reflectDir = d - 2.0 * Vector.Dot (normal, d) * normal
        нехай naturalcolor = Color.DefaultColor + (GetNaturalColor isect.Thing pos normal reflectDir scene)
        нехай reflectedColor =
            якщо depth >= maxDepth тоді Color.Grey
            інакше GetReflectionColor (isect.Thing, pos + (0.001*reflectDir), normal, reflectDir, scene, depth)
        naturalcolor + reflectedColor

    and GetReflectionColor (thing: SceneObject, pos, normal: Vector, rd: Vector, scene: Scene, depth: int) =
        Color.Scale (thing.Surface.Reflect (pos), TraceRay { Start = pos; Dir = rd } scene (depth + 1))

    and GetNaturalColor thing pos normal rd scene =
        нехай змінливий color = Color.DefaultColor
        для light у scene.Lights зробити
            color <- AddLight thing pos normal rd scene color light
        color

    and AddLight (thing: SceneObject) pos normal rd scene color light =
        нехай ldis = light.Pos - pos
        нехай livec = Vector.Norm (ldis)
        нехай neatIsect = TestRay { Start = pos; Dir = livec } scene
        нехай isInShadow =
            співстав neatIsect із
            | None -> false
            | Some d -> not (d > Vector.Mag (ldis))
        якщо isInShadow тоді color
        інакше
            нехай illum = Vector.Dot (livec, normal)
            нехай lcolor =
                якщо illum > 0.0
                тоді Color.Scale (illum, light.Color)
                інакше Color.DefaultColor
            нехай specular = Vector.Dot (livec, Vector.Norm (rd))
            нехай scolor =
                якщо specular > 0.0
                тоді Color.Scale (specular ** thing.Surface.Roughness, light.Color)
                інакше Color.DefaultColor
            color + thing.Surface.Diffuse (pos) * lcolor +
                    thing.Surface.Specular (pos) * scolor

    нехай GetPoint x y width height (camera: Camera) =
        нехай RecenterX x =  (float x - (float width / 2.0))  / (2.0 * float width)
        нехай RecenterY y = -(float y - (float height / 2.0)) / (2.0 * float height)
        Vector.Norm (camera.Forward + RecenterX (x) * camera.Right + RecenterY (y) * camera.Up)

    нехай Render scene (data: byte[]) (x, y, width, height) =
        нехай clamp v = min (max (v * 255.0) 0.0) 255.0 |> byte
        для y = y to height-1 зробити
            нехай stride = y * width
            для x = x to width-1 зробити
                нехай index = (x + stride) * 4
                нехай dir = GetPoint x y width height scene.Camera
                нехай ray = { Start = scene.Camera.Pos; Dir = dir }
                нехай color = TraceRay ray scene 0
                data.[index+0] <- clamp color.R
                data.[index+1] <- clamp color.G
                data.[index+2] <- clamp color.B
                data.[index+3] <- 255uy

модуль SceneObjects =

    тип Sphere (center, radius, surface) =
        інтерфейс SceneObject із
            член this.Surface = surface
            член this.Normal pos = Vector.Norm (pos - center)
            член this.Intersect ray =
                нехай eo = center - ray.Start
                нехай v = Vector.Dot (eo, ray.Dir)
                нехай dist =
                    якщо (v < 0.0) тоді infinity
                    інакше
                        нехай disc = radius * radius - (Vector.Dot (eo,eo) - (v*v))
                        якщо disc < 0.0
                        тоді infinity
                        інакше v - (sqrt (disc))
                dist

    тип Plane (normal, offset, surface) =
        інтерфейс SceneObject із
            член this.Surface = surface
            член this.Normal pos = normal
            член this.Intersect ray =
                нехай denom = Vector.Dot (normal, ray.Dir)
                нехай dist =
                    якщо denom > 0.0
                    тоді infinity
                    інакше (Vector.Dot (normal, ray.Start) + offset) / (-denom)
                dist

модуль Surfaces =

    тип Shiny() =
        інтерфейс Surface із
            член s.Diffuse pos = Color.White
            член s.Specular pos = Color.Grey
            член s.Reflect pos = 0.7
            член s.Roughness = 250.0

    тип Checkerboard() =
        інтерфейс Surface із
            член s.Diffuse pos =
                якщо (int (floor (pos.Z) + floor (pos.X))) % 2 <> 0
                тоді Color.White
                інакше Color.Black
            член s.Specular pos = Color.White
            член s.Reflect pos =
                якщо (int (floor (pos.Z) + floor (pos.X))) % 2 <> 0
                тоді 0.1
                інакше 0.7
            член s.Roughness = 150.0

модуль Scenes =

    нехай TwoSpheresOnACheckerboard = {
        Things = [|
            SceneObjects.Plane ({ X = 0.0; Y = 1.0; Z = 0.0 }, 0.0, Surfaces.Checkerboard())
            SceneObjects.Sphere ({ X = 0.0; Y = 1.0; Z = -0.25 }, 1.0, Surfaces.Shiny())
            SceneObjects.Sphere ({ X = -1.0; Y = 0.5; Z = 1.5 }, 0.5, Surfaces.Shiny())
        |];
        Lights = [|
            { Pos = { X = -2.0; Y = 2.5; Z = 0.0 }; Color = { R = 0.49; G = 0.07; B = 0.07 } }
            { Pos = { X = 1.5; Y = 2.5; Z = 1.5 }; Color = { R = 0.07; G = 0.07; B = 0.49 } }
            { Pos = { X = 1.5; Y = 2.5; Z = -1.5 }; Color = { R = 0.07; G = 0.49; B = 0.071 } }
            { Pos = { X = 0.0; Y = 3.5; Z = 0.0 }; Color = { R = 0.21; G = 0.21; B = 0.35 } }
        |];
        Camera =
            Camera ({ X = 3.0; Y = 2.0; Z = 4.0 }, { X = -1.0; Y = 0.5; Z = 0.0 })
    }

відкрити Fable.Core.JsInterop
відкрити Browser.Types
відкрити Browser

нехай renderScene scene (x, y, width, height) =
    нехай canvas = document.getElementsByTagName("canvas").[0] :?> HTMLCanvasElement
    нехай ctx = canvas.getContext_2d()
    нехай img = ctx.createImageData(float width, float height)
    RayTracer.Render scene img.data (x, y, width, height)
    ctx.putImageData(img, float -x, float -y)

нехай measure f x y =
    нехай dtStart = window?performance?now()
    нехай res = f x y
    нехай elapsed = window?performance?now() - dtStart
    res, elapsed

нехай x, y, w, h = (0, 0, 512, 512)
нехай _, elapsed = measure renderScene Scenes.TwoSpheresOnACheckerboard (x, y, w, h)
printfn "Ray tracing:\n - rendered image size: (%dx%d)\n - elapsed: %f ms" w h elapsed
