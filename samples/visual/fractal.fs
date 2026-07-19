// Fractal playground by Mark Pattison (Twitter @mark_pattison)
// Source code available у Github: https://github.com/markpattison/FableFractal

модуль Elmish =

    відкрити System
    відкрити Fable.Core
    відкрити Browser
    відкрити Browser.Types

    // ------------------------------------------------------------------------------------------------
    // Virtual Dom bindings
    // ------------------------------------------------------------------------------------------------

    тип IVirtualdom =
        абстрактний h: arg1: string * arg2: obj * arg3: obj[] -> obj
        абстрактний diff: tree1:obj * tree2:obj -> obj
        абстрактний patch: node:obj * patches:obj -> Node
        абстрактний create: e:obj -> Node

    [<Global("virtualDom")>]
    нехай Virtualdom: IVirtualdom = jsNative

    // ------------------------------------------------------------------------------------------------
    // F# representation з DOM and rendering using VirtualDom
    // ------------------------------------------------------------------------------------------------

    тип DomAttribute =
        | EventHandler з (Event -> unit)
        | Attribute з string
        | Property з string

    тип DomNode =
        | Text з string
        | Element з tag:string * attributes:(string * DomAttribute)[] * children : DomNode[]

    нехай createTree tag args children =
            нехай attrs = ResizeArray<_>()
            нехай props = ResizeArray<_>()
            для k, v у args зробити
                співстав k, v із
                | "style", Attribute v
                | "style", Property v ->
                        нехай args = v.Split(';') |> Array.map (фун a ->
                            нехай sep = a.IndexOf(':')
                            якщо sep > 0 тоді a.Substring(0, sep), box (a.Substring(sep+1))
                            інакше a, box "" )
                        props.Add ("style", JsInterop.createObj args)
                | "class", Attribute v
                | "class", Property v ->
                        attrs.Add (k, box v)
                | k, Attribute v ->
                        attrs.Add (k, box v)
                | k, Property v ->
                        props.Add (k, box v)
                | k, EventHandler f ->
                        props.Add (k, box f)
            нехай attrs = JsInterop.createObj attrs
            нехай props = JsInterop.createObj (Seq.append ["attributes", attrs] props)
            нехай elem = Virtualdom.h(tag, props, children)
            elem

    нехай rec render node =
        співстав node із
        | Text(s) ->
                box s
        | Element(tag, attrs, children) ->
                createTree tag attrs (Array.map render children)

    // ------------------------------------------------------------------------------------------------
    // Helpers для dynamic property access & для creating HTML elements
    // ------------------------------------------------------------------------------------------------

    тип Dynamic() =
        [<Emit("$0[$1]")>]
        статичний член (?) (d:Dynamic, s:string) : Dynamic = jsNative

    нехай text s = Text(s)
    нехай (=>) k v = k, Property(v)
    нехай (=!>) k f = k, EventHandler(фун e -> f e)

    тип El() =
        статичний член (?) (_:El, n:string) = фун a b ->
            Element(n, Array.ofList a, Array.ofList b)

    нехай h = El()

    // ------------------------------------------------------------------------------------------------
    // Entry point - create event and update on trigger
    // ------------------------------------------------------------------------------------------------

    тип Cmd<'Msg> = (('Msg -> unit) -> unit) list

    тип SingleObservable<'T>() =
        нехай змінливий listener: IObserver<'T> option = None
        член _.Trigger v =
            співстав listener із
            | Some lis -> lis.OnNext v
            | None -> ()
        інтерфейс IObservable<'T> із
            член _.Subscribe w =
                listener <- Some w
                { новий IDisposable із
                    член _.Dispose() = () }

    нехай app id (init: unit -> 'Model * Cmd<'Msg>) update view =
        нехай event = новий Event<'Msg>()
        нехай trigger e = event.Trigger(e)
        нехай model, cmds = init()
        нехай змінливий state = model
        нехай змінливий tree = view state trigger |> render
        нехай змінливий container = Virtualdom.create(tree)
        document.getElementById(id).appendChild(container) |> ignore

        нехай handleEvent evt =
            нехай model, cmds = update evt state
            нехай newTree = view model trigger |> render
            нехай patches = Virtualdom.diff(tree, newTree)
            container <- Virtualdom.patch(container, patches)
            tree <- newTree
            state <- model
            для cmd у cmds зробити
                cmd trigger

        event.Publish.Add(handleEvent)
        для cmd у cmds зробити
            cmd trigger

модуль WebGLHelper =

  відкрити Browser.Types
  відкрити Fable.Core.JsInterop

  // Shorthand
  тип GL = WebGLRenderingContext

  нехай getWebGLContext (canvas: HTMLCanvasElement) =
      нехай getContext ctxString =
          canvas.getContext(ctxString, createObj [ "premultipliedAlpha" ==> false ]) |> unbox<WebGLRenderingContext>

      нехай webgl = getContext "webgl"

      // If we have webgl = нуль у JS тоді try to get experimental-webgl
      // Edge and webkit use experimental-webgl
      якщо not (unbox webgl) тоді
          getContext "experimental-webgl"
      інакше
          webgl

  нехай createShaderProgram (gl:GL) vertex fragment =
      нехай vertexShader = gl.createShader(gl.VERTEX_SHADER)
      gl.shaderSource(vertexShader, vertex)
      gl.compileShader(vertexShader)

      нехай fragShader = gl.createShader(gl.FRAGMENT_SHADER)
      gl.shaderSource(fragShader, fragment)
      gl.compileShader(fragShader)

      нехай program = gl.createProgram()
      gl.attachShader(program, vertexShader)
      gl.attachShader(program, fragShader)
      gl.linkProgram(program)

      program

  нехай createUniformLocation (gl:GL) program name =
      нехай uniformLocation = gl.getUniformLocation(program, name)
      uniformLocation

  нехай createAttributeLocation (gl : GL) program name =
      нехай attributeLocation = gl.getAttribLocation(program, name)
      gl.enableVertexAttribArray(attributeLocation)

      attributeLocation

  нехай createBuffer (items : float[]) (gl:GL) =
      нехай buffer = gl.createBuffer()

      gl.bindBuffer(gl.ARRAY_BUFFER, buffer)
      gl.bufferData(gl.ARRAY_BUFFER, (createNew Fable.Core.JS.Constructors.Float32Array items) |> unbox, gl.STATIC_DRAW)

      buffer

  нехай clear (gl:GL) (width, height) =
      gl.clearColor(1.0, 1.0, 1.0, 1.0)

      gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA)
      //gl.enable(gl.DEPTH_TEST)
      gl.enable(gl.BLEND)

      gl.viewport(0., 0., width, height)
      gl.clear(float (int gl.COLOR_BUFFER_BIT ||| int gl.DEPTH_BUFFER_BIT))

модуль Types =

  відкрити Browser.Types

  тип Msg =
      | MandelbrotClick
      | JuliaClick
      | JuliaMoveClick
      | JuliaChangeSeedClick
      | MouseDownMsg з MouseEvent
      | MouseUpMsg з MouseEvent
      | MouseMoveMsg з MouseEvent
      | MouseLeaveMsg з MouseEvent
    //   | WheelMsg з WheelEvent
    //   | TouchStartMsg з TouchEvent
    //   | TouchEndMsg з TouchEvent
    //   | TouchMoveMsg з TouchEvent
      | RenderMsg

  тип JuliaSeed = { SeedX: float; SeedY: float }
  тип JuliaScrolling = Move | ChangeSeed

  тип FractalType =
      | Mandelbrot
      | Julia з JuliaSeed * JuliaScrolling

  тип Transform =
      | Scrolling з float * float
      | Pinching з float
      | NoTransform

  тип Model =
      {
          CanvasHeight: float
          Zoom: float
          FractalType: FractalType
          X: float
          Y: float
          Now: System.DateTime
          Render: (Model -> unit) option
          Transform: Transform
      }

модуль FractalRenderer =

  відкрити System
  відкрити Browser
  відкрити Browser.Types
  відкрити WebGLHelper
  відкрити Types

  нехай myVertex = """
      precision highp float;
      precision highp int;

      attribute vec4 aVertexPosition;
      attribute vec2 aTextureCoord;
      varying vec2 vTextureCoord;
      void main() {
        gl_Position = aVertexPosition;
        vTextureCoord = aTextureCoord;
      }
  """

  нехай myFragment = """
      precision highp float;
      precision highp int;
      uniform float uWidthOverHeight;
      uniform float uZoom;
      uniform vec2 uOffset, uJuliaSeed;
      uniform bool uIsJulia;
      varying vec2 vTextureCoord;
      vec2 calculatePosition(vec2 inputCoords, float zoom, float widthOverHeight, vec2 offset)
      {
          повернути (inputCoords - 0.5) * vec2(widthOverHeight, 1.0) / zoom + offset;
      }
      vec4 applyColourMap(float x)
      {
          повернути vec4(sin(x * 4.0), sin (x * 5.0), sin (x * 6.0), 1.0);
      }
      vec2 cConj(vec2 z)
      {
          повернути vec2(z.x, -z.y);
      }
      vec2 cMul(vec2 a, vec2 b)
      {
          повернути vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
      }
      vec2 cSq(vec2 z)
      {
          повернути cMul(z, z);
      }
      vec2 cCube(vec2 z)
      {
          повернути cMul(z, cMul(z, z));
      }
      vec2 cPow4(vec2 z)
      {
          повернути cSq(cSq(z));
      }
      vec2 cDiv(vec2 a, vec2 b)
      {
          повернути cMul(a, cConj(b));
      }
      vec2 cRecip(vec2 z)
      {
          повернути cDiv(vec2(1.0, 0.0), z);
      }
      vec2 f(vec2 z, vec2 offset)
      {
          повернути cSq(z) + offset;
      }
      float pixelResult(vec2 z, vec2 offset)
      {
          float result = 0.0;
          vec2 zsq = z * z;
          int iterations = 0;
          для (int i = 0; i < 128; i++)
          {
              iterations = i;
              якщо (zsq.x + zsq.y > 49.0)
              {
                  break;
              }
              z = f(z, offset);
              zsq = z * z;
          }
          якщо (iterations == 127)
          {
              result = 0.0;
          }
          інакше
          {
              result = float(iterations) + (log(2.0 * log(7.0)) - log(log(zsq.x + zsq.y))) / log(2.0);
              result = log(result * 0.4) / log(128.0);
          }
          повернути result;
      }
      void main(void)
      {
          vec2 z = calculatePosition(vTextureCoord, uZoom, uWidthOverHeight, uOffset);
          float result = pixelResult(z, uIsJulia ? uJuliaSeed : z);
          gl_FragColor = applyColourMap(result);
      }
  """

  нехай initBuffers gl =
      нехай positions =
          createBuffer
              [|
                  -1.0; -1.0;
                    1.0; -1.0;
                  -1.0;  1.0;
                    1.0;  1.0
              |] gl
      нехай textureCoords =
          createBuffer
              [|
                  0.0; 0.0;
                  1.0; 0.0;
                  0.0; 1.0;
                  1.0; 1.0
              |] gl
      positions, textureCoords

  нехай create (holder : Element) =

      нехай canvas = document.createElement "canvas" :?> HTMLCanvasElement
      нехай width = 640
      нехай height = 480

      canvas.width <- float width
      canvas.height <- float height

      holder.appendChild(canvas) |> ignore

      нехай context = getWebGLContext canvas

      нехай program = createShaderProgram context myVertex myFragment

      нехай positionBuffer, colourBuffer = initBuffers context
      нехай vertexPositionAttribute = createAttributeLocation context program "aVertexPosition"
      нехай textureCoordAttribute = createAttributeLocation context program "aTextureCoord"
      нехай widthOverHeightUniform = createUniformLocation context program "uWidthOverHeight"
      нехай zoomUniform = createUniformLocation context program "uZoom"
      нехай offsetUniform = createUniformLocation context program "uOffset"
      нехай juliaSeedUniform = createUniformLocation context program "uJuliaSeed"
      нехай isJuliaUniform = createUniformLocation context program "uIsJulia"

      нехай draw widthOverHeight zoom x y jx jy isJulia =
          context.useProgram(program)

          context.bindBuffer(context.ARRAY_BUFFER, positionBuffer)
          context.vertexAttribPointer(vertexPositionAttribute, 2.0, context.FLOAT, false, 0.0, 0.0)
          context.bindBuffer(context.ARRAY_BUFFER, colourBuffer)
          context.vertexAttribPointer(textureCoordAttribute, 2.0, context.FLOAT, false, 0.0, 0.0)

          context.uniform1f(widthOverHeightUniform, widthOverHeight)
          context.uniform1f(zoomUniform, zoom)
          context.uniform2f(offsetUniform, x, y)
          context.uniform2f(juliaSeedUniform, jx, jy)
          context.uniform1i(isJuliaUniform, якщо isJulia тоді 1.0 інакше 0.0)

          context.drawArrays (context.TRIANGLE_STRIP, 0., 4.0)

      нехай clear = clear context

      // Try not to use "context" after this point, bind a функція above.

      нехай imageLoadCanvas = document.createElement "canvas" :?> HTMLCanvasElement
      нехай imageLoadCanvasContext = imageLoadCanvas.getContext_2d()

      нехай змінливий last = DateTime.Now

      нехай render model =
          співстав model із
          | model when model.Now <> last ->
              last <- model.Now

              нехай resolution = canvas.width, canvas.height
              нехай widthOverHeight = якщо canvas.height = 0.0 тоді 1.0 інакше canvas.width / canvas.height
              clear resolution

              співстав model.FractalType із
              | Mandelbrot ->
                  draw widthOverHeight model.Zoom model.X model.Y 0.0 0.0 false
              | Julia ({ SeedX = seedX; SeedY = seedY }, _) ->
                  draw widthOverHeight model.Zoom model.X model.Y seedX seedY true

          | _ -> ignore()

      render, height

модуль State =

    відкрити Browser
    відкрити Browser.Types
    відкрити Fable.Core.JsInterop
    відкрити Types

    // тип INormalizedWheel =
    //     абстрактний член pixelX: float
    //     абстрактний член pixelY: float
    //     абстрактний член spinX: float
    //     абстрактний член spinY: float

    // нехай normalizeWheel : WheelEvent -> INormalizedWheel = importDefault "normalize-wheel"

    нехай renderCommand =
        нехай sub dispatch =
            window.requestAnimationFrame(фун _ -> dispatch RenderMsg) |> ignore
        [sub]

    нехай initMandelbrot =
        {
            CanvasHeight = 1.0
            Zoom = 0.314
            FractalType = Mandelbrot
            X = -0.5
            Y = 0.0
            Now = System.DateTime.Now
            Render = None
            Transform = NoTransform
        }

    нехай initJulia =
        {
            CanvasHeight = 1.0
            Zoom = 0.314
            FractalType = Julia ({ SeedX = 0.0; SeedY = 0.0 }, ChangeSeed)
            X = 0.0
            Y = 0.0
            Now = System.DateTime.Now
            Render = None
            Transform = NoTransform
        }

    нехай init() =
        document.addEventListener("gesturestart", (фун e -> e.preventDefault()), true)
        document.addEventListener("gesturechange", (фун e -> e.preventDefault()), true)
        document.addEventListener("gestureend", (фун e -> e.preventDefault()), true)
        document.addEventListener("scroll", (фун e -> e.preventDefault()), true)
        initMandelbrot, renderCommand

    нехай updateForMove x y model =
        співстав model.Transform із
        | Scrolling (lastScreenX, lastScreenY) ->
            { model із
                X = model.X - (x - lastScreenX) / (model.Zoom * model.CanvasHeight)
                Y = model.Y + (y - lastScreenY) / (model.Zoom * model.CanvasHeight)
                Transform = Scrolling (x, y)
            }, []
        | _ -> model, []

    нехай updateForSeedChange seed x y model =
        співстав model.Transform із
        | Scrolling (lastScreenX, lastScreenY) ->
            { model із
                FractalType = Julia ( {
                                        SeedX = seed.SeedX - (x - lastScreenX) / (model.Zoom * model.CanvasHeight)
                                        SeedY = seed.SeedY - (y - lastScreenY) / (model.Zoom * model.CanvasHeight)}, ChangeSeed)
                Transform = Scrolling (x, y)
            }, []
        | _ -> model, []

    нехай update msg model =
        співстав model.FractalType, msg із
        | Julia _, MandelbrotClick _ ->
            { model із
                Zoom = 0.314; FractalType = Mandelbrot; X = -0.5; Y = 0.0
            }, []

        | Mandelbrot, JuliaClick ->
            { model із
                Zoom = 0.314; FractalType = Julia ({ SeedX = 0.0; SeedY = 0.0 }, ChangeSeed); X = 0.0; Y = 0.0
            }, []

        | Julia (seed, _), JuliaMoveClick ->
            { model із FractalType = Julia (seed, Move) }, []

        | Julia (seed, _), JuliaChangeSeedClick ->
            { model із FractalType = Julia (seed, ChangeSeed) }, []

        | _, MouseDownMsg me when me.button = 0.0 ->
            { model із
                Transform = Scrolling (me.screenX, me.screenY)
            }, []

        | _, MouseUpMsg me when me.button = 0.0 -> { model із Transform = NoTransform }, []

        | _, MouseLeaveMsg _ -> { model із Transform = NoTransform }, []

        | Mandelbrot, MouseMoveMsg me
        | Julia (_, Move), MouseMoveMsg me ->
            updateForMove me.screenX me.screenY model

        | Julia (seed, ChangeSeed), MouseMoveMsg me ->
            updateForSeedChange seed me.screenX me.screenY model

        // | _, WheelMsg we ->
        //     нехай zoom = (normalizeWheel we).pixelY / 100.0
        //     { model із Zoom = model.Zoom * 0.99 ** zoom }, []

        // | _, TouchEndMsg _ -> { model із Transform = NoTransform }, []

        // | _, TouchStartMsg te when te.touches.Length = 1 ->
        //     { model із
        //         Transform = Scrolling (te.touches.[0].clientX, te.touches.[0].clientY)
        //     }, []

        // | _, TouchStartMsg te when te.touches.Length = 2 ->
        //     нехай dx = te.touches.[1].clientX - te.touches.[0].clientX
        //     нехай dy = te.touches.[1].clientY - te.touches.[0].clientY
        //     нехай distance = sqrt (dx * dx + dy * dy)
        //     { model із
        //         Transform = Pinching distance
        //     }, []

        // | Mandelbrot, TouchMoveMsg te
        // | Julia (_, Move), TouchMoveMsg te when te.touches.Length = 1 ->
        //     updateForMove te.touches.[0].screenX te.touches.[0].screenY model

        // | Julia (seed, ChangeSeed), TouchMoveMsg te when te.touches.Length = 1 ->
        //     updateForSeedChange seed te.touches.[0].screenX te.touches.[0].screenY model

        // | Mandelbrot, TouchMoveMsg te
        // | Julia _, TouchMoveMsg te when te.touches.Length = 2 ->
        //     співстав model.Transform із
        //     | Pinching lastDistance ->
        //         нехай dx = te.touches.[1].clientX - te.touches.[0].clientX
        //         нехай dy = te.touches.[1].clientY - te.touches.[0].clientY
        //         нехай distance = sqrt (dx * dx + dy * dy)
        //         { model із
        //             Zoom = model.Zoom * 0.99 ** (lastDistance - distance)
        //             Transform = Pinching distance
        //         }, []
        //     | _ -> model, []

        | _, RenderMsg ->
            співстав model.Render із
            | None ->
                нехай holder = document.getElementById("Fractal")
                співстав holder із
                | нуль -> model, renderCommand
                | h ->
                    нехай renderer, height = FractalRenderer.create h
                    { model із Render = Some renderer; CanvasHeight = float height }, renderCommand
            | Some render ->
                render model
                { model із Now = System.DateTime.Now }, renderCommand

        | _ -> model, []

модуль View =

    відкрити Elmish
    відкрити Types
    відкрити State

    нехай showParams model =
        співстав model.FractalType із
        | Julia (seed, _) ->
            [
                h?p [] [ Text $"X = %.6f{model.X}" ]
                h?p [] [ Text $"Y = %.6f{model.Y}" ]
                h?p [] [ Text $"Zoom = %.6f{model.Zoom}" ]
                h?p [] [ Text $"Seed X = %.6f{seed.SeedX}" ]
                h?p [] [ Text $"Seed Y = %.6f{seed.SeedY}" ]
            ]
        | Mandelbrot ->
            [
                h?p [] [ Text $"X = %.6f{model.X}" ]
                h?p [] [ Text $"Y = %.6f{model.Y}" ]
                h?p [] [ Text $"Zoom = %.6f{model.Zoom}" ]
            ]

    нехай showButtons model dispatch =
        h?div [] [
            h?div [ "class" => "field has-addons" ] [
                h?button [
                    (співстав model.FractalType із
                        | Mandelbrot -> "class" => "button is-primary is-selected"
                        | Julia _ -> "class" => "button")
                    "onclick" =!> (фун _ -> MandelbrotClick |> dispatch)
                ] [ Text "Mandelbrot" ]
                h?button [
                    (співстав model.FractalType із
                        | Mandelbrot -> "class" => "button"
                        | Julia _ -> "class" => "button is-primary is-selected")
                    "onclick" =!> (фун _ -> JuliaClick |> dispatch)
                ] [ Text "Julia" ]
            ]
            h?div [] [
                співстав model.FractalType із
                | Julia (_, scrollType) ->
                    поступатися h?button [
                        (співстав scrollType із
                            | Move -> "class" => "button is-primary is-selected"
                            | ChangeSeed -> "class" => "button")
                        "onclick" =!> (фун _ -> JuliaMoveClick |> dispatch)
                    ] [ Text "Move" ]
                    поступатися h?button [
                        (співстав scrollType із
                            | Move -> "class" => "button"
                            | ChangeSeed -> "class" => "button is-primary is-selected")
                        "onclick" =!> (фун _ -> JuliaChangeSeedClick |> dispatch)
                    ] [ Text "ChangeSeed" ]
                | _ -> ()
            ]
        ]

    нехай hud model dispatch =
        h?div [ "class" => "columns" ] [
            h?div [ "class" => "column" ] (showParams model)
            h?div [ "class" => "column" ] [ showButtons model dispatch ]
        ]

    нехай fractalCanvas dispatch =
        нехай dispatch (msg: 'Event -> Msg) (e: Browser.Types.Event) =
            e.preventDefault()
            msg (e :?> 'Event) |> dispatch

        h?div [
            "id" => "Fractal"
            "onmousedown" =!> dispatch MouseDownMsg
            "onmouseup" =!> dispatch MouseUpMsg
            "onmousemove" =!> dispatch MouseMoveMsg
            "onmouseleave" =!> dispatch MouseLeaveMsg
            // "onwheel" =!> dispatch WheelMsg
            // "ontouchstart" =!> dispatch TouchStartMsg
            // "ontouchmove" =!> dispatch TouchMoveMsg
            // "ontouchend" =!> dispatch TouchEndMsg
            // "ontouchcancel" =!> dispatch TouchEndMsg
        ] []

    нехай root model dispatch =
        h?div [] [
            hud model dispatch
            fractalCanvas dispatch
        ]

    app "FableFractal" init update root
