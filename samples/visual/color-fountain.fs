модуль Color.Fountain

// Color Fountain by Erik Novales: https://github.com/enovales

відкрити Fable.Core
відкрити Fable.Core.JsInterop
відкрити Browser.Types
відкрити Browser

нехай канва = document.getElementsByTagName("canvas").[0] :?> HTMLCanvasElement
канва.width <- 1000.
канва.height <- 800.
нехай кткст = канва.getContext_2d()

нехай гвч (): float = JS.Math.random()

нехай обмеженняЧастинок = 200

тип Частинка = {
    x: double
    y: double
    xшивд: double
    yшивд: double
    к: (int * int * int)
    оберт: double
    обертШвид: double
}
із
    override this.ToString() =
        нехай (r,g,b) = this.к
        sprintf "Частинка(x = %O, y = %O, xшивд = %O, yшивд = %O, c = (%O, %O, %O))"
            this.x this.y this.xшивд this.yшивд r g b


нехай оновитиЧастинку(dt: double)(ч: Частинка) =
    {
        ч із
            x = ч.x + ч.xшивд * dt
            y = ч.y + ч.yшивд * dt
            yшивд = ч.yшивд + 1. * dt
            оберт = (ч.оберт + ч.обертШвид * dt) % (2. * 3.14159)
    }

нехай заповнитиЧастинки(ч: Частинка array, dt: double) =
    нехай щеДійсні =
        ч |> Array.filter(фун pt -> (pt.y < 1000.))
    //System.Console.WriteLine("stillValid.Length = " + stillValid.Length.ToString())
    нехай оновитиПоз =
        щеДійсні
        |> Array.map(оновитиЧастинку(dt))

    //System.Console.WriteLine("updatedPos = " + updatedPos |> Array.map(фун p -> p.ToString()).ToString())
    нехай дляСтворення = обмеженняЧастинок - щеДійсні.Length
    //System.Console.WriteLine("going to create " + toCreate.ToString() + " particles")
    нехай новіЧасинки =
        seq {
            для i у 0..дляСтворення зробити
                поступатися {
                    Частинка.x = 200.
                    y = 300.
                    xшивд = (гвч() - 0.5) * (гвч() * 30.)
                    yшивд = -(гвч() * 25.)
                    к = (int (гвч() * 255.), int (гвч() * 255.), int (гвч() * 255.))
                    оберт = (гвч() * 2. * 3.14159)
                    обертШвид = (гвч() * 1.5)
                }
        }
        |> Seq.toArray

    оновитиПоз |> Array.append(новіЧасинки)

нехай змінливий частинки = [||]
нехай крокчасу = 0.8

нехай rec цикл останній t =
    // Comment out this line to make sure the animation runs
    // із same speed on different frame rates
    // нехай timestep = (t - last) / 20.
    частинки <- заповнитиЧастинки(частинки, крокчасу)

    кткст.clearRect(0., 0., 10000., 10000.)
    нехай намалюватиЧастинку(p: Частинка) =
        нехай (r,g,b) = p.к
        нехай fs = "rgb(" + r.ToString() + ", " + g.ToString() + ", " + b.ToString() + ")"
        кткст.fillStyle <- !^fs

        нехай x1 = (p.x - 5.)
        нехай x2 = (p.x + 5.)
        нехай y1 = (p.y - 5.)
        нехай y2 = (p.y + 5.)

        // нехай x1 = (p.x - (10. * System.Math.Cos(p.rot)))
        // нехай x2 = (p.x + (10. * System.Math.Cos(p.rot)))
        // нехай y1 = (p.y - (10. * System.Math.Sin(p.rot)))
        // нехай y2 = (p.y + (10. * System.Math.Sin(p.rot)))

        // ctx.fillRect(x1, y1, 10., 10.)
        кткст.beginPath()
        кткст.moveTo(x1, y1)
        кткст.lineTo(x2, y1)
        кткст.lineTo(x2, y2)
        кткст.lineTo(x1, y2)
        кткст.lineTo(x1, y1)
        кткст.closePath()
        кткст.fill()

    частинки
    |> Array.iter намалюватиЧастинку

    window.requestAnimationFrame(цикл t) |> ignore

// start the loop
цикл 0. 0.
