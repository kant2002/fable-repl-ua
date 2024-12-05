module Color.Fountain

// Color Fountain by Erik Novales: https://github.com/enovales

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Browser

let канва = document.getElementsByTagName("canvas").[0] :?> HTMLCanvasElement
канва.width <- 1000.
канва.height <- 800.
let кткст = канва.getContext_2d()

let гвч (): float = JS.Math.random()

let обмеженняЧастинок = 200

type Частинка = {
    x: double
    y: double
    xшивд: double
    yшивд: double
    к: (int * int * int)
    оберт: double
    обертШвид: double
}
with
    override this.ToString() =
        let (r,g,b) = this.к
        sprintf "Частинка(x = %O, y = %O, xшивд = %O, yшивд = %O, c = (%O, %O, %O))"
            this.x this.y this.xшивд this.yшивд r g b


let оновитиЧастинку(dt: double)(ч: Частинка) =
    {
        ч with
            x = ч.x + ч.xшивд * dt
            y = ч.y + ч.yшивд * dt
            yшивд = ч.yшивд + 1. * dt
            оберт = (ч.оберт + ч.обертШвид * dt) % (2. * 3.14159)
    }

let заповнитиЧастинки(ч: Частинка array, dt: double) =
    let щеДійсні =
        ч |> Array.filter(fun pt -> (pt.y < 1000.))
    //System.Console.WriteLine("stillValid.Length = " + stillValid.Length.ToString())
    let оновитиПоз =
        щеДійсні
        |> Array.map(оновитиЧастинку(dt))

    //System.Console.WriteLine("updatedPos = " + updatedPos |> Array.map(fun p -> p.ToString()).ToString())
    let дляСтворення = обмеженняЧастинок - щеДійсні.Length
    //System.Console.WriteLine("going to create " + toCreate.ToString() + " particles")
    let новіЧасинки =
        seq {
            for i in 0..дляСтворення do
                yield {
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

let mutable частинки = [||]
let крокчасу = 0.8

let rec цикл останній t =
    // Comment out this line to make sure the animation runs
    // with same speed on different frame rates
    // let timestep = (t - last) / 20.
    частинки <- заповнитиЧастинки(частинки, крокчасу)

    кткст.clearRect(0., 0., 10000., 10000.)
    let намалюватиЧастинку(p: Частинка) =
        let (r,g,b) = p.к
        let fs = "rgb(" + r.ToString() + ", " + g.ToString() + ", " + b.ToString() + ")"
        кткст.fillStyle <- !^fs

        let x1 = (p.x - 5.)
        let x2 = (p.x + 5.)
        let y1 = (p.y - 5.)
        let y2 = (p.y + 5.)

        // let x1 = (p.x - (10. * System.Math.Cos(p.rot)))
        // let x2 = (p.x + (10. * System.Math.Cos(p.rot)))
        // let y1 = (p.y - (10. * System.Math.Sin(p.rot)))
        // let y2 = (p.y + (10. * System.Math.Sin(p.rot)))

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