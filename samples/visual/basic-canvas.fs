модуль BasicCanvas

відкрити Fable.Core
відкрити Fable.Core.JsInterop
відкрити Browser.Types
відкрити Browser

нехай ініц() =
    нехай канва = document.querySelector(".view") :?> HTMLCanvasElement

    нехай кткст = канва.getContext_2d()
    // The (!^) operator checks and casts a value to an Erased Union тип
    // See http://fable.io/docs/interacting.html#Erase-attribute
    кткст.fillStyle <- !^"rgb(200,0,0)"
    кткст.fillRect (10., 10., 55., 50.)
    кткст.fillStyle <- !^"rgba(0, 0, 200, 0.5)"
    кткст.fillRect (30., 30., 55., 50.)

ініц()
