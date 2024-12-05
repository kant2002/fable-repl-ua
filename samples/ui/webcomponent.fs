module ВебКомпонент

// Веб Компоненти із Fable від Onur Gümüş (Twitter @OnurGumusDev)
// Перевірте тег користувача у вкладинці HTML і прочитайте цю гілку для більшої інформації:
// https://twitter.com/OnurGumusDev/status/1329019698667790337

// Для більш високорівневої бібліотеки для будування Веб Компонентів, спробуйте Fable.Lit:
// https://fable.io/Fable.Lit/docs/web-components.html

open Fable.Core
open Browser
open Browser.Types
open Fable.Core.JsInterop

[<AllowNullLiteral>]
type HTMLTemplateElement =
    inherit HTMLElement
    abstract content: DocumentFragment with get, set

[<AllowNullLiteral>]
type HTMLTemplateElementType =
    [<EmitConstructor>]
    abstract Create: unit -> HTMLTemplateElement

let шаблон: HTMLTemplateElement =
    downcast document.createElement ("template")

шаблон.innerHTML <-
    """
  <style>
    .container {
      padding: 8px;
    }
    button {
      display: block;
      overflow: hidden;
      position: relative;
      padding: 0 16px;
      font-size: 16px;
      font-weight: bold;
      text-overflow: ellipsis;
      white-space: nowrap;
      cursor: pointer;
      outline: none;
      width: 100%;
      height: 40px;
      box-sizing: border-box;
      border: 1px solid #a1a1a1;
      background: #ffffff;
      box-shadow: 0 2px 4px 0 rgba(0,0,0, 0.05), 0 2px 8px 0 rgba(161,161,161, 0.4);
      color: #363636;
      cursor: pointer;
    }
  </style>
  <div class="container">
    <button>Label</button>
  </div>
"""

[<Global>]
module customElements =
    let define (назваЕлементу: string, ty: obj) = jsNative

[<Global>]
type ShadowRoot() =
    member this.appendChild(el: Browser.Types.Node) = jsNative
    member this.querySelector(selector: string): Browser.Types.HTMLElement = jsNative

let inline приєднатиСтатичне<'T> (назва: string) (f: obj): unit = jsConstructor<'T>?назва <- f

let inline приєднатиСтатичнийГеттер<'T, 'V> (назва: string) (f: unit -> 'V): unit =
    JS.Constructors.Object.defineProperty (jsConstructor<'T>, назва, !!{| get = f |})
    |> ignore

[<Global; AbstractClass>]
[<AllowNullLiteral>]
type HTMLElement() =
    member _.getAttribute(attr: string): string = jsNative
    member _.attachShadow(obj): ShadowRoot = jsNative
    abstract connectedCallback: unit -> unit
    abstract attributeChangedCallback: string * obj * obj -> unit

[<AllowNullLiteral>]
type Button() =
    inherit HTMLElement()

    let тіньовийКорень: ShadowRoot = base.attachShadow ({| mode = "open" |})

    do
        let клон = шаблон.content.cloneNode (true)
        тіньовийКорень.appendChild (клон)

    let кнопка = тіньовийКорень.querySelector ("button")

    member this.перемалювати() =
        кнопка.innerHTML <- this.getAttribute ("мітка")

    override _.connectedCallback() = printf "обратний виклик підключено"

    override this.attributeChangedCallback(name, oldVal, newVal) = this.перемалювати ()

приєднатиСтатичнийГеттер<Button, _> "observedAttributes" (fun () -> [| "мітка" |])

customElements.define ("my-button", jsConstructor<Button>)
