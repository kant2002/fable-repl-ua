модуль ВебКомпонент

// Веб Компоненти із Fable від Onur Gümüş (Twitter @OnurGumusDev)
// Перевірте тег користувача у вкладинці HTML і прочитайте цю гілку для більшої інформації:
// https://twitter.com/OnurGumusDev/status/1329019698667790337

// Для більш високорівневої бібліотеки для будування Веб Компонентів, спробуйте Fable.Lit:
// https://fable.io/Fable.Lit/docs/web-components.html

відкрити Fable.Core
відкрити Browser
відкрити Browser.Types
відкрити Fable.Core.JsInterop

[<AllowNullLiteral>]
тип HTMLTemplateElement =
    успадкує HTMLElement
    абстрактний content: DocumentFragment із get, set

[<AllowNullLiteral>]
тип HTMLTemplateElementType =
    [<EmitConstructor>]
    абстрактний Create: unit -> HTMLTemplateElement

нехай шаблон: HTMLTemplateElement =
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
модуль customElements =
    нехай define (назваЕлементу: string, ty: obj) = jsNative

[<Global>]
тип ShadowRoot() =
    член this.appendChild(el: Browser.Types.Node) = jsNative
    член this.querySelector(selector: string): Browser.Types.HTMLElement = jsNative

нехай інлайн приєднатиСтатичне<'T> (назва: string) (f: obj): unit = jsConstructor<'T>?назва <- f

нехай інлайн приєднатиСтатичнийГеттер<'T, 'V> (назва: string) (f: unit -> 'V): unit =
    JS.Constructors.Object.defineProperty (jsConstructor<'T>, назва, !!{| get = f |})
    |> ignore

[<Global; AbstractClass>]
[<AllowNullLiteral>]
тип HTMLElement() =
    член _.getAttribute(attr: string): string = jsNative
    член _.attachShadow(obj): ShadowRoot = jsNative
    абстрактний connectedCallback: unit -> unit
    абстрактний attributeChangedCallback: string * obj * obj -> unit

[<AllowNullLiteral>]
тип Button() =
    успадкує HTMLElement()

    нехай тіньовийКорень: ShadowRoot = base.attachShadow ({| mode = "відкрити" |})

    зробити
        нехай клон = шаблон.content.cloneNode (true)
        тіньовийКорень.appendChild (клон)

    нехай кнопка = тіньовийКорень.querySelector ("button")

    член this.перемалювати() =
        кнопка.innerHTML <- this.getAttribute ("мітка")

    override _.connectedCallback() = printf "обратний виклик підключено"

    override this.attributeChangedCallback(name, oldVal, newVal) = this.перемалювати ()

приєднатиСтатичнийГеттер<Button, _> "observedAttributes" (фун () -> [| "мітка" |])

customElements.define ("my-button", jsConstructor<Button>)
