// More info about Fulma at https://mangelmaxime.github.io/Fulma/
модуль Fulma.Notification

відкрити Fable.Core
відкрити Fable.Core.JsInterop
відкрити Fable.React
відкрити Fable.React.Props
відкрити Fulma

нехай basic () =
    Notification.notification [ ]
        [ str "I am a notification" ]

нехай color () =
    Notification.notification [ Notification.Color IsSuccess ]
        [ str "I am a notification із some colors" ]

нехай withCross () =
    Notification.notification [ Notification.Color IsDanger ]
        [ Notification.delete [ ] [ ]
          str "I am a notification із some colors and a delete button" ]

div [] [
    Card.card [] [Card.content [] [basic()] ]
    Card.card [] [Card.content [] [color()] ]
    Card.card [] [Card.content [] [withCross()] ]
] |> mountById "elmish-app"
