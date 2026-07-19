// More info about Fulma at https://mangelmaxime.github.io/Fulma/
модуль Fulma.Footer

відкрити Fable.React
відкрити Fulma

нехай basic () =
    Footer.footer [ ]
        [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ h1 [ ]
                 [ str "Fulma" ]
              p [ ]
                [ str "A wrapper around Bulma to help you create application quicker" ] ] ]

div [] [
    Card.card [] [Card.content [] [basic()] ]
] |> mountById "elmish-app"
