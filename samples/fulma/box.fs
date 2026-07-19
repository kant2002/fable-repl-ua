// More info about Fulma at https://mangelmaxime.github.io/Fulma/
модуль Fulma.Box

відкрити Fable.React
відкрити Fable.React.Props
відкрити Fulma

нехай basic () =
    div [ Class "block" ]
        [ Box.box' [ ]
            [ str "Lorem ipsum dolor sit amet, consectetur adipisicing elit
                   , sed зробити eiusmod tempor incididunt ut labore et dolore magna aliqua."] ]

div [] [
    Card.card [] [Card.content [] [basic()] ]
] |> mountById "elmish-app"
