// More info about Fulma at https://mangelmaxime.github.io/Fulma/
модуль Fulma.Delete

відкрити Fable.React
відкрити Fable.React.Props
відкрити Fulma

нехай demoInteractive () =
    div [ Class "block" ]
        [ Delete.delete
            [ Delete.Size IsSmall ] [ ]
          Delete.delete
            [ ] [ ]
          Delete.delete
            [ Delete.Size IsMedium ] [ ]
          Delete.delete
            [ Delete.Size IsLarge ] [ ] ]

div [] [
    Card.card [] [Card.content [] [demoInteractive()] ]
] |> mountById "elmish-app"
