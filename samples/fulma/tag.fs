// More info about Fulma at https://mangelmaxime.github.io/Fulma/
модуль Fulma.Tag

відкрити Fable.React
відкрити Fable.React.Props
відкрити Fulma

нехай colorInteractive () =
    div [ Class "block" ]
        [ Tag.tag [ ] [ str "Default" ]
          Tag.tag [ Tag.Color IsWhite ] [ str "White" ]
          Tag.tag [ Tag.Color IsLight ] [ str "Light" ]
          Tag.tag [ Tag.Color IsDark ] [ str "Dark" ]
          Tag.tag [ Tag.Color IsBlack ] [ str "Black" ]
          Tag.tag [ Tag.Color IsPrimary ] [ str "Primary" ]
          Tag.tag [ Tag.Color IsInfo ] [ str "Info" ]
          Tag.tag [ Tag.Color IsSuccess ] [ str "Success" ]
          Tag.tag [ Tag.Color IsWarning ] [ str "Warning" ]
          Tag.tag [ Tag.Color IsDanger ] [ str "Danger" ] ]

нехай sizeInteractive () =
    div [ Class "block" ]
        [ Tag.tag [ ] [ str "Normal" ]
          Tag.tag [ Tag.Color IsPrimary; Tag.Size IsMedium ] [ str "Medium" ]
          Tag.tag [ Tag.Color IsInfo; Tag.Size IsLarge ] [ str "Large" ] ]

нехай nestedDeleteStyleInteractive () =
    div [ Class "block" ]
        [ Tag.tag [ Tag.Color IsDark ]
            [ str "With delete"
              Delete.delete [ Delete.Size IsSmall ] [ ] ]
          Tag.tag [ Tag.Size IsMedium ]
            [ str "With delete"
              Delete.delete [ ] [ ] ]
          Tag.tag [ Tag.Color IsWarning; Tag.Size IsLarge ]
            [ str "With delete"
              Delete.delete [ Delete.Size IsLarge ] [ ] ] ]

нехай list () =
    Tag.list [ Tag.List.HasAddons ]
        [ Tag.tag [ Tag.Color IsDanger ] [ str "Maxime Mangel" ]
          Tag.delete [ ] [ ] ]

div [] [
    Card.card [] [Card.content [] [colorInteractive()] ]
    Card.card [] [Card.content [] [sizeInteractive()] ]
    Card.card [] [Card.content [] [nestedDeleteStyleInteractive()] ]
    Card.card [] [Card.content [] [list()] ]
] |> mountById "elmish-app"
