// More info about Fulma at https://mangelmaxime.github.io/Fulma/
модуль Fulma.Tabs

відкрити Fable.React
відкрити Fulma

нехай basic () =
    Tabs.tabs [ ]
        [ Tabs.tab [ Tabs.Tab.IsActive true ]
            [ a [ ] [ str "Fable" ] ]
          Tabs.tab [ ]
            [ a [ ] [ str "Elmish" ] ]
          Tabs.tab [ ]
            [ a [ ] [ str "Bulma" ] ]
          Tabs.tab [ ]
            [ a [ ] [ str "Hink" ] ] ]

нехай alignment () =
    Tabs.tabs [ Tabs.IsCentered ]
        [ Tabs.tab [ Tabs.Tab.IsActive true ]
            [ a [ ] [ str "Fable" ] ]
          Tabs.tab [ ]
            [ a [ ] [ str "Elmish" ] ]
          Tabs.tab [ ]
            [ a [ ] [ str "Bulma" ] ]
          Tabs.tab [ ]
            [ a [ ] [ str "Hink" ] ] ]

нехай size () =
    Tabs.tabs [ Tabs.Size IsLarge ]
        [ Tabs.tab [ Tabs.Tab.IsActive true ]
            [ a [ ] [ str "Fable" ] ]
          Tabs.tab [ ]
            [ a [ ] [ str "Elmish" ] ]
          Tabs.tab [ ]
            [ a [ ] [ str "Bulma" ] ]
          Tabs.tab [ ]
            [ a [ ] [ str "Hink" ] ] ]

нехай styles () =
    Tabs.tabs [ Tabs.IsFullWidth
                Tabs.IsBoxed ]
        [ Tabs.tab [ Tabs.Tab.IsActive true ]
            [ a [ ] [ str "Fable" ] ]
          Tabs.tab [ ]
            [ a [ ] [ str "Elmish" ] ]
          Tabs.tab [ ]
            [ a [ ] [ str "Bulma" ] ]
          Tabs.tab [ ]
            [ a [ ] [ str "Hink" ] ] ]

div [] [
    Card.card [] [Card.content [] [basic()] ]
    Card.card [] [Card.content [] [alignment()] ]
    Card.card [] [Card.content [] [size()] ]
    Card.card [] [Card.content [] [styles()] ]
] |> mountById "elmish-app"
