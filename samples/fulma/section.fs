// More info about Fulma at https://mangelmaxime.github.io/Fulma/
модуль Fulma.Section

відкрити Fable.React
відкрити Fulma

нехай basic () =
    Section.section [ ]
        [ Container.container [ Container.IsFluid ]
            [ Heading.h1 [ ]
                [ str "Section" ]
              Heading.h2 [ Heading.IsSubtitle ]
                [ str "A simple container to divide your page into sections" ] ] ]

div [] [
    Card.card [] [Card.content [] [basic()] ]
] |> mountById "elmish-app"
