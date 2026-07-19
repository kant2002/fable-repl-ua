модуль Elmish.Memory

(**
 Classic Memory game, by Zaid Ajaj.
 You can find more info about Emish architecture and samples at https://elmish.github.io/
*)

відкрити Fable.React
відкрити Fable.React.Props
відкрити Browser
відкрити Elmish
відкрити Elmish.React

// Types
тип Card = {
    Id : int
    ImgUrl : string
    Selected : bool
    MatchFound : bool
}

тип Model = {
    Cards : Card list
    FirstSelection : int option
    SecondSelection : int option
}

тип Actions =
    | SelectCard з int
    | StartNewGame
    | NoOp

// State
нехай random = новий System.Random()

нехай origin =
    // Sample is running у an iframe, so get the location з parent
    нехай topLocation = window.top.location
    topLocation.origin + topLocation.pathname

нехай getCards() =
    нехай images = [ "violin"; "electric-guitar"; "headphones"; "piano"; "saxophone";  "trumpet";"turntable";"bass-guitar" ]
    images
    |> List.append images
    |> List.sortBy (фун img -> random.Next())
    |> List.map (sprintf "%simg/memory/%s.png" origin)
    |> List.mapi (фун index img -> { Id = index; ImgUrl = img; Selected = false; MatchFound = false})

нехай initialModel() = {
    Cards = getCards()
    FirstSelection = None
    SecondSelection = None
}

нехай cardsEqual id1 id2 (cards: Card list) =
    нехай card1 = cards |> List.find (фун c -> c.Id = id1)
    нехай card2 = cards |> List.find (фун c -> c.Id = id2)
    card1.ImgUrl = card2.ImgUrl

нехай cardSelected id (cards: Card list) =
    нехай card = List.find (фун c -> c.Id = id) cards
    card.Selected

нехай gameCleared (model: Model) =
    List.forall (фун card -> card.MatchFound) model.Cards

нехай update action model  =
    співстав action із
    | StartNewGame -> initialModel()
    | SelectCard index ->
        співстав model.FirstSelection, model.SecondSelection із
        | None, None ->
            нехай cards =
                model.Cards
                |> List.map (фун card ->
                    якщо card.Id = index
                    тоді { card із Selected = true }
                    інакше card)
            { model із Cards = cards; FirstSelection = Some index }
        | Some id, None when id = index -> model
        | Some id, None when cardsEqual id index (model.Cards) ->
            нехай cards =
                model.Cards
                |> List.map (фун card ->
                    якщо card.Id = index || card.Id = id
                    тоді { card із Selected = true; MatchFound = true }
                    інакше card)
            { model із Cards = cards; FirstSelection = None; SecondSelection = None }
        | Some id, None when id <> index ->
            нехай cards =
                model.Cards
                |> List.map (фун card ->
                    якщо card.Id = index
                    тоді { card із Selected = true }
                    інакше card)
            { Cards = cards; FirstSelection = Some id; SecondSelection = Some index }
        | Some id, Some id' when cardsEqual id' index (model.Cards) ->
            нехай cards =
                model.Cards
                |> List.map (фун card ->
                    якщо (card.Id = id && not card.MatchFound)
                    тоді { card із Selected = false }
                    інякщо (card.Id = id' || card.Id = index)
                    тоді { card із Selected = true; MatchFound = true }
                    інакше card)
            { model із Cards = cards; FirstSelection = None; SecondSelection = None }
        | Some id, Some id' ->
            нехай cards =
                model.Cards
                 |> List.map (фун card ->
                      якщо (card.Id = id || card.Id = id') && not card.MatchFound
                      тоді { card із Selected = false }
                      інякщо card.Id = index
                      тоді { card із Selected = true }
                      інакше card
                 )
            { Cards = cards; FirstSelection = Some index; SecondSelection = None }
        | _, _ -> failwith "Cannot happen :)"
    | NoOp -> model

// View
нехай cardClicked (card: Card) dispatch  =
   якщо not (card.MatchFound) && not (card.Selected)
   тоді dispatch (SelectCard card.Id)
   інакше dispatch (NoOp)

нехай viewCard (card: Card) dispatch =
    div
      [ classList [ "card-container", true; "співстав-found", card.MatchFound]
        OnClick (фун _ -> cardClicked card dispatch) ]
      [ img [ Src (якщо card.Selected тоді card.ImgUrl інакше origin + "img/memory/fable.jpg") ] ]

нехай view model dispatch =
    якщо gameCleared model тоді
        h1
          [ Class "winner centered"
            Style [ Padding 20; Width "500px" ]
            OnClick (фун _ -> dispatch StartNewGame ) ]
          [ str "You win, Click me to play again" ]
    інакше
        div [ Class "container centered"
              Style [ Width "500px" ] ]
            [ для card у model.Cards -> viewCard card dispatch ]

// App
Program.mkSimple initialModel update view
|> Program.withReactSynchronous "elmish-app"
|> Program.run
