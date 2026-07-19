модуль Elmish.TodoMVC

(**
 TodoMVC app ported from Elm.
 You can find more info about Emish architecture and samples at https://elmish.github.io/
 NOTE: The API у Fable's REPL may differ from Fable.Elmish & Fable.React nuget libraries.
       The generated JS code won't be as optimized as when using dotnet-fable.
*)

відкрити Fable.Core
відкрити Fable.React
відкрити Fable.React.Props
відкрити Browser.Types
відкрити Browser
відкрити Elmish
відкрити Elmish.React

нехай [<Literal>] ESC_KEY = 27.
нехай [<Literal>] ENTER_KEY = 13.

тип WhatIsVisible =
   | All
   | Active
   | Completed

нехай toStr v =
    співстав v із
    | All -> "All"
    | Active -> "Active"
    | Completed -> "Completed"

// MODEL
тип Entry =
    { description : string
      completed : bool
      editing : bool
      id : int }

// The full application state з our todo app.
тип Model =
    { entries : Entry list
      field : string
      uid : int
      visibility : WhatIsVisible }

нехай emptyModel () =
    { entries = []
      visibility = All
      field = ""
      uid = 0 }

нехай newEntry desc id =
  { description = desc
    completed = false
    editing = false
    id = id }

// UPDATE

(** Users з our app can trigger messages by clicking and typing. These
messages are fed into the `update` функція as they occur, letting us react
to them.
*)
тип Msg =
    | Failure з string
    | UpdateField з string
    | EditingEntry з int*bool
    | UpdateEntry з int*string
    | Add
    | Delete з int
    | DeleteComplete
    | Check з int*bool
    | CheckAll з bool
    | ChangeVisibility з WhatIsVisible

// How we update our Model on a given Msg?
нехай update (msg:Msg) (model:Model) =
    співстав msg із
    | Failure err ->
        JS.console.error(err)
        model

    | Add ->
        нехай xs = якщо System.String.IsNullOrEmpty model.field тоді
                    model.entries
                 інакше
                    model.entries @ [newEntry model.field model.uid]
        { model із
            uid = model.uid + 1
            field = ""
            entries = xs }

    | UpdateField str ->
      { model із field = str }

    | EditingEntry (id,isEditing) ->
        нехай updateEntry t =
          якщо t.id = id тоді { t із editing = isEditing } інакше t
        { model із entries = List.map updateEntry model.entries }

    | UpdateEntry (id,task) ->
        нехай updateEntry t =
          якщо t.id = id тоді { t із description = task } інакше t
        { model із entries = List.map updateEntry model.entries }

    | Delete id ->
        { model із entries = List.filter (фун t -> t.id <> id) model.entries }

    | DeleteComplete ->
        { model із entries = List.filter (фун t -> not t.completed) model.entries }

    | Check (id,isCompleted) ->
        нехай updateEntry t =
          якщо t.id = id тоді { t із completed = isCompleted } інакше t
        { model із entries = List.map updateEntry model.entries }

    | CheckAll isCompleted ->
        нехай updateEntry t = { t із completed = isCompleted }
        { model із entries = List.map updateEntry model.entries }

    | ChangeVisibility visibility ->
        { model із visibility = visibility }

нехай onEnter msg dispatch =
    OnKeyDown (фун ev ->
        якщо ev.keyCode = ENTER_KEY тоді
            dispatch msg)

нехай targetValue (ev: Event) =
    (ev.target :?> HTMLInputElement).value

нехай viewInput (model:string) dispatch =
    header [ Class "header" ] [
        h1 [] [ str "todos" ]
        input [
            Class "новий-todo"
            Placeholder "What needs to be done?"
            Value model
            onEnter Add dispatch
            OnChange (фун ev ->
                targetValue ev |> UpdateField |> dispatch)
            AutoFocus true
        ]
    ]

нехай classList classes =
    classes
    |> List.fold (фун complete -> функція | (name,true) -> complete + " " + name | _ -> complete) ""
    |> Class

нехай viewEntry todo dispatch =
  li
    [ classList [ ("completed", todo.completed); ("editing", todo.editing) ]]
    [ div
        [ Class "view" ]
        [ input
            [ Class "toggle"
              Type "checkbox"
              Checked todo.completed
              OnChange (фун _ -> Check (todo.id,(not todo.completed)) |> dispatch) ]
          label
            [ OnDoubleClick (фун _ -> EditingEntry (todo.id,true) |> dispatch) ]
            [ str todo.description ]
          button
            [ Class "destroy"
              OnClick (фун _-> Delete todo.id |> dispatch) ]
            []
        ]
      input
        [ Class "edit"
          Value todo.description
          Name "title"
          Id ("todo-" + (string todo.id))
          OnInput (фун ev -> UpdateEntry (todo.id, targetValue ev) |> dispatch)
          OnBlur (фун _ -> EditingEntry (todo.id,false) |> dispatch)
          onEnter (EditingEntry (todo.id,false)) dispatch ]
    ]

нехай viewEntries visibility entries dispatch =
    нехай isVisible todo =
        співстав visibility із
        | Completed -> todo.completed
        | Active -> not todo.completed
        | All -> true

    нехай allCompleted =
        List.forall (фун t -> t.completed) entries

    нехай cssVisibility =
        якщо List.isEmpty entries тоді "hidden" інакше "visible"

    section
      [ Class "main"
        Style [ Visibility cssVisibility ]]
      [ input
          [ Class "toggle-all"
            Type "checkbox"
            Name "toggle"
            Checked allCompleted
            OnChange (фун _ -> CheckAll (not allCompleted) |> dispatch)]
        label
          [ HtmlFor "toggle-all" ]
          [ str "Mark all as complete" ]
        ul
          [ Class "todo-list" ]
          (entries
           |> List.filter isVisible
           |> List.map (фун i -> viewEntry i dispatch)) ]

// VIEW CONTROLS AND FOOTER
нехай visibilitySwap uri visibility actualVisibility dispatch =
  li
    [ OnClick (фун _ -> ChangeVisibility visibility |> dispatch) ]
    [ a [ Href uri
          classList ["selected", visibility = actualVisibility] ]
          [ str (toStr visibility) ] ]

нехай viewControlsFilters visibility dispatch =
  ul
    [ Class "filters" ]
    [ visibilitySwap "#/" All visibility dispatch
      str " "
      visibilitySwap "#/active" Active visibility dispatch
      str " "
      visibilitySwap "#/completed" Completed visibility dispatch ]

нехай viewControlsCount entriesLeft =
  нехай item =
      якщо entriesLeft = 1 тоді " item" інакше " items"

  span
      [ Class "todo-count" ]
      [ strong [] [ str (string entriesLeft) ]
        str (item + " left") ]

нехай viewControlsClear entriesCompleted dispatch =
  button
    [ Class "clear-completed"
      Hidden (entriesCompleted = 0)
      OnClick (фун _ -> DeleteComplete |> dispatch)]
    [ str ("Clear completed (" + (string entriesCompleted) + ")") ]

нехай viewControls visibility entries dispatch =
  нехай entriesCompleted =
      entries
      |> List.filter (фун t -> t.completed)
      |> List.length

  нехай entriesLeft =
      List.length entries - entriesCompleted

  footer
      [ Class "footer"
        Hidden (List.isEmpty entries) ]
      [ viewControlsCount entriesLeft
        viewControlsFilters visibility dispatch
        viewControlsClear entriesCompleted dispatch ]

нехай infoFooter =
  footer [ Class "info" ]
    [ p []
        [ str "Double-click to edit a todo" ]
      p []
        [ str "Ported from Elm by "
          a [ Href "https://github.com/et1975" ] [ str "Eugene Tolmachev" ]]
      p []
        [ str "Part з "
          a [ Href "http://todomvc.com" ] [ str "TodoMVC" ]]
    ]

нехай view model dispatch =
  div
    [ Class "todomvc-wrapper"]
    [ section
        [ Class "todoapp" ]
        [ viewInput model.field dispatch
          viewEntries model.visibility model.entries dispatch
          viewControls model.visibility model.entries dispatch ]
      infoFooter ]

// App
Program.mkSimple emptyModel update view
|> Program.withReactSynchronous "todoapp"
|> Program.withConsoleTrace
|> Program.run
