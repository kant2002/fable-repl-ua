модуль Elmish.SimpleInput

(**
Minimal application showing how to use Elmish
You can find more info about Emish architecture and samples at https://elmish.github.io/
*)

відкрити Fable.Core.JsInterop
відкрити Fable.React
відкрити Fable.React.Props
відкрити Elmish
відкрити Elmish.React

// MODEL

тип Model =
    { Value : string }

тип Msg =
    | ChangeValue з string

нехай init () = { Value = "" }, Cmd.none

// UPDATE

нехай update (msg:Msg) (model:Model) =
    співстав msg із
    | ChangeValue newValue ->
        { model із Value = newValue }, Cmd.none

// VIEW (rendered із React)

нехай view model dispatch =
    div [ Class "main-container" ]
        [ input [ Class "input"
                  Value model.Value
                  OnChange (фун ev -> ev.target?value |> string |> ChangeValue |> dispatch) ]
          span [ ]
            [ str "Hello, "
              str model.Value
              str "!" ] ]

// App
Program.mkProgram init update view
|> Program.withConsoleTrace
|> Program.withReactSynchronous "elmish-app"
|> Program.run
