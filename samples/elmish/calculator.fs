модуль Elmish.Calculator

(**
 Calculator sample, by Zaid Ajaj.
 You can find more info about Emish architecture and samples at https://elmish.github.io/
*)

відкрити Fable.React
відкрити Fable.React.Props
відкрити Elmish
відкрити Elmish.React

// Types
тип Input =
    | Const з int
    | Plus
    | Minus
    | Times
    | Div
    | Clear
    | Equals

тип Model =  InputStack з Input list

тип Messages = PushInput з Input

// State

/// Active pattern that matches із an operation
нехай (|Operation|_|) = функція
    | Plus -> Some Plus
    | Minus -> Some Minus
    | Times -> Some Times
    | Div -> Some Div
    | _ -> None

/// Given a model, calculate the answer
нехай solve (state : Input list) =
  співстав state із
  | [Const x; Operation op; Const y] ->
      співстав op із
      | Plus -> Some (x + y)
      | Minus -> Some (x - y)
      | Times -> Some (x * y)
      | Div when y = 0 -> None // division by zero not allowed
      | Div -> Some (x / y)
      | _ -> None
  | _ -> None


/// Given two integers, append the second on the first
/// concatInts 3 5 -> 35
/// concatInts 1 1 -> 11
нехай concatInts x y = x * 10 + y * (sign x)

нехай initialState() : Model = InputStack []

/// Given the input message and the state з the app, calculate the next state. This is known as the update функція
нехай update (PushInput input) (InputStack xs)  =
    якщо input = Clear тоді InputStack []
    інакше
    співстав xs із
    | [] ->
        співстав input із
        | Minus -> InputStack [Minus]
        | Operation op -> InputStack [ ]
        | Equals -> InputStack []
        | _ -> InputStack [input]
    | [Minus] ->
        співстав input із
        | Const x -> InputStack [ Const (-x) ]
        | _ -> InputStack xs
    | [Const x] ->
        співстав input із
        | Const y -> InputStack [Const (concatInts x y)]
        | Operation op -> InputStack [Const x; op]
        | _ -> InputStack xs
    | [Const x; Operation op] ->
        співстав input із
        | Const y -> InputStack [Const x; op; Const y] // push Const y to stack
        | Minus when op = Minus -> InputStack [Const x; Plus] // Minus Minus = Plus
        | Minus -> InputStack [Const x; op; Minus]
        | Operation otherOp -> InputStack [Const x; otherOp] // replace op із otherOp
        | _ -> InputStack xs // зробити nothing
    | [Const x; Operation op; Minus] ->
        співстав input із
        | Const y -> InputStack [Const x; op; Const (-y)]
        | _ -> InputStack xs
    | [Const x; Operation op; Const y] ->
        співстав input із
        | Const y' -> InputStack [Const x; op; Const (concatInts y y')]
        | Equals ->
            співстав solve xs із
            | Some answer -> InputStack [Const answer]
            | None -> InputStack xs
        | Operation op ->
            співстав solve xs із
            | Some answer -> InputStack [Const answer; op]
            | None -> InputStack xs
        | _ -> InputStack xs
    | _ -> InputStack xs

// View
нехай inputToString = функція
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Div -> "/"
    | Equals -> "="
    | Clear -> "CE"
    | Const n -> string n

нехай modelToString (InputStack xs) =
    xs
    |> Seq.map inputToString
    |> String.concat " "

нехай digitBtn n dispatch =
    нехай message = PushInput (Const n)
    div
      [ Class "calculator-button is-digit"; OnClick (фун _ -> dispatch message) ]
      [ str (string n) ]

нехай operationBtn input dispatch =
    нехай message = PushInput input
    div
        [ Class "calculator-button is-op"; OnClick (фун _ -> dispatch message) ]
        [ str (inputToString input) ]

нехай tableRow xs = tr [] [ для x у xs -> td [] [x] ]

нехай view model dispatch =
    нехай digit n = digitBtn n dispatch
    нехай opBtn op = operationBtn op dispatch
    div
      [ Class "calculator" ]
      [ h2
          [ Style [ Height 40; MarginLeft 20 ] ]
          [ str (modelToString model) ]
        br []
        table []
            [ tableRow [digit 1; digit 2; digit 3; opBtn Plus]
              tableRow [digit 4; digit 5; digit 6; opBtn Minus]
              tableRow [digit 7; digit 8; digit 9; opBtn Times]
              tableRow [opBtn Input.Clear; digit 0; opBtn Equals; opBtn Div] ] ]

// App
Program.mkSimple initialState update view
|> Program.withReactSynchronous "elmish-app"
|> Program.run
