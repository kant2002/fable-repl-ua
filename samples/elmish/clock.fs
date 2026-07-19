модуль Elmish.Clock

(**
 Timer as a source з events із an SVG clock, by Zaid Ajaj.
 You can find more info about Emish architecture and samples at https://elmish.github.io/
*)

відкрити System
відкрити Fable.React
відкрити Fable.React.Props
відкрити Browser
відкрити Elmish
відкрити Elmish.React
тип SVG = SVGAttr

// Types
тип Model = CurrentTime з DateTime
тип Messages = Tick з DateTime

// State
нехай initialState() =
    CurrentTime DateTime.Now, Cmd.none

нехай update (Tick next) (CurrentTime _time) =
    CurrentTime next, Cmd.none

нехай timerTick dispatch =
    window.setInterval(фун _ ->
        dispatch (Tick DateTime.Now)
    , 1000) |> ignore

// View
тип Time =
    | Hour з int
    | Minute з int
    | Second з int

нехай clockHand time color width length =
    нехай clockPercentage =
        співстав time із
        | Hour n -> (float n) / 12.0
        | Second n -> (float n) / 60.0
        | Minute n -> (float n) / 60.0
    нехай angle = 2.0 * Math.PI * clockPercentage
    нехай handX = (50.0 + length * cos (angle - Math.PI / 2.0))
    нехай handY = (50.0 + length * sin (angle - Math.PI / 2.0))
    line [ X1 "50"
           Y1 "50"
           X2 handX
           Y2 handY
           // Qualify these props to avoid name collision із CSSProp
           SVG.Stroke color
           SVG.StrokeWidth width ] []

нехай handTop n color length fullRound =
    нехай revolution = float n
    нехай angle = 2.0 * Math.PI * (revolution / fullRound)
    нехай handX = (50.0 + length * cos (angle - Math.PI / 2.0))
    нехай handY = (50.0 + length * sin (angle - Math.PI / 2.0))
    circle [ Cx handX
             Cy handY
             R "2"
             SVG.Fill color ] []

нехай view (CurrentTime time) dispatch =
    svg
      [ ViewBox "0 0 100 100"
        SVG.Width "350px" ]
      [ circle
          [ Cx "50"
            Cy "50"
            R "45"
            SVG.Fill "#0B79CE" ] []
        // Hours
        clockHand (Hour time.Hour) "lightgreen" "2" 25.0
        handTop time.Hour "lightgreen" 25.0 12.0
        // Minutes
        clockHand (Minute time.Minute) "white" "2" 35.0
        handTop time.Minute "white" 35.0 60.0
        // Seconds
        clockHand (Second time.Second) "#023963" "1" 40.0
        handTop time.Second "#023963" 40.0 60.0
        // circle у the center
        circle
          [ Cx "50"
            Cy "50"
            R "3"
            SVG.Fill "#0B79CE"
            SVG.Stroke "#023963"
            SVG.StrokeWidth 1.0 ] []
      ]

// App
Program.mkProgram initialState update view
|> Program.withSubscription (фун _ -> Cmd.ofSub timerTick)
|> Program.withReactSynchronous "elmish-app"
|> Program.run
