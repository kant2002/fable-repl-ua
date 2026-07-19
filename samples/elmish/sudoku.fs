модуль Sudoku

відкрити System.Collections.Generic
відкрити Fable.React
відкрити Fable.React.Props
відкрити Elmish
відкрити Elmish.React

тип Box = int
тип Sudoku = Box array array

нехай rows = id
нехай cols (sudoku:Sudoku) =
    sudoku
    |> Array.mapi (фун a row -> row |> Array.mapi (фун b cell -> sudoku.[b].[a]))

нехай getBoxIndex count row col =
   нехай n = row/count
   нехай m = col/count
   n * count + m

нехай boxes (sudoku:Sudoku) =
    нехай l = sudoku |> Array.length
    нехай d = float l |> System.Math.Sqrt |> int
    нехай list = новий List<_>()
    для a у 0..l - 1 зробити
        list.Add(новий List<_>())

    для a у 0..(l - 1) зробити
        для b у 0..(l - 1) зробити
            list.[getBoxIndex d a b].Add(sudoku.[a].[b])

    list
      |> Seq.map Seq.toArray

нехай toSudoku x : Sudoku =
    x
    |> Seq.map Seq.toArray
    |> Seq.toArray

нехай allUnique numbers =
    нехай set = новий HashSet<_>()
    numbers
    |> Seq.filter ((<>) 0)
    |> Seq.forall set.Add

нехай solvable sudoku =
    rows sudoku
    |> Seq.append (cols sudoku)
    |> Seq.append (boxes sudoku)
    |> Seq.forall allUnique

нехай replaceAtPos (x:Sudoku) row col newValue :Sudoku =
    [| для a у 0..(Array.length x - 1) ->
        [| для b у 0..(Array.length x - 1) ->
            якщо a = row && b = col тоді newValue інакше x.[a].[b] |] |]

нехай rec substitute row col (x:Sudoku) =
    нехай a,b = якщо col >= Array.length x тоді row+1,0 інакше row,col
    якщо a >= Array.length x тоді seq { поступатися x } інакше
    якщо x.[a].[b] = 0 тоді
        [1..Array.length x]
            |> Seq.map (replaceAtPos x a b)
            |> Seq.filter solvable
            |> Seq.collect (substitute a (b+1))
     інакше substitute a (b+1) x

нехай getFirstSolution = substitute 0 0 >> Seq.head

нехай puzzle =
    [[0; 0; 8;  3; 0; 0;  6; 0; 0]
     [0; 0; 4;  0; 0; 0;  0; 1; 0]
     [6; 7; 0;  0; 8; 0;  0; 0; 0]

     [0; 1; 6;  4; 3; 0;  0; 0; 0]
     [0; 0; 0;  7; 9; 0;  0; 2; 0]
     [0; 9; 0;  0; 0; 0;  4; 0; 1]

     [0; 0; 0;  9; 1; 0;  0; 0; 5]
     [0; 0; 3;  0; 5; 0;  0; 0; 2]
     [0; 5; 0;  0; 0; 0;  0; 7; 4]]
    |> toSudoku

нехай init() = puzzle

тип Model = Sudoku

тип Msg =
| Reset
| Solve

нехай update (msg:Msg) (model:Model) =
    співстав msg із
    | Reset -> puzzle
    | Solve -> getFirstSolution model

нехай tableRow xs = tr [] [ для x у xs -> td [] [x] ]


нехай view (model:Model) dispatch =
    div
      []
      [ div
          [ Class "calc" ]
          [ table []
                [ для row у model ->
                    tableRow [
                        для n у row ->
                            div [ Class "digit" ] [
                                str (якщо n = 0 тоді "" інакше string n) ] ] ]
          ]
        br []
        div
          [ Class "controls" ]
          [ div
              [ Class "op-button"
                OnClick (фун _ -> dispatch Reset) ]
              [ str "Reset" ]
            div
              [ Class "op-button"
                OnClick (фун _ -> dispatch Solve) ]
              [ str "Solve" ]]]

// App
Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.run
