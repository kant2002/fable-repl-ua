// Ф# Колонія мурах. Редакція Фабле

// Ported from: https://github.com/robertpi/F--Ant-Colony/ which is a folk з: https://github.com/Rickasaurus/F--Ant-Colony

// Original notice:

//
// This is Richard Minerich's F# Ant Colony Silverlight Ediiton
// Visit my Blog at http://RichardMinerich.com
// This code is free to be used для anything you like as long as I am properly acknowledged.
//
// The basic Silverlight used here is based on Phillip Trelford's Missile Command Example
// http://www.trelford.com/blog/post/MissileCommand.aspx
//

модуль Мурахи

відкрити Fable.Core
відкрити Fable.Core.JsInterop
відкрити Browser.Types
відкрити Browser

модуль Типи =

    нехай xSize = 50
    нехай ySize = 50
    нехай розмірГнізда = 5
    нехай maxTotalFoodPerSquare = 200
    нехай minGeneratedFoodPerSquare = 20
    нехай maxGeneratedFoodPerSquare = 100
    нехай максЇжіМурахаМожеНести = 5
    нехай chanceOfFood = 0.04

    нехай maxCellPheromoneQuantity = 255
    нехай maxAntDropPheromoneQunatity = 50
    нехай pheromoneDispersalRate = 1

    нехай percentFoodToWin = 0.5
    нехай maxWorldCycles = 1500

    тип УІД = { X: int; Y: int }

    нехай уід (x, y) = { X = x; Y = y}

    тип КолірМурахи =
        | Чорний
        | Червоний

    тип ТипКлітиниСвіту =
            | FieldCell
            | КлітинаГнізда з КолірМурахи

    тип Мураха =
        { Колір : КолірМурахи
          FoodCarried : int }
        із
            член м.IsFullOfFood = м.FoodCarried >= максЇжіМурахаМожеНести
            член м.HasFood = м.FoodCarried > 0
            член м.MaxPheromonesToDrop = maxAntDropPheromoneQunatity

    and КлітинаСвіту =
        { Ід : УІД
          Їжа : int
          Мураха : option<Мураха>
          ТипКлітини : ТипКлітиниСвіту
          Феромони : Map<КолірМурахи, int> }
        із
            член к.IsFullOfFood = к.Їжа >= maxTotalFoodPerSquare
            член к.ЄЇжа = к.Їжа > 0
            член к.МіститьМураху = к.Мураха.IsSome
            член к.ЄФеромони color = not (к.Феромони.[color] = 0)
            член к.MaxPheromones = maxCellPheromoneQuantity
            член к.MaxFood = maxTotalFoodPerSquare

    and Світ = Map<УІД, КлітинаСвіту>

    and ДіяМурахи =
        | Нічого
        | Перемістити з КлітинаСвіту
        | ВзятиЇжу з КлітинаСвіту
        | КинутиЇжу з КлітинаСвіту
        | КинутиФеромон з КлітинаСвіту * int

    тип Гніздо(ix, iy, розмірх, розміру) =
        член internal t.МінХ = ix
        член internal t.МінУ = iy
        член internal t.МаксХ = ix + розмірх
        член internal t.МаксУ = iy + розміру
        член internal t.ЧиУМежах x y = x >= t.МінХ && x <= t.МаксХ && y >= t.МінУ && y <= t.МаксУ
        член t.Дістанція клітина =
                нехай cx, cy = t.МінХ + ((t.МаксХ - t.МінХ) / 2), t.МінУ + ((t.МаксУ - t.МінУ) / 2)
                нехай x, y = клітина.Ід.X, клітина.Ід.Y
                нехай ступінь x = x * x
                sqrt (ступінь(double cx - double x) + ступінь(double cy - double y))
        член t.КількістьЇжі (світ: Світ) =
                Map.fold (фун s (к: УІД) зз -> якщо t.ЧиУМежах к.X к.Y тоді s + зз.Їжа інакше s) 0 світ


    тип ІПоведінкаМурахи =
        абстрактний член Ім'я : string
        абстрактний член Поведінка : Мураха -> КлітинаСвіту -> КлітинаСвіту list -> Гніздо -> ДіяМурахи

    тип ЗмінаСвіту = Світ -> Світ

модуль Помічники =

    відкрити System
    відкрити System.Reflection

    модуль Array =
        нехай randomPermute a =
            нехай n = Array.length a
            якщо n > 0 тоді
                нехай rand = новий Random()
                нехай rec aux = функція
                    | 0 -> a
                    | k ->
                        нехай i = rand.Next(k+1)
                        нехай tmp = a.[i]
                        a.[i] <- a.[k]
                        a.[k] <- tmp
                        aux (k-1)
                aux (n-1)
            інакше a

    модуль Seq =
        нехай randomPermute a =
            a |> Seq.toArray |> Array.randomPermute |> Array.toSeq

    модуль List =

        нехай приватний r = Random(int DateTime.Now.Ticks)
        нехай random l =
            нехай index = r.Next(0, List.length l) у
                l.[index]

модуль Світ =

    відкрити System

    відкрити Типи
    відкрити Помічники

    нехай ГніздоЧорнихМурах = новий Гніздо( 0, 0, розмірГнізда - 1, розмірГнізда - 1 )
    нехай ГніздоЧервонихМурах = новий Гніздо( 1 + xSize - розмірГнізда, 1 + ySize - розмірГнізда, розмірГнізда - 1, розмірГнізда - 1)

    нехай (|УЧорномуГнізді|УЧервономуГнізді|Ніде|) (x,y) =
        якщо ГніздоЧорнихМурах.ЧиУМежах x y тоді УЧорномуГнізді
        інякщо ГніздоЧервонихМурах.ЧиУМежах x y тоді УЧервономуГнізді
        інакше Ніде

    нехай взятиГніздоМурахи мураха =
        співстав мураха.Колір із
        | КолірМурахи.Чорний -> ГніздоЧорнихМурах
        | КолірМурахи.Червоний -> ГніздоЧервонихМурах

    нехай пустийНабірФермнів =
        seq { нехай кольори = [| КолірМурахи.Чорний; КолірМурахи.Червоний |]
              для колір у кольори зробити
                поступатися колір, 0 }
        |> Map.ofSeq

    нехай клітинаЗаЗамовчанням id = {Ід = id; Їжа = 0; Мураха = None; ТипКлітини = FieldCell; Феромони = пустийНабірФермнів }
    нехай чорнаМурахаЗаЗамовчанням = Some { Колір = КолірМурахи.Чорний; FoodCarried = 0 }
    нехай червонаМурахаЗаЗамовчанням = Some { Колір = КолірМурахи.Червоний; FoodCarried = 0 }

    нехай buildWorldInitialWorld () =
        нехай слч = новий System.Random() у
            seq { для x у 0 .. xSize зробити
                    для y у 0 .. ySize зробити
                        нехай уід = уід (x, y)
                        нехай клітиназазамовчанням = клітинаЗаЗамовчанням уід
                        співстав x, y із
                        | УЧорномуГнізді -> поступатися уід, { клітиназазамовчанням із Мураха = чорнаМурахаЗаЗамовчанням; ТипКлітини = КлітинаГнізда(КолірМурахи.Чорний) }
                        | УЧервономуГнізді ->   поступатися уід, { клітиназазамовчанням із Мураха = червонаМурахаЗаЗамовчанням; ТипКлітини = КлітинаГнізда(КолірМурахи.Червоний) }
                        | Ніде ->     якщо chanceOfFood > слч.NextDouble()
                                            тоді поступатися уід, { клітиназазамовчанням із Їжа = слч.Next(minGeneratedFoodPerSquare, maxGeneratedFoodPerSquare) }
                                            інакше поступатися уід, клітиназазамовчанням
                }
            |> Map.ofSeq

    нехай getAntViews (світ: Світ) =
        нехай взятиКлітинуСвіту x y = Map.tryFind (уід (x,y)) світ
        нехай worldFold state (uid: УІД) cell =
                нехай x, y = (uid.X, uid.Y)
                співстав cell.Мураха із
                | None -> state
                | Some(ant) ->
                    нехай visibleCells = [ взятиКлітинуСвіту x (y - 1); взятиКлітинуСвіту x (y + 1); взятиКлітинуСвіту (x - 1) y; взятиКлітинуСвіту (x + 1) y ]
                                        |> List.choose id
                    state @ [ant, cell, visibleCells, взятиГніздоМурахи ant]
        Map.fold worldFold [] світ

    нехай взятиДіїМурах (поведінкаЧорних: ІПоведінкаМурахи) (поведінкаЧервоних: ІПоведінкаМурахи) (views: (Мураха * КлітинаСвіту * КлітинаСвіту list * Гніздо) list) =
        нехай взятиПоведінкуМурахи мураха =
            співстав мураха.Колір із
            | КолірМурахи.Чорний -> поведінкаЧорних
            | КолірМурахи.Червоний -> поведінкаЧервоних
        нехай transformView (мураха, клітина, antView, гніздо) =
            нехай поведінка = взятиПоведінкуМурахи мураха у
            клітина, поведінка.Поведінка мураха клітина antView гніздо
        List.map transformView views

    нехай побудуватиТранзакцію (очікуваніКлітини: КлітинаСвіту list) дії =
        нехай предікат (світ: Світ) =
            List.forall (фун (cell: КлітинаСвіту) -> (Map.find cell.Ід світ) = cell) очікуваніКлітини
        нехай дія (iworld: Світ) =
            List.fold (фун (cworld: Світ) (ід, дія) -> Map.add ід (дія cworld.[ід]) cworld) iworld дії
        предікат, дія

    нехай getWorldChangeTransactions дії =
        seq { для source, дія у дії зробити
                нехай мураха = Option.get source.Мураха
                співстав дія із
                | Нічого -> ()
                | Перемістити (ціль) ->
                    якщо Option.isSome ціль.Мураха тоді ()
                    інакше поступатися побудуватиТранзакцію
                                    [ source; ціль ]
                                    [ source.Ід, (фун стараклітина -> { стараклітина із Мураха = None });
                                        ціль.Ід, (фун oldtarget -> { oldtarget із Мураха = source.Мураха }) ]
                | ВзятиЇжу (ціль) ->
                    якщо ціль.Їжа <= 0 тоді ()
                    інакше
                        нехай foodToGet = min (ціль.Їжа) (максЇжіМурахаМожеНести - мураха.FoodCarried)
                        поступатися побудуватиТранзакцію
                                    [ source; ціль ]
                                    [ ціль.Ід, (фун oldtarget -> { oldtarget із Їжа = oldtarget.Їжа - foodToGet });
                                        source.Ід, (фун oldcell -> { oldcell із Мураха = Some { мураха із FoodCarried = мураха.FoodCarried + foodToGet } } ) ]
                | КинутиЇжу (ціль) ->
                    якщо ціль.Їжа >= maxTotalFoodPerSquare тоді ()
                    інакше
                        нехай foodToDrop = min (maxTotalFoodPerSquare - ціль.Їжа) (мураха.FoodCarried)
                        нехай transaction =
                            побудуватиТранзакцію
                                    [ source; ціль ]
                                    [ ціль.Ід, (фун oldtarget -> { oldtarget із Їжа = oldtarget.Їжа + foodToDrop });
                                        source.Ід, (фун oldcell -> { source із Мураха = Some { мураха із FoodCarried = мураха.FoodCarried - foodToDrop } }) ]
                        поступатися transaction
                | КинутиФеромон (ціль, кількість) ->
                    нехай новеЗначення = max (ціль.Феромони.[мураха.Колір] + кількість) maxCellPheromoneQuantity
                    поступатися побудуватиТранзакцію
                                [ ціль ]
                                [ ціль.Ід, (фун oldtarget -> { oldtarget із Феромони = oldtarget.Феромони.Add(мураха.Колір, новеЗначення ) } ) ] }

    нехай деградуватиФеромони (світ: Світ) =
        світ
        |> Map.map (фун уід клітина -> { клітина із Феромони = клітина.Феромони |> Map.map (фун ключ кількість -> max (кількість - 1) 0) } )

    нехай applyWorldTransactions (старийСвіт: Світ) зміни =
        нехай foldAction (світ: Світ) (пред, дія) =
            якщо пред світ
            тоді дія світ
            інакше світ
        Seq.fold foldAction старийСвіт зміни

    нехай уід2ху (уід: УІД) = уід.X, уід.Y

    нехай worldCycle bPlayer rPlayer світ : Світ =
        світ
        |> getAntViews
        |> взятиДіїМурах bPlayer rPlayer
        |> Seq.randomPermute
        |> getWorldChangeTransactions
        |> applyWorldTransactions світ
        |> деградуватиФеромони

модуль Полотно =

    // Get the canvas context для drawing
    нехай полотно = document.getElementsByTagName("canvas").[0] :?> HTMLCanvasElement
    нехай контекст = полотно.getContext_2d()

    // Format RGB color as "rgb(r,g,b)"
    нехай ($) s n = s + n.ToString()
    нехай rgb r g b = "rgb(" $ r $ "," $ g $ "," $ b $ ")"

    // Fill rectangle із given color
    нехай filled (колір: string) rect =
        нехай кнт = контекст
        кнт.fillStyle <- !^ колір
        кнт.fillRect rect

    нехай намалюватиПляму (колір: string) розмір (x, y) =
        контекст.beginPath()
        контекст.arc(x, y, розмір, 0., 2. * System.Math.PI, false )
        контекст.fillStyle <- !^ колір
        контекст.fill()

    нехай взятиРозміриВікна () =
        полотно.width, полотно.height


    нехай зображення (src:string) =
        нехай зображення = document.getElementsByTagName("img").[0] :?> HTMLImageElement
        якщо зображення.src.IndexOf(src) = -1 тоді зображення.src <- src
        зображення

    нехай updateInput ім'я текст =
        нехай image = document.getElementsByName(ім'я).[0] :?> HTMLDivElement
        image.innerHTML <- текст
        image


модуль Сімуляція =
    відкрити Типи
    відкрити Світ
    відкрити Полотно

    нехай намалюватиМураху x y колірМурахи =
        нехай колір =
            співстав колірМурахи із
            | КолірМурахи.Чорний -> rgb 0 0 0
            | КолірМурахи.Червоний -> rgb 255 0 0
        намалюватиПляму колір 4. (x, y)

    нехай намалюватиЇжу їжа x y =
        нехай радіус = ((float їжа / float maxTotalFoodPerSquare) * 3.) + 1.
        нехай колір = rgb 0 255 0
        намалюватиПляму колір радіус (x, y)

    нехай зробитиГрадіент кількість max =
        нехай inverseGrediant = 1. - (float кількість / float max)
        нехай levelDiff = 200. - 111. // difference between the "full pheromone color and background"
        levelDiff * inverseGrediant
    нехай намалюватиФеромони x y колірМурахи amount =
        нехай opacityFudge = зробитиГрадіент amount maxCellPheromoneQuantity
        нехай рівень = int opacityFudge + 111
        // console.log(sprintf "level: %d" level)
        нехай колір =
            співстав колірМурахи із
            | КолірМурахи.Чорний -> rgb рівень рівень рівень
            | КолірМурахи.Червоний -> rgb рівень opacityFudge рівень
        намалюватиПляму колір 4. (x, y)

    нехай намалюватиОновлення (ширина, висота) (світ: Світ) =
        нехай оновитиКлітину уід клітина =
            нехай wm, hm = ширина / float (xSize + 1), висота / float (ySize + 1)
            нехай offset x y = (x + 0.5) * wm, (y + 0.5) * hm
            нехай x, y = уід2ху уід
            нехай ox, oy = offset (float x) (float y)
            клітина.Феромони |> Map.iter (фун колір кількість -> якщо кількість > 0 тоді намалюватиФеромони ox oy колір кількість)
            якщо клітина.Їжа > 0 тоді намалюватиЇжу клітина.Їжа ox oy
            якщо клітина.Мураха.IsSome тоді намалюватиМураху ox oy клітина.Мураха.Value.Колір
        світ
        |> Map.iter оновитиКлітину


модуль HardishAI =

    відкрити Помічники
    відкрити Типи

    нехай слч = System.Random(int System.DateTime.Now.Ticks)

    тип ТестоваПоведінкаМурахи() =
        інтерфейс ІПоведінкаМурахи із
            член x.Ім'я = "Rick's Hardish"
            член x.Поведінка я тут місця гніздо =

                нехай цеМійДім вузел = вузел.ТипКлітини = ТипКлітиниСвіту.КлітинаГнізда(я.Колір)
                нехай locationsWithoutAnts = місця |> List.filter  (фун вузел -> вузел.Мураха = None)

                нехай (|МаєЇжу|МаєМаксимальноЇжу|НемаєЇжі|) (мураха: Мураха) =
                    якщо мураха.FoodCarried = 0 тоді НемаєЇжі
                    інякщо мураха.FoodCarried = максЇжіМурахаМожеНести тоді МаєМаксимальноЇжу
                    інакше МаєЇжу

                нехай (|NearHome|_|) (locations: КлітинаСвіту list) =
                    нехай домашніВузли = locations |> List.filter (фун node -> цеМійДім node)
                    якщо List.isEmpty домашніВузли тоді None
                    інакше Some домашніВузли

                нехай (|ВдаліВідДому|БіляДому|) (locations: КлітинаСвіту list) =
                    нехай homeLocations, awayLocations = locations |> List.partition (фун node -> цеМійДім node)
                    якщо List.isEmpty homeLocations тоді ВдаліВідДому awayLocations
                    інакше БіляДому homeLocations

                нехай (|CanDrop|CantDrop|) (місця: КлітинаСвіту list) =
                    нехай dropFoodLocations = місця |> List.filter (фун node -> not (node.IsFullOfFood))
                    якщо List.isEmpty dropFoodLocations тоді CantDrop
                    інакше CanDrop dropFoodLocations

                нехай (|HasUnownedFood|_|) (місця: КлітинаСвіту list) =
                    нехай місцяЇжі = місця |> List.filter (фун node -> node.ЄЇжа && not (цеМійДім node))
                    якщо List.isEmpty місцяЇжі тоді None
                    інакше Some місцяЇжі

                нехай (|ЄФеромониІБезМурахи|_|) (місця: КлітинаСвіту list) =
                    нехай місцяФеромонів = місця |> List.filter (фун node -> node.Мураха = None) |> List.filter (фун node -> node.ЄФеромони я.Колір)
                    якщо List.isEmpty місцяФеромонів тоді None
                    інакше Some місцяФеромонів

                нехай (|БезМурахи|_|) (місця: КлітинаСвіту list) =
                    нехай пустьМісця = місця |> List.filter (фун node -> node.Мураха = None)
                    якщо List.length пустьМісця > 0 тоді
                        Some (пустьМісця)
                    інакше None

                нехай (|ShortestDistanceWithNoAnt|_|)  (locations: КлітинаСвіту list) =
                    нехай noAnts = locations |> List.filter (фун node -> node.Мураха = None)
                    якщо List.length noAnts > 0 тоді Some (noAnts |> List.minBy (фун node -> гніздо.Дістанція node))
                    інакше None

                нехай maxFood = List.maxBy (фун node -> node.Їжа)
                нехай minPhero = List.minBy (фун node -> node.Феромони.[я.Колір])
                нехай noAnts = List.filter (фун node -> node.Мураха = None)

                // [snippet:Simple Pheromone-Using Ant Colony AI]
                співстав я із
                | МаєЇжу
                | МаєМаксимальноЇжу ->
                    співстав місця із
                    | БіляДому homeCells ->
                        співстав homeCells із
                        | CanDrop dropCells -> КинутиЇжу dropCells.Head
                        | БезМурахи noAntCells -> Перемістити (List.random noAntCells)
                        | _ -> Нічого
                    | ВдаліВідДому allCells ->
                        співстав тут.Феромони.[я.Колір] із
                        | n when n < 20 -> КинутиФеромон (тут, 100 - n)
                        | _ ->
                            співстав allCells із
                            | БезМурахи noAnts when слч.Next(0, 3) = 0 -> Перемістити (List.random noAnts)
                            | ShortestDistanceWithNoAnt node -> Перемістити node
                            | _ -> Нічого
                | НемаєЇжі ->
                    співстав місця із
                    | БезМурахи noAnts when слч.Next(0, 3) = 0 -> Перемістити (List.random noAnts)
                    | HasUnownedFood foodCells -> ВзятиЇжу (maxFood foodCells)
                    | ЄФеромониІБезМурахи pheroCells -> Перемістити (minPhero pheroCells)
                    | БезМурахи noAntCells -> Перемістити (List.random noAntCells)
                    | _ -> Нічого


модуль AntsEverywhereExmampleAI =
    відкрити Типи

    нехай randomGen = новий System.Random()

    нехай getRandomVal min max =
        lock randomGen (фун () -> randomGen.Next(min, max))

    тип ТестоваПоведінкаМурахи() =
        інтерфейс ІПоведінкаМурахи із
            член x.Ім'я = "Frank_Levine"
            член x.Поведінка я тут місця гніздо =

                // This Ant's basic strategy is this:
                // If you have food and are near the nest
                //      drop the food
                // If you can't carry anymore food (bur are not near the nest)
                //      head back to the nest із the following exception
                //          якщо the current cell (here) has <40 phereomones, replenish the supply back to 100
                // If you're not dropping off food or heading home, you're foraging
                //      The logic для foraging is:
                //      If you see food, take it (this applies even when you have food but aren't full)
                //      If you see pheromones, move to the pheromone that is farthest from the nest
                //          якщо all pheromones are closer to the nest than you, тоді make a random move
                //      Otherwise you'e у the middle з nowhere, wanter randomly
                //
                // Special note on 'Traffic Control':  Inbound ants always поступатися to outbound ants
                //                                     This seems reasonable since the inbound ants
                //                                     Know where they're going and the outbound ones
                //                                     Are dependent on the pheromone trail



                //
                // вспоміжні функції
                нехай єКлітиною (cell: КлітинаСвіту) = cell.ТипКлітини = ТипКлітиниСвіту.КлітинаГнізда(я.Колір)

                // how зробити I negate a функція?!?  this seems a bit heavy-handed
                нехай неЄКлітиною (cell: КлітинаСвіту) =
                    якщо єКлітиною cell тоді
                        false
                    інакше
                        true

                // nest cells that can receive food
                нехай nestCells = місця |> List.filter єКлітиною
                                        |> List.filter (фун c -> c.IsFullOfFood = false)

                // all empty neighbors, sorted so we can get at the closest and farthest ones from the nest
                // first = closest to nest
                // last = farthest from nest
                нехай emptyNeighbors = місця |> List.filter (фун c -> c.МіститьМураху = false)
                                            |> List.sortBy (фун c -> гніздо.Дістанція(c))

                // all empty neighbors із my pheromones
                нехай emptyNeighborsWithP = emptyNeighbors |> List.filter( фун c -> c.ЄФеромони(я.Колір))
                                                        |> List.sortBy( фун c -> гніздо.Дістанція(c))
                                                        |> List.toArray

                // all neighbors із food, ordered by the amount з food decending
                нехай neighborsWithFood = місця |> List.filter (неЄКлітиною)
                                                |> List.filter (фун c -> c.ЄЇжа)
                                                |> List.sortBy (фун c -> c.Їжа)
                                                |> List.rev

                // functions to make the code below more readable
                // NullMove does nothing (like when you're boxed у)
                // RandomMove is... Random
                нехай NullMove = фун() -> Перемістити тут

                нехай RandomMove = фун () ->
                    нехай i = getRandomVal 0 emptyNeighbors.Length
                    Перемістити (List.item i emptyNeighbors)


                // maximum amount з pheromone to leave on a cell
                нехай МАКС_ФЕРО = 100;

                // when returning to the nest, add more pheromones when the cell
                // has less than this number
                нехай REFRESH_THRESHOLD = 50;



                // active pattern to determine the ant's high-level state
                нехай (|ShouldDropFood|Forage|ReturnToNest|) (ant: Мураха) =
                    нехай haveAvailableNestCells = (nestCells.IsEmpty = false)
                    співстав ant із
                        | a when a.HasFood && haveAvailableNestCells -> ShouldDropFood
                        | a when a.IsFullOfFood -> ReturnToNest
                        | _ -> Forage

                // active pattern to decide якщо we need to refresh pheromones
                нехай (|NeedsRefresh|NoRefresh|) (cell: КлітинаСвіту) =
                    співстав cell.Феромони.[я.Колір] із
                        | x when x < REFRESH_THRESHOLD ->
                            нехай amt = МАКС_ФЕРО - x     // amt is the number з pheromones required to bring this cell back to 100
                            NeedsRefresh amt
                        | _ -> NoRefresh    // there are enough для now

                // gets the relative distance to the nest
                // relativeDist > 0 --> cell is farther from the nest than 'here'
                // relativeDist < 0 --> cell is closer to the nest than 'here'
                нехай relativeDist (cell: КлітинаСвіту) =
                    нехай dHere = гніздо.Дістанція(тут)
                    нехай dCell = гніздо.Дістанція(cell)
                    dCell - dHere

                // функція to get the last thing from an array
                нехай останній (arr: 'a[]) =
                    arr.[arr.Length-1]

                // the ant parameter isn't used, but I don't know how to make a
                // parameterless active pattern
                нехай (|AdjacentToFood|AdjacentToPheromone|NoMansLand|) (ant: Мураха) =
                    якщо neighborsWithFood.Length > 0 тоді
                        AdjacentToFood
                    інякщо emptyNeighborsWithP.Length > 0 && relativeDist (останній emptyNeighborsWithP) > 0. тоді
                        // remember emptyNeighborsWithP is sorted
                        AdjacentToPheromone (останній emptyNeighborsWithP)
                    інакше
                        NoMansLand

                // The Actual logic...

                якщо emptyNeighbors.IsEmpty тоді
                    NullMove()
                інакше
                    співстав я із
                    | ShouldDropFood -> КинутиЇжу nestCells.Head
                    | ReturnToNest ->
                        співстав тут із
                        | NeedsRefresh amt -> КинутиФеромон (тут, amt)
                        | NoRefresh -> Перемістити emptyNeighbors.Head
                    | Forage ->
                        співстав я із
                        | AdjacentToFood -> ВзятиЇжу neighborsWithFood.Head
                        | AdjacentToPheromone pheroCell -> Перемістити pheroCell
                        | NoMansLand -> RandomMove()

відкрити Полотно
відкрити Типи
відкрити Світ
відкрити Сімуляція

нехай origin =
    // Sample is running у an iframe, so get the location з parent
    нехай topLocation = window.top.location
    topLocation.origin + topLocation.pathname

нехай форматуватиКарткуРахунку ім'яЧорного їжаЧорного ім'яЧервоного їжаЧервоного =
    sprintf "Чорний (%s): %05d проти Червоного (%s): %05d" ім'яЧорного їжаЧорного ім'яЧервоного їжаЧервоного

нехай форматуватиЗалишок залишок =
    sprintf "Залишилося циклів: %05d" залишок


нехай maxCycles = 1000
нехай світ = ref (buildWorldInitialWorld())
нехай foodToWin = int <| double (Map.fold (фун s k v -> s + v.Їжа) 0 світ.Value) * percentFoodToWin
нехай цикли = ref 0

нехай чорнийШІ = новий HardishAI.ТестоваПоведінкаМурахи() :> ІПоведінкаМурахи
нехай червонийШІ = новий AntsEverywhereExmampleAI.ТестоваПоведінкаМурахи() :> ІПоведінкаМурахи

нехай render (w,h) =
    цикли.Value <- цикли.Value + 1

    нехай рахунокЧорних = ГніздоЧорнихМурах.КількістьЇжі світ.Value
    нехай рахунокЧервоних = ГніздоЧервонихМурах.КількістьЇжі світ.Value

    нехай залишок = maxCycles - цикли.Value

    нехай рядокРахунка = форматуватиКарткуРахунку чорнийШІ.Ім'я рахунокЧорних червонийШІ.Ім'я рахунокЧервоних
    updateInput "score" рядокРахунка |> ignore

    нехай рядокЗалишку = форматуватиЗалишок залишок
    updateInput "secondline" рядокЗалишку |> ignore


    (0., 0., w, h) |> filled (rgb 200 200 200)
    намалюватиОновлення (w,h) світ.Value
    світ.Value <- worldCycle чорнийШІ червонийШІ світ.Value

    якщо рахунокЧорних > foodToWin || рахунокЧервоних > foodToWin || цикли.Value > maxCycles тоді
        якщо рахунокЧорних > рахунокЧервоних тоді Some чорнийШІ.Ім'я
        інякщо рахунокЧервоних > рахунокЧорних тоді Some червонийШІ.Ім'я
        інакше None
    інакше None

нехай w, h = взятиРозміриВікна()

нехай rec оновити () =
    нехай результат = render (w,h)
    співстав результат із
    | None ->
        window.setTimeout(оновити, 1000 / 30) |> ignore
    | Some переможець ->
        updateInput "secondline" (sprintf "Переможець: %s" переможець) |> ignore

оновити ()
