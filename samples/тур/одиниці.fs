module Tour.UnitsOfMeasure

// Із https://docs.microsoft.com/en-us/dotnet/fsharp/tour
// Щоб дізнатися більше про кожну тему, перейдіть за посиланням вище
// Ви також можете знайти більше навчальних ресурсів за адресою https://fsharp.org/
// (лише англійською)

// Одиниці виміру це шлях анотації примітивних числових типів у типо безпечний шлях.
// Ви можете виконувати типобезпечну аріфметику на цих значеннях.
//
// Щоб дізнатися більше, дивіться: https://docs.microsoft.com/dotnet/fsharp/language-reference/units-of-measure

// Спочатку, відкрийте колекцію загальні назв одиниць виміру
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

/// Визначте вимірну константу
let прикладЗначення1 = 1600.0<meter>

/// Далі, визначте новий тип одиниці виміру
[<Measure>]
type міля =
    /// Коефіцієнт конверсії милі до метрів.
    static member якМетр = 1609.34<meter/міля>

/// Визначте вимірну константу
let прикладЗначення2 = 500.0<міля>

/// Розрахуйте метрічну константу
let прикладЗначення3 = прикладЗначення2 * міля.якМетр

// Значення використовуючи Одиниці Виміру можуть бути використовані так само як і примітивні числові типи для таких речей як друкування.
printfn "Після гонки на %f метрів я пройду %f миль що буде дорівнювати %f метрам" прикладЗначення1 прикладЗначення2 прикладЗначення3
