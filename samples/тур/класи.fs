module Tour.Classes

// Із https://docs.microsoft.com/en-us/dotnet/fsharp/tour
// Щоб дізнатися більше про кожну тему, перейдіть за посиланням вище
// Ви також можете знайти більше навчальних ресурсів за адресою https://fsharp.org/
// (лише англійською)

/// Класи це спосіб визначити новий тип об'єктів у F#, і підтримувати стандартні Об'єктно оріентовані конструкції.
/// Вони можуть мати різномаїть членів (методи, властивості, події, тощо)
///
/// Щоб дізнатися про Класи, дивиться: https://docs.microsoft.com/dotnet/fsharp/language-reference/classes
///
/// Щоб дізнатися про Членів, дивиться: https://docs.microsoft.com/dotnet/fsharp/language-reference/members

/// Простий двовимірний класс Вектор.
///
/// Конструктор класа на першому рядку,
/// він приймає два аргумента: dx та dy, обидва типу 'double'.
type Вектор2D(dx : double, dy : double) =

    /// Це внутрішне поле зберігає довжину вектора, обчислюючи її коли
    /// об'єкт створюється
    let довжина = sqrt (dx*dx + dy*dy)

    // 'я' вказує ім'я для ідентіфікатора самого себе для об'єкту.
    // У методах екземпляру, він повинен йти перед назвою члена.
    member я.DX = dx

    member я.DY = dy

    member я.Довжина = довжина

    /// Цей член метод.  Попередні члени були властивостями.
    member this.Масштабувати(k) = Вектор2D(k * this.DX, k * this.DY)

/// Ви створюєте екземпляр класу Вектор2D таким чином.
let вектор1 = Вектор2D(3.0, 4.0)

/// Отримати новий масштабований об'єкт вектора, без зміни першоначального об'єкта.
let вектор2 = вектор1.Масштабувати(10.0)

printfn "Довжина вектор1: %f\nДовжина вектор2: %f" вектор1.Довжина вектор2.Довжина


/// Узагальнені класи дозволяють визначати типи відносно набору параметрів типів.
/// У наступного типа, 'T це параметр типа для класу.
///
/// Щоб дізнатися більше, дивиться: https://docs.microsoft.com/dotnet/fsharp/language-reference/generics/

type ВідслідковувачСтану<'T>(початковийЕлемент: 'T) =

    /// Це внутрішне поле яке зберігає стани у списку.
    let mutable стани = [ початковийЕлемент ]

    /// Додає новий елемент к списку станів.
    member я.ОновитиСтан новийСтан =
        стани <- новийСтан :: стани  // use the '<-' operator to mutate the value.

    /// Повертає повний список історичних станів.
    member я.Історія = стани

    /// Повертає останній стан.
    member this.Поточний = стани.Head

/// Екземпляр 'int' класу відслідковувача стану. Зверніть увагу що параметр типу виведен.
let відслідковувач = ВідслідковувачСтану 10

// Додати стан
відслідковувач.ОновитиСтан 17


/// Інтерфеййси це типи об'єктів із лише 'абстрактними' членами.
/// Типи об'єктів і вирази об'єктів можуть реалізовувати інтерфейси.
///
/// Щоб дізнатися більше, дивиться: https://docs.microsoft.com/dotnet/fsharp/language-reference/interfaces

/// Це тип який реалізує IDisposable.
type ПрочитатиФайл(шлях: string) =
    member я.ПрочитатиРядок() = printfn "Читаємо %s..." шлях

    // Це реалізація членів IDisposable.
    interface System.IDisposable with
        member я.Dispose() = printfn "Закриваємо %s..." шлях


/// Це об'єкт який реалізував IDisposable через Вираз Об'єктуі
/// На відміну від інших мов таких як C# або Java, нове визначення типу не потрібно
/// для реалізації інтерфейсу.
let реалізаціяІнтерфейсу =
    { new System.IDisposable with
        member this.Dispose() = printfn "disposed" }

