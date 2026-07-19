// Undertone - Programmable music
// Ported from desktop version: https://github.com/robertpi/undertone
// Inspired by Overtone / Sonic-PI
// By Robert Pickering

модуль Undertone

відкрити System
відкрити Fable.Core
відкрити Fable.Core.JsInterop
відкрити Browser.Types
відкрити Browser

тип Нота =
    | ДоБемоль   = -1
    | До         = 0
    | ДоДієз     = 1
    | РеБемоль   = 1
    | Ре         = 2
    | РеДієз     = 3
    | МіБемоль   = 3
    | Мі         = 4
    | МіДієз     = 5
    | ФаБемоль   = 4
    | Фа         = 5
    | ФаДієз     = 6
    | СольБемоль = 6
    | Соль       = 7
    | СольДієз   = 8
    | ЛяБемоль   = 8
    | Ля         = 9
    | ЛяДієз     = 10
    | СіБемоль   = 10
    | Сі         = 11
    | СіДієз     = 12

модуль РізніКонст =

    /// стандартна частота діскретізації
    /// Дивиться: http://en.wikipedia.org/wiki/44,100_Hz
    нехай ЧастотаДіскретізації = 44100

    /// "Standard Pitch" noted as A440. The a note that is above middle c
    /// See: http://en.wikipedia.org/wiki/A440_(pitch_standard)
    нехай A440 = 440.

модуль Хвилі =
    відкрити System

    /// The ratio require to move from one semi-tone to the next
    /// See: http://en.wikipedia.org/wiki/Semitone
    нехай приватний півтон =
        Math.Pow(2., 1. / 12.)

    /// Since our Note enum is relative to c, we need to find middle c.
    /// We know A440 = 440 hz and that the next c is three semi tones
    /// above that, but this is c one ocative above middle c, so we
    /// half the result to get middle c.
    /// Middle c is around 261.626 Hz, and this approximately the value we get
    /// See: http://en.wikipedia.org/wiki/C_(musical_note)
    нехай приватний середнеДо =
        РізніКонст.A440 * Math.Pow(півтон, 3.) / 2.

    /// Converts from our note enum to the notes frequency
    нехай частотаНоти (нота: Нота) октава =
        середнеДо *
        // calculate the ratio need to move to the note's semitone
        Math.Pow(півтон, double (int нота)) *
        // calculate the ratio need to move to the note's octave
        Math.Pow(2., double (октава - 4))

    /// calculates the distance you need to move у each sample
    нехай інкрементФазовогоКутаДляЧастоти частота =
        частота / double РізніКонст.ЧастотаДіскретізації

/// functions an constants для manipulating musical time
модуль Час =
    /// this hard codes our модуль to the lower "4" у 4/4 time
    нехай beatsPerSemibreve = 4.
    /// number bars
    нехай приватний ударівВСекунду bmp =  60. / bmp
    /// number з samples required to make a bar з m- usic
    нехай приватний samplesPerBar bmp = (float РізніКонст.ЧастотаДіскретізації * ударівВСекунду bmp * beatsPerSemibreve)

    /// longa - either twice or three times as long as a breve (we choose twice)
    /// it is no longer used у modern music notation
    нехай longa = 4.
    /// double whole note -  twice as long as semibreve
    нехай бревіс = 2.
    /// whole note -  its length is equal to four beats у 4/4 time
    /// most other notes are fractions з the whole note
    нехай ціла = 1.
    /// half note
    нехай половинка = 1. / 2.
    /// quarter note
    нехай чвертка = 1. / 4.
    /// eighth note
    нехай вісімка = 1. / 8.
    /// sixteenth note
    нехай шістнадцятка = 1. / 16.
    /// thirty-second note
    нехай тридцятьДруга = 1. / 32.

    /// caculates a note's length у samples
    нехай noteValue bmp нота =
        samplesPerBar bmp * нота |> int

/// Functions для creating waves
модуль Створення =

    /// make a period з silence
    нехай зробитиТишу довжина =
        Seq.init довжина (фун _ -> 0.)

    /// make a wave using the given функція, length and frequency
    нехай зробитиХвилю хвильоваФунк довжина частота =
        нехай інкрементФазовогоКута = Хвилі.інкрементФазовогоКутаДляЧастоти частота
        Seq.init довжина (фун x ->
            нехай фазовийКут = інкрементФазовогоКута * (float x)
            нехай x = Math.Floor(фазовийКут)
            хвильоваФунк (фазовийКут - x))  

    /// make a wave using the given функція, length note and octave
    нехай зробитиНоту хвильоваФунк довжина нота октава =
        нехай частота = Хвилі.частотаНоти нота октава
        зробитиХвилю хвильоваФунк довжина частота

    /// функція для making a sine wave
    нехай сінусоїда фазовийКут =
        Math.Sin(2. * Math.PI * фазовийКут)

    /// функція для making a square wave
    нехай квадрат фазовийКут =
        якщо фазовийКут < 0.5 тоді -1.0 інакше 1.0

    /// функція для making triangular waves
    нехай трикутник фазовийКут =
        якщо фазовийКут < 0.5 тоді
            2. * фазовийКут
        інакше
            1. - (2. * фазовийКут)

    // функція для making making "saw tooth" wave
    нехай пила фазовийКут =
        -1. + фазовийКут

    // функція для combining several waves into a cord combines
    нехай makeCord (waveDefs: seq<seq<float>>) =
        нехай wavesMatrix = waveDefs |> Seq.map (Seq.toArray) |> Seq.toArray
        нехай waveScaleFactor = 1. / float wavesMatrix.Length
        нехай maxLength = wavesMatrix |> Seq.maxBy (фун x -> x.Length)
        нехай getValues i =
            seq { для x у 0 .. wavesMatrix.Length - 1 зробити
                    поступатися якщо i > wavesMatrix.[x].Length тоді 0. інакше wavesMatrix.[x].[i] }
        seq { для x у 0 .. maxLength.Length - 1 зробити поступатися (getValues x |> Seq.sum) * waveScaleFactor }

    // same as makeCord but does use arrays so can handle long or even infinite sequences.
    нехай combine (waveDefs: seq<seq<float>>) =
        нехай enumerators = waveDefs |> Seq.map (фун x -> x.GetEnumerator()) |> Seq.cache
        нехай loop () =
            нехай значення =
                enumerators
                |> Seq.choose
                    (фун x -> якщо x.MoveNext() тоді Some x.Current інакше None)
                |> Seq.toList
            співстав значення із
            | [] -> None
            | x -> Some ((x |> Seq.sum), ())
        Seq.unfold loop ()

/// functions для transforming waves
модуль Трансформації =
    /// makes the waves amplitude large or small by scaling by the given multiplier
    нехай scaleHeight multiplier (waveDef: seq<float>) =
        waveDef |> Seq.map (фун x -> x * multiplier)

    нехай приватний rnd = новий Random()

    /// Adds some noise to the wave (not recommended)
    нехай додатиШум multiplier (waveDef: seq<float>) =
        waveDef
        |> Seq.map (фун x ->
                        нехай rndValue = 0.5 - rnd.NextDouble()
                        x +  (rndValue * multiplier))

    /// flattens the wave at the given limit to give an overdrive effect
    нехай flatten limit (waveDef: seq<float>) =
        waveDef
        |> Seq.map (фун x -> max -limit (min x limit))

    /// provides a way to linearly tapper a wave, the startMultiplier is
    /// applied to the first value з the a wave, and endMultiplier is
    /// applied to the last value, the other values have value that is linearly
    /// interpolated between the two values
    нехай tapper startMultiplier endMultiplier (waveDef: seq<float>) =
        нехай waveVector = waveDef |> Seq.toArray
        нехай step = (endMultiplier - startMultiplier) / float waveVector.Length
        waveVector
        |> Seq.mapi (фун i x -> x * (startMultiplier + (step * float i)))

    /// gets a point on the gaussian distribution
    нехай приватний gaussian a b c x  = Math.Pow((a * Math.E), -(Math.Pow(x - b, 2.) / Math.Pow(c * 2., 2.)))

    /// applies a gaussian tapper to the front з a wave
    нехай gaussianTapper length (waveDef: seq<float>) =
        нехай waveVector = waveDef |> Seq.toArray
        нехай step = 1. / float waveVector.Length
        waveVector
        |> Seq.mapi (фун i x -> x * gaussian 1. 0. length (step * float i))

    /// applies a gaussian tapper to the back з a wave
    нехай revGaussianTapper length (waveDef: seq<float>) =
        нехай waveVector = waveDef |> Seq.toArray
        нехай len = float waveVector.Length
        нехай step = 1. / len
        waveVector
        |> Seq.mapi (фун i x -> x * gaussian 1. 0. length (step * (len - float i)))

    /// applies a gaussian tapper to the front and back з a wave
    нехай doubleGaussianTapper startLength endLength (waveDef: seq<float>) =
        нехай waveVector = waveDef |> Seq.toArray
        нехай len = float waveVector.Length
        нехай step = 1. / len
        waveVector
        |> Seq.mapi (фун i x -> x *
                                (gaussian 1. 0. startLength (step * (len - float i))) *
                                (gaussian 1. 0. endLength (step * float i)))

/// Functions to turn a list з chords into a playable sound wave
модуль NoteSequencer =
    тип Chord = seq<Нота*int>

    /// version з Seq.take that doesn't though exceptions якщо you reach the end з the sequence
    нехай приватний safeTake wanted (source : seq<'T>) =
        (* Note: don't create or dispose any IEnumerable якщо n = 0 *)
        якщо wanted = 0 тоді Seq.empty інакше
        seq { use e = source.GetEnumerator()
              нехай count = ref 0
              while e.MoveNext() && count.Value < wanted зробити
                count.Value <- count.Value + 1
                поступатися e.Current }

    // функція that does a функція the describes how a note should be played and list з chords
    // and generates a sound wave from them
    нехай sequence (noteTable: Нота -> int -> seq<float>) (notes: seq<#Chord*int>) =
        seq { для cordNotes, length у notes зробити
                нехай notes = cordNotes |> Seq.map (фун (note, octave) -> noteTable note octave)
                yield! Створення.combine notes |> safeTake length }

модуль WaveFormat =
    нехай sampleRate = 44100
    нехай канали = 1

    нехай байтиЦіл16 i =
        [ 0; 8; ]
        |> List.map (фун зсув -> (i >>> зсув) &&& 0x00ffs |> byte)

    нехай байтиЦіл i =
        [ 0; 8; 16; 24 ]
        |> List.map (фун зсув -> (i >>> зсув) &&& 0x000000ff |> byte)

    нехай wavOfBuffer (буфер: float[]) =
        нехай sixteenBitLength = 2 * буфер.Length

        [| yield! "RIFF" |> Seq.map byte
           yield! байтиЦіл (sixteenBitLength + 15)
           yield! "WAVE" |> Seq.map byte
           yield! "fmt " |> Seq.map byte
           поступатися 0x12uy // fmt chunksize: 18
           поступатися 0x00uy
           поступатися 0x00uy //
           поступатися 0x00uy
           поступатися 0x01uy // format tag : 1
           поступатися 0x00uy
           поступатися канали |> byte // channels
           поступатися 0x00uy
           yield! байтиЦіл (sampleRate)
           yield! байтиЦіл (2*канали*sampleRate)
           поступатися 0x04uy // block align
           поступатися 0x00uy
           поступатися 0x10uy // bit per sample
           поступатися 0x00uy
           поступатися 0x00uy // cb size
           поступатися 0x00uy
           yield! "data" |> Seq.map byte
           yield! байтиЦіл sixteenBitLength
           для i у [ 0 .. буфер.Length - 1 ] зробити
                нехай тимч = буфер.[i]
                якщо (тимч >= 1.) тоді
                    поступатися 0xFFuy
                    поступатися 0xFFuy
                інякщо (тимч <= -1.) тоді
                    поступатися 0x00uy
                    поступатися 0x00uy
                інакше
                    yield! Math.Round(тимч * float (Int16.MaxValue)) |> int16 |> байтиЦіл16 |]

модуль Svg =
    нехай svg = document.getElementById("svg")

    нехай displayWave (points: float[]) =
        нехай margin = 10.
        нехай lineSpacing = 1.
        нехай lineWidth = 1.

        нехай довжина = (svg.clientWidth / lineSpacing) |> int
        нехай midPoint = svg.clientHeight / 2.
        нехай maxLine = midPoint - margin

        нехай rnd = новий Random()

        нехай chunkSize = points.Length / довжина

        нехай samples =
            points
            |> Seq.map (фун x -> Math.Abs(x))
            |> Seq.chunkBySize chunkSize
            |> Seq.map Array.average
            |> Seq.toArray

        нехай svgns = "http://www.w3.org/2000/svg";
        для i у 1 .. довжина зробити
            нехай розмір = samples.[i] * maxLine
            нехай y1 = midPoint - розмір
            нехай y2 = midPoint + розмір
            нехай лінія = document.createElementNS(svgns, "line");
            нехай x = float i * lineSpacing

            лінія.setAttributeNS(нуль, "x1", string x);
            лінія.setAttributeNS(нуль, "y1", string y1);
            лінія.setAttributeNS(нуль, "x2", string x);
            лінія.setAttributeNS(нуль, "y2", string y2);
            лінія.setAttributeNS(нуль, "stroke-width", string lineWidth);
            лінія.setAttributeNS(нуль, "stroke", "#000000");

            document.getElementById("svg").appendChild(лінія) |> ignore

модуль Html =
    нехай audio = document.getElementsByTagName("audio").[0] :?> HTMLAudioElement

    нехай завантажитиЗвук (soundSequence: seq<float>) =
        нехай getBaseWav64 sound =
            нехай wav = WaveFormat.wavOfBuffer (sound |> Seq.toArray)
            Convert.ToBase64String(wav)

        нехай звуковийБуфер = soundSequence |> Seq.toArray

        нехай wavBase64 = getBaseWav64 звуковийБуфер
        audio.src <- "data:audio/wav;base64," + wavBase64

        Svg.displayWave звуковийБуфер


нехай bpm = 90.
нехай половинка = Час.noteValue bpm Час.половинка
нехай чвертка = Час.noteValue bpm Час.чвертка
нехай вісімка = Час.noteValue bpm Час.вісімка

нехай створитиНоту час нота =
    Створення.зробитиНоту Створення.сінусоїда час нота 4
    |> Трансформації.gaussianTapper 0.1

нехай baaBaaBlackSheepChorus =
    seq {
          //C C G G A A AA G
          //Baa baa black sheep have you any wool?
          yield! створитиНоту чвертка Нота.До
          yield! створитиНоту чвертка Нота.До
          yield! створитиНоту чвертка Нота.Соль
          yield! створитиНоту чвертка Нота.Соль

          yield! створитиНоту вісімка Нота.Ля
          yield! створитиНоту вісімка Нота.Ля
          yield! створитиНоту вісімка Нота.Ля
          yield! створитиНоту вісімка Нота.Ля
          yield! створитиНоту половинка Нота.Соль
          //F F E E D D C
          //Yes sir yes sir three bags full.
          yield! створитиНоту чвертка Нота.Фа
          yield! створитиНоту чвертка Нота.Фа
          yield! створитиНоту чвертка Нота.Мі
          yield! створитиНоту чвертка Нота.Мі

          yield! створитиНоту чвертка Нота.Ре
          yield! створитиНоту чвертка Нота.Ре
          yield! створитиНоту половинка Нота.До
          
          //F F E E D D C
          //One для the master, one для the dame
          yield! створитиНоту чвертка Нота.Ля
          yield! створитиНоту вісімка Нота.Ля
          yield! створитиНоту вісімка Нота.Ля

          yield! створитиНоту чвертка Нота.Фа
          yield! створитиНоту чвертка Нота.Фа
          
          yield! створитиНоту чвертка Нота.Мі
          yield! створитиНоту вісімка Нота.Мі
          yield! створитиНоту вісімка Нота.Мі

          yield! створитиНоту половинка Нота.Ре

          //One для the little boy, who lives down the lane
          yield! створитиНоту чвертка Нота.Ля
          yield! створитиНоту вісімка Нота.Ля
          yield! створитиНоту вісімка Нота.Ля

          yield! створитиНоту вісімка Нота.Фа
          yield! створитиНоту вісімка Нота.Фа
          yield! створитиНоту вісімка Нота.Фа
          yield! створитиНоту вісімка Нота.Фа
          
          yield! створитиНоту чвертка Нота.Мі
          yield! створитиНоту вісімка Нота.Мі
          yield! створитиНоту вісімка Нота.Мі

          yield! створитиНоту половинка Нота.Ре
          
          //C C G G A A AA G
          //Baa baa black sheep have you any wool?
          yield! створитиНоту чвертка Нота.До
          yield! створитиНоту чвертка Нота.До
          yield! створитиНоту чвертка Нота.Соль
          yield! створитиНоту чвертка Нота.Соль

          yield! створитиНоту вісімка Нота.Ля
          yield! створитиНоту вісімка Нота.Ля
          yield! створитиНоту вісімка Нота.Ля
          yield! створитиНоту вісімка Нота.Ля
          yield! створитиНоту половинка Нота.Соль
          //F F E E D D C
          //Yes sir yes sir three bags full.
          yield! створитиНоту чвертка Нота.Фа
          yield! створитиНоту чвертка Нота.Фа
          yield! створитиНоту чвертка Нота.Мі
          yield! створитиНоту чвертка Нота.Мі

          yield! створитиНоту чвертка Нота.Ре
          yield! створитиНоту чвертка Нота.Ре
          yield! створитиНоту половинка Нота.До
        }

Html.завантажитиЗвук baaBaaBlackSheepChorus
