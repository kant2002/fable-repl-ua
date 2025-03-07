// Undertone - Programmable music
// Ported from desktop version: https://github.com/robertpi/undertone
// Inspired by Overtone / Sonic-PI
// By Robert Pickering

module Undertone

open System
open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Browser

type Нота =
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

module РізніКонст =

    /// стандартна частота діскретізації
    /// Дивиться: http://en.wikipedia.org/wiki/44,100_Hz
    let ЧастотаДіскретізації = 44100

    /// "Standard Pitch" noted as A440. The a note that is above middle c
    /// See: http://en.wikipedia.org/wiki/A440_(pitch_standard)
    let A440 = 440.

module Хвилі =
    open System

    /// The ratio require to move from one semi-tone to the next
    /// See: http://en.wikipedia.org/wiki/Semitone
    let private півтон =
        Math.Pow(2., 1. / 12.)

    /// Since our Note enum is relative to c, we need to find middle c.
    /// We know A440 = 440 hz and that the next c is three semi tones
    /// above that, but this is c one ocative above middle c, so we
    /// half the result to get middle c.
    /// Middle c is around 261.626 Hz, and this approximately the value we get
    /// See: http://en.wikipedia.org/wiki/C_(musical_note)
    let private середнеДо =
        РізніКонст.A440 * Math.Pow(півтон, 3.) / 2.

    /// Converts from our note enum to the notes frequency
    let частотаНоти (нота: Нота) октава =
        середнеДо *
        // calculate the ratio need to move to the note's semitone
        Math.Pow(півтон, double (int нота)) *
        // calculate the ratio need to move to the note's octave
        Math.Pow(2., double (октава - 4))

    /// calculates the distance you need to move in each sample
    let інкрементФазовогоКутаДляЧастоти частота =
        частота / double РізніКонст.ЧастотаДіскретізації

/// functions an constants for manipulating musical time
module Час =
    /// this hard codes our module to the lower "4" in 4/4 time
    let beatsPerSemibreve = 4.
    /// number bars
    let private ударівВСекунду bmp =  60. / bmp
    /// number of samples required to make a bar of m- usic
    let private samplesPerBar bmp = (float РізніКонст.ЧастотаДіскретізації * ударівВСекунду bmp * beatsPerSemibreve)

    /// longa - either twice or three times as long as a breve (we choose twice)
    /// it is no longer used in modern music notation
    let longa = 4.
    /// double whole note -  twice as long as semibreve
    let бревіс = 2.
    /// whole note -  its length is equal to four beats in 4/4 time
    /// most other notes are fractions of the whole note
    let ціла = 1.
    /// half note
    let половинка = 1. / 2.
    /// quarter note
    let чвертка = 1. / 4.
    /// eighth note
    let вісімка = 1. / 8.
    /// sixteenth note
    let шістнадцятка = 1. / 16.
    /// thirty-second note
    let тридцятьДруга = 1. / 32.

    /// caculates a note's length in samples
    let noteValue bmp нота =
        samplesPerBar bmp * нота |> int

/// Functions for creating waves
module Створення =

    /// make a period of silence
    let зробитиТишу довжина =
        Seq.init довжина (fun _ -> 0.)

    /// make a wave using the given function, length and frequency
    let зробитиХвилю хвильоваФунк довжина частота =
        let інкрементФазовогоКута = Хвилі.інкрементФазовогоКутаДляЧастоти частота
        Seq.init довжина (fun x ->
            let фазовийКут = інкрементФазовогоКута * (float x)
            let x = Math.Floor(фазовийКут)
            хвильоваФунк (фазовийКут - x))  

    /// make a wave using the given function, length note and octave
    let зробитиНоту хвильоваФунк довжина нота октава =
        let частота = Хвилі.частотаНоти нота октава
        зробитиХвилю хвильоваФунк довжина частота

    /// function for making a sine wave
    let сінусоїда фазовийКут =
        Math.Sin(2. * Math.PI * фазовийКут)

    /// function for making a square wave
    let квадрат фазовийКут =
        if фазовийКут < 0.5 then -1.0 else 1.0

    /// function for making triangular waves
    let трикутник фазовийКут =
        if фазовийКут < 0.5 then
            2. * фазовийКут
        else
            1. - (2. * фазовийКут)

    // function for making making "saw tooth" wave
    let пила фазовийКут =
        -1. + фазовийКут

    // function for combining several waves into a cord combines
    let makeCord (waveDefs: seq<seq<float>>) =
        let wavesMatrix = waveDefs |> Seq.map (Seq.toArray) |> Seq.toArray
        let waveScaleFactor = 1. / float wavesMatrix.Length
        let maxLength = wavesMatrix |> Seq.maxBy (fun x -> x.Length)
        let getValues i =
            seq { for x in 0 .. wavesMatrix.Length - 1 do
                    yield if i > wavesMatrix.[x].Length then 0. else wavesMatrix.[x].[i] }
        seq { for x in 0 .. maxLength.Length - 1 do yield (getValues x |> Seq.sum) * waveScaleFactor }

    // same as makeCord but does use arrays so can handle long or even infinite sequences.
    let combine (waveDefs: seq<seq<float>>) =
        let enumerators = waveDefs |> Seq.map (fun x -> x.GetEnumerator()) |> Seq.cache
        let loop () =
            let значення =
                enumerators
                |> Seq.choose
                    (fun x -> if x.MoveNext() then Some x.Current else None)
                |> Seq.toList
            match значення with
            | [] -> None
            | x -> Some ((x |> Seq.sum), ())
        Seq.unfold loop ()

/// functions for transforming waves
module Трансформації =
    /// makes the waves amplitude large or small by scaling by the given multiplier
    let scaleHeight multiplier (waveDef: seq<float>) =
        waveDef |> Seq.map (fun x -> x * multiplier)

    let private rnd = new Random()

    /// Adds some noise to the wave (not recommended)
    let додатиШум multiplier (waveDef: seq<float>) =
        waveDef
        |> Seq.map (fun x ->
                        let rndValue = 0.5 - rnd.NextDouble()
                        x +  (rndValue * multiplier))

    /// flattens the wave at the given limit to give an overdrive effect
    let flatten limit (waveDef: seq<float>) =
        waveDef
        |> Seq.map (fun x -> max -limit (min x limit))

    /// provides a way to linearly tapper a wave, the startMultiplier is
    /// applied to the first value of the a wave, and endMultiplier is
    /// applied to the last value, the other values have value that is linearly
    /// interpolated between the two values
    let tapper startMultiplier endMultiplier (waveDef: seq<float>) =
        let waveVector = waveDef |> Seq.toArray
        let step = (endMultiplier - startMultiplier) / float waveVector.Length
        waveVector
        |> Seq.mapi (fun i x -> x * (startMultiplier + (step * float i)))

    /// gets a point on the gaussian distribution
    let private gaussian a b c x  = Math.Pow((a * Math.E), -(Math.Pow(x - b, 2.) / Math.Pow(c * 2., 2.)))

    /// applies a gaussian tapper to the front of a wave
    let gaussianTapper length (waveDef: seq<float>) =
        let waveVector = waveDef |> Seq.toArray
        let step = 1. / float waveVector.Length
        waveVector
        |> Seq.mapi (fun i x -> x * gaussian 1. 0. length (step * float i))

    /// applies a gaussian tapper to the back of a wave
    let revGaussianTapper length (waveDef: seq<float>) =
        let waveVector = waveDef |> Seq.toArray
        let len = float waveVector.Length
        let step = 1. / len
        waveVector
        |> Seq.mapi (fun i x -> x * gaussian 1. 0. length (step * (len - float i)))

    /// applies a gaussian tapper to the front and back of a wave
    let doubleGaussianTapper startLength endLength (waveDef: seq<float>) =
        let waveVector = waveDef |> Seq.toArray
        let len = float waveVector.Length
        let step = 1. / len
        waveVector
        |> Seq.mapi (fun i x -> x *
                                (gaussian 1. 0. startLength (step * (len - float i))) *
                                (gaussian 1. 0. endLength (step * float i)))

/// Functions to turn a list of chords into a playable sound wave
module NoteSequencer =
    type Chord = seq<Нота*int>

    /// version of Seq.take that doesn't though exceptions if you reach the end of the sequence
    let private safeTake wanted (source : seq<'T>) =
        (* Note: don't create or dispose any IEnumerable if n = 0 *)
        if wanted = 0 then Seq.empty else
        seq { use e = source.GetEnumerator()
              let count = ref 0
              while e.MoveNext() && count.Value < wanted do
                count.Value <- count.Value + 1
                yield e.Current }

    // function that does a function the describes how a note should be played and list of chords
    // and generates a sound wave from them
    let sequence (noteTable: Нота -> int -> seq<float>) (notes: seq<#Chord*int>) =
        seq { for cordNotes, length in notes do
                let notes = cordNotes |> Seq.map (fun (note, octave) -> noteTable note octave)
                yield! Створення.combine notes |> safeTake length }

module WaveFormat =
    let sampleRate = 44100
    let канали = 1

    let байтиЦіл16 i =
        [ 0; 8; ]
        |> List.map (fun зсув -> (i >>> зсув) &&& 0x00ffs |> byte)

    let байтиЦіл i =
        [ 0; 8; 16; 24 ]
        |> List.map (fun зсув -> (i >>> зсув) &&& 0x000000ff |> byte)

    let wavOfBuffer (буфер: float[]) =
        let sixteenBitLength = 2 * буфер.Length

        [| yield! "RIFF" |> Seq.map byte
           yield! байтиЦіл (sixteenBitLength + 15)
           yield! "WAVE" |> Seq.map byte
           yield! "fmt " |> Seq.map byte
           yield 0x12uy // fmt chunksize: 18
           yield 0x00uy
           yield 0x00uy //
           yield 0x00uy
           yield 0x01uy // format tag : 1
           yield 0x00uy
           yield канали |> byte // channels
           yield 0x00uy
           yield! байтиЦіл (sampleRate)
           yield! байтиЦіл (2*канали*sampleRate)
           yield 0x04uy // block align
           yield 0x00uy
           yield 0x10uy // bit per sample
           yield 0x00uy
           yield 0x00uy // cb size
           yield 0x00uy
           yield! "data" |> Seq.map byte
           yield! байтиЦіл sixteenBitLength
           for i in [ 0 .. буфер.Length - 1 ] do
                let тимч = буфер.[i]
                if (тимч >= 1.) then
                    yield 0xFFuy
                    yield 0xFFuy
                elif (тимч <= -1.) then
                    yield 0x00uy
                    yield 0x00uy
                else
                    yield! Math.Round(тимч * float (Int16.MaxValue)) |> int16 |> байтиЦіл16 |]

module Svg =
    let svg = document.getElementById("svg")

    let displayWave (points: float[]) =
        let margin = 10.
        let lineSpacing = 1.
        let lineWidth = 1.

        let довжина = (svg.clientWidth / lineSpacing) |> int
        let midPoint = svg.clientHeight / 2.
        let maxLine = midPoint - margin

        let rnd = new Random()

        let chunkSize = points.Length / довжина

        let samples =
            points
            |> Seq.map (fun x -> Math.Abs(x))
            |> Seq.chunkBySize chunkSize
            |> Seq.map Array.average
            |> Seq.toArray

        let svgns = "http://www.w3.org/2000/svg";
        for i in 1 .. довжина do
            let розмір = samples.[i] * maxLine
            let y1 = midPoint - розмір
            let y2 = midPoint + розмір
            let лінія = document.createElementNS(svgns, "line");
            let x = float i * lineSpacing

            лінія.setAttributeNS(null, "x1", string x);
            лінія.setAttributeNS(null, "y1", string y1);
            лінія.setAttributeNS(null, "x2", string x);
            лінія.setAttributeNS(null, "y2", string y2);
            лінія.setAttributeNS(null, "stroke-width", string lineWidth);
            лінія.setAttributeNS(null, "stroke", "#000000");

            document.getElementById("svg").appendChild(лінія) |> ignore

module Html =
    let audio = document.getElementsByTagName("audio").[0] :?> HTMLAudioElement

    let завантажитиЗвук (soundSequence: seq<float>) =
        let getBaseWav64 sound =
            let wav = WaveFormat.wavOfBuffer (sound |> Seq.toArray)
            Convert.ToBase64String(wav)

        let звуковийБуфер = soundSequence |> Seq.toArray

        let wavBase64 = getBaseWav64 звуковийБуфер
        audio.src <- "data:audio/wav;base64," + wavBase64

        Svg.displayWave звуковийБуфер


let bpm = 90.
let чвертка = Час.noteValue bpm Час.чвертка
let вісімка = Час.noteValue bpm Час.вісімка

let створитиНоту час нота =
    Створення.зробитиНоту Створення.сінусоїда час нота 4
    |> Трансформації.gaussianTapper 0.1

let baaBaaBlackSheepChorus =
    seq {

let baaBaaBlackSheepChorus =
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
          //One for the master, one for the dame
          yield! створитиНоту чвертка Нота.Ля
          yield! створитиНоту вісімка Нота.Ля
          yield! створитиНоту вісімка Нота.Ля

          yield! створитиНоту чвертка Нота.Фа
          yield! створитиНоту чвертка Нота.Фа
          
          yield! створитиНоту чвертка Нота.Мі
          yield! створитиНоту вісімка Нота.Мі
          yield! створитиНоту вісімка Нота.Мі

          yield! створитиНоту половинка Нота.Ре

          //One for the little boy, who lives down the lane
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
