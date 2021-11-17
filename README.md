# SMPTE Timecode

A library for working with video timecodes, supporting [`SMPTE`][smpte] standards and drop frames.

## [Read docs][docs]

[smpte]: https://en.wikipedia.org/wiki/SMPTE_timecode
[docs]: https://package.elm-lang.org/packages/nomalab/elm-timecode/latest/Timecode

## Install

```
elm install nomalab/elm-timecode
```

## Usage

```elm
import Timecode

formatted =
    Result.map Timecode.fullFormat (Timecode.fromFrameNumber 17982 Timecode.FPS_29_97 True))

parsed =
    Timecode.fromString "00:10:00:00@29.97"

parsedWithFPS =
    Timecode.withFramerateFromString Timecode.FPS_29_97 "00:10:00;00"

operations =
    parsed
    |> Result.map (Timecode.add (Timecode.minutes 10))

parsedWithFPS =
    Timecode.withFramerateFromString Timecode.FPS_29_97 "00:10:00;00"

```