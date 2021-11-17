module Timecode exposing
    ( Timecode, fromFrameNumber, fromDisplay, fromString, withFramerateFromString, getFrameNumber, getFramerate, getDropFrame, getTime
    , Display, display, format, fullFormat
    , add, hours, minutes, seconds, frames
    , Framerate(..), framerateAsFloat, framerateAsString, framerateFromFloat, framerateFromString, supportsDropFrame
    , TimecodeError(..), errorToString
    )


{-| A library for working with video timecodes, supporting [`SMPTE`][smpte] standards and drop frames.

[smpte]: https://en.wikipedia.org/wiki/SMPTE_timecode

# Timecode
@docs Timecode, fromFrameNumber, fromDisplay, fromString, withFramerateFromString, add, getFrameNumber, getFramerate, getDropFrame, getTime

# Render
@docs Display, display, format, fullFormat

# Operations
@docs add, hours, minutes, seconds, frames

# Handle Framerate
@docs Framerate, framerateAsFloat, framerateAsString, framerateFromFloat, framerateFromString, supportsDropFrame

# Errors
@docs TimecodeError, errorToString

-}


import Regex


{-| Timcode's opaque type -}
type Timecode =
    Timecode
        { frameNumber : Float
        , framerate : Framerate
        , dropFrame : Bool
        }


{-| Display timecodes in a readable fashion -}
type alias Display =
    { hours : String
    , minutes : String
    , seconds : String
    , frames : String
    , fps : String
    , dropFrame : Bool
    }


{-| Standard video framerates -}
type Framerate
    = FPS_24
    | FPS_25
    | FPS_30
    | FPS_50
    | FPS_60
    | FPS_23_98
    | FPS_29_97
    | FPS_59_94


{-| Timecode errors -}
type TimecodeError
    = DropFrameSupport
    | InvalidTimecode


{-| Private type that holds what's particular about each Framerate -}
type alias FramerateInfo =
    { frames : Float
    , second : Float
    , droppedFrames : Maybe Float
    }


{-|  English description of the error -}
errorToString : TimecodeError -> String
errorToString err =
    case err of
        DropFrameSupport -> "Drop frame is only supported for 29.97, and 59.94 framerate"
        InvalidTimecode -> "Invalid timecode"


getFramerateInfo : Framerate -> FramerateInfo
getFramerateInfo framerate =
    case framerate of
        FPS_24    -> FramerateInfo 24 1.000 Nothing
        FPS_25    -> FramerateInfo 25 1.000 Nothing
        FPS_30    -> FramerateInfo 30 1.000 Nothing
        FPS_50    -> FramerateInfo 50 1.000 Nothing
        FPS_60    -> FramerateInfo 60 1.000 Nothing
        FPS_23_98 -> FramerateInfo 24 1.001 Nothing
        FPS_29_97 -> FramerateInfo 30 1.001 (Just 2)
        FPS_59_94 -> FramerateInfo 60 1.001 (Just 4)


{-| Get Timecode as a Display value -}
display : Timecode -> Display
display (Timecode { frameNumber, framerate, dropFrame }) =
    let
        hours_ =
            modBy 24 (floor (frameNumberAdjusted / (framerateRounded * 3600)))

        minutes_ =
            modBy 60 (floor (frameNumberAdjusted / (framerateRounded * 60)))

        seconds_ =
            modBy 60 (floor (frameNumberAdjusted / framerateRounded))

        frames_ =
            modBy (round framerateRounded) (round frameNumberAdjusted)

        framerateInfo =
            getFramerateInfo framerate

        framerateRounded =
            framerateInfo.frames

        frameNumberAdjusted =
            if dropFrame then
                let
                    droppedFramesCount =
                        framerateInfo.droppedFrames
                        |> Maybe.withDefault 0

                    oneMinuteFramesCount =
                        framerateRounded * 60 - droppedFramesCount

                    tenMinutesFramesCount =
                        (10 * oneMinuteFramesCount) - droppedFramesCount

                    tenMinutesChunks =
                        frameNumber / tenMinutesFramesCount

                    oneMinuteChunks =
                        modBy (round tenMinutesFramesCount) (round frameNumber)

                    tenMinutesPart =
                        (droppedFramesCount * 10 - droppedFramesCount) * tenMinutesChunks

                    oneMinutePart =
                        droppedFramesCount * ((toFloat oneMinuteChunks - droppedFramesCount) / oneMinuteFramesCount)
                in
                frameNumber + tenMinutesPart + (if oneMinutePart < 0 then 0 else oneMinutePart)
            else
                frameNumber

        doubleDigit =
            String.fromInt >> String.padLeft 2 '0'
    in
    { hours     = doubleDigit hours_
    , minutes   = doubleDigit minutes_
    , seconds   = doubleDigit seconds_
    , frames    = doubleDigit frames_
    , fps       = framerateAsString framerate
    , dropFrame = dropFrame
    }


{-| Get Timecode from frame number -}
fromFrameNumber : Float -> Framerate -> Bool -> Result TimecodeError Timecode
fromFrameNumber frameNumber framerate dropFrame =
    if dropFrame && not (supportsDropFrame framerate) then
        Err DropFrameSupport
    else
        Ok <| Timecode
            { frameNumber = frameNumber
            , framerate   = framerate
            , dropFrame   = dropFrame
            }


{-| Get Timecode from Display value -}
fromDisplay : Display -> Result TimecodeError Timecode
fromDisplay display_ =
    Maybe.map5
        (\hours_ minutes_ seconds_ frames_ framerate_ ->
            let
                isOutOfBound =
                    let
                        hhOutOfBound = hours_   < 0 || hours_   >  23
                        mmOutOfBound = minutes_ < 0 || minutes_ >  59
                        ssOutOfBound = seconds_ < 0 || seconds_ >  59
                        ffOutOfBound = frames_  < 0 || frames_  >= framerateRounded
                        frameDropped = display_.dropFrame && seconds_ == 0 && modBy 10 (round minutes_) /= 0 && frames_ < droppedFramesCount
                    in
                    hhOutOfBound || mmOutOfBound || ssOutOfBound || ffOutOfBound || frameDropped

                framerateInfo =
                    getFramerateInfo framerate_

                framerateRounded =
                    framerateInfo.frames

                droppedFramesCount =
                    if display_.dropFrame then
                        framerateInfo.droppedFrames
                        |> Maybe.withDefault 0
                    else
                        0

                hourFramesCount =
                    framerateRounded * 3600

                minuteFramesCount =
                    framerateRounded * 60

                totalMinutes =
                    (60 * hours_) + minutes_

                frameNumber =
                    hourFramesCount * hours_ + minuteFramesCount * minutes_ + framerateRounded * seconds_ + frames_ - (droppedFramesCount * (totalMinutes - (totalMinutes / 10)))
            in
            if display_.dropFrame && not (supportsDropFrame framerate_) then
                Err DropFrameSupport
            else if isOutOfBound then
                Err InvalidTimecode
            else
                Ok <| Timecode
                    { frameNumber = frameNumber
                    , framerate   = framerate_
                    , dropFrame   = display_.dropFrame
                    }
        )
        (String.toFloat display_.hours)
        (String.toFloat display_.minutes)
        (String.toFloat display_.seconds)
        (String.toFloat display_.frames)
        (framerateFromString display_.fps)
        |> Maybe.withDefault (Err InvalidTimecode)


{-| Get Timecode from a String which contains the framerate: hh:mm:ss:ff@FPS -}
fromString : String -> Result TimecodeError Timecode
fromString str =
    let
        regex =
            Regex.fromString "^([012]\\d):(\\d\\d):(\\d\\d)(:|;|\\.)(\\d\\d)@([\\d\\.]+)$"
            |> Maybe.withDefault Regex.never

        matches =
            Regex.find regex str
            |> List.head
            |> Maybe.map .submatches
            |> Maybe.withDefault []
    in
    case matches of
        Just hours_ :: Just minutes_ :: Just seconds_ :: Just dropFrame_ :: Just frames_ :: Just framerate_ :: [] ->
            fromDisplay
                { hours     = hours_
                , minutes   = minutes_
                , seconds   = seconds_
                , frames    = frames_
                , fps       = framerate_
                , dropFrame = dropFrame_ == ";"
                }

        _ ->
            Err InvalidTimecode


{-| Get Timecode from a String (hh:mm:ss:ff) and provided Framerate -}
withFramerateFromString : Framerate -> String -> Result TimecodeError Timecode
withFramerateFromString framerate str =
    fromString (str ++ "@" ++ framerateAsString framerate)


{-| Perform operation on frames -}
add : (Framerate -> Float) -> Timecode -> Timecode
add fn (Timecode tc) =
    Timecode
        { frameNumber = tc.frameNumber + fn tc.framerate |> round |> toFloat
        , framerate = tc.framerate
        , dropFrame = tc.dropFrame
        }


{-| Number of frames in hours

```
Timecode.add (Timecode.hours 10) myTC
```
-}
hours : Float -> Framerate -> Float
hours unit framerate =
    framerateAsFloat framerate * 3600 * unit


{-| Number of frames in minutes

```
Timecode.add (Timecode.minutes 10) myTC
```
-}
minutes : Float -> Framerate -> Float
minutes unit framerate =
    framerateAsFloat framerate * 60 * unit


{-| Number of frames in seconds

```
Timecode.add (Timecode.seconds 10) myTC
```
-}
seconds : Float -> Framerate -> Float
seconds unit framerate =
    framerateAsFloat framerate * unit


{-| Number of frames in frames

```
Timecode.add (Timecode.frames 10) myTC
```
-}
frames : Float -> Framerate -> Float
frames unit _ =
    unit


{-| Get Timecode as a formatted String: `hh:mm:ss;ff` -}
format : Timecode -> String
format tc =
    let
        display_ = display tc
    in
    [ display_.hours
    , ":"
    , display_.minutes
    , ":"
    , display_.seconds
    , if display_.dropFrame then ";" else ":"
    , display_.frames
    ]
    |> String.concat


{-| Get Timecode and Framerate as a formatted String: `hh:mm:ss;ff@FPS` -}
fullFormat : Timecode -> String
fullFormat tc =
    let
        display_ = display tc
    in
    [ display_.hours
    , ":"
    , display_.minutes
    , ":"
    , display_.seconds
    , if display_.dropFrame then ";" else ":"
    , display_.frames
    , "@"
    , display_.fps
    ]
    |> String.concat


{-| Get frame number from Timecode -}
getFrameNumber : Timecode -> Float
getFrameNumber (Timecode { frameNumber, framerate, dropFrame }) =
    frameNumber


{-| Get framerate from Timecode -}
getFramerate : Timecode -> Framerate
getFramerate (Timecode { frameNumber, framerate, dropFrame }) =
    framerate


{-| Get drop frame from Timecode -}
getDropFrame : Timecode -> Bool
getDropFrame (Timecode { frameNumber, framerate, dropFrame }) =
    dropFrame


{-| Get time from Timecode -}
getTime : Timecode -> Float
getTime (Timecode { frameNumber, framerate, dropFrame }) =
    framerateAsFloat framerate * frameNumber


{-| Get Framerate as a decimal, as precise as possible -}
framerateAsFloat : Framerate -> Float
framerateAsFloat framerate =
    let
        framerateInfo = getFramerateInfo framerate
    in
    framerateInfo.frames / framerateInfo.second


{-| Get Framerate as String -}
framerateAsString : Framerate -> String
framerateAsString framerate =
    case framerate of
        FPS_24 -> "24"
        FPS_25 -> "25"
        FPS_30 -> "30"
        FPS_50 -> "50"
        FPS_60 -> "60"
        FPS_23_98 -> "23.98"
        FPS_29_97 -> "29.97"
        FPS_59_94 -> "59.94"


{-| Get Framerate from a decimal -}
framerateFromFloat : Float -> Maybe Framerate
framerateFromFloat framerate =
    if framerate == 24 then Just FPS_24
    else if framerate == 25 then Just FPS_25
    else if framerate == 30 then Just FPS_30
    else if framerate == 50 then Just FPS_50
    else if framerate == 60 then Just FPS_60
    else if framerate >= 23.97 && framerate <= 23.98 then Just FPS_23_98
    else if framerate >= 29.97 && framerate <  29.98 then Just FPS_29_97
    else if framerate >= 59.94 && framerate <  59.95 then Just FPS_59_94
    else Nothing


{-| Get Framerate as a decimal -}
framerateFromString : String -> Maybe Framerate
framerateFromString framerate =
    String.toFloat framerate
    |> Maybe.andThen framerateFromFloat


{-| Does the Framerate supports drop frame? -}
supportsDropFrame : Framerate -> Bool
supportsDropFrame framerate =
    getFramerateInfo framerate
    |> .droppedFrames
    |> (/=) Nothing

