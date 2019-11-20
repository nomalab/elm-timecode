module Timecode exposing
  ( valid
  , fromSeconds
  , toSeconds
  , countFrames
  , fromFrames
  , add
  , subtract
  )

{-|

# Timecode

  Simplified https://github.com/eoyilmaz/timecode elm port

-}

import Parser exposing (Parser, run, map, oneOf, succeed, (|.), (|=), symbol, int)


{-| separator.
-}
separator : String
separator =
  ":"


{-| drop separator.
-}
dropSeparator : String
dropSeparator =
  ";"


{-| Frame rates with drop frame
-}
dropFrameRates =
  [ 29.97
  , 59.94
  ]


{-| Represents the timecode.
-}
type alias Timecode =
  { frame: Int
  , drop: Bool
  , rate: Float
  }


{-| String representation of the timecode without frame rate
-}
type alias TimecodeString =
  { hour: Int
  , minute: Int
  , second: Int
  , drop: Bool
  , frame: Int
  }


{-| Parser.
-}
timecodeString : Parser TimecodeString
timecodeString =
  succeed TimecodeString
    |= paddedInt 2
    |. symbol separator
    |= paddedInt 2
    |. symbol separator
    |= paddedInt 2
    |= oneOf
      [ map (\_ -> False) (symbol separator)
      , map (\_ -> True) (symbol dropSeparator)
      ]
    |= paddedInt 2


{-| Validate tc string.
-}
valid : String -> Maybe String
valid tc =
  Maybe.map toString (parse tc)


{-| parse tc string.
-}
parse : String -> Maybe TimecodeString
parse tc =
  case run timecodeString tc of
    Ok rslt ->
      Just rslt
    Err err ->
      Nothing


{-| A fixed-length integer padded with zeroes.
  https://github.com/rtfeldman/elm-iso8601-date-strings/blob/master/src/Iso8601.elm
-}
paddedInt : Int -> Parser Int
paddedInt quantity =
    let
        helper str =
            if String.length str == quantity then
                -- StringtoInt works on zero-padded integers
                case String.toInt str of
                    Just intVal ->
                        Parser.succeed intVal
                            |> Parser.map Parser.Done

                    Nothing ->
                        Parser.problem ("Invalid integer: \"" ++ str ++ "\"")

            else
                Parser.chompIf Char.isDigit
                    |> Parser.getChompedString
                    |> Parser.map (\nextChar -> Parser.Loop <| String.append str nextChar)
    in
        Parser.loop "" helper


{-| 0 padded String.fromInt.
-}
pad : Int -> Int -> String
pad n v =
  String.padLeft n '0' (String.fromInt v)


{-| String representation.
-}
toString : TimecodeString -> String
toString tc =
  String.concat
    [ pad 2 tc.hour
    , separator
    , pad 2 tc.minute
    , separator
    , pad 2 tc.second
    , if tc.drop then dropSeparator else separator
    , pad 2 tc.frame
    ]


{-| Count frames from given timcode and framerate.
-}
countFrames_ : Float -> TimecodeString -> Int
countFrames_ rate tc =
  let
    dropFrames =
      if tc.drop then
        -- drop frames = 6% of framerate rounded to nearest int
        ceiling ((toFloat <| ceiling rate) * 0.066666)
      else
        0

    -- taking now integer value of framerate
    fr =
      round rate

    hourFrames =
      fr * 3600

    minFrames =
      fr * 60

    totalMinutes =
      (60 * tc.hour) + tc.minute

    frameNumber =
      ((hourFrames * tc.hour) + (minFrames * tc.minute) + (fr * tc.second) + tc.frame)
      - (dropFrames * (totalMinutes - (totalMinutes // 10)))

  in
    frameNumber + 1


{-| Transform frame based timecode to string representation.
-}
toTimecodeString : Timecode -> TimecodeString
toTimecodeString tc =
  let
    dropFrames =
      if tc.drop then
        round ( (toFloat <| round tc.rate) * 0.066666)
      else
        0

    framesPerHour =
      round ( tc.rate * 3600 )

    framesPer24Hour =
      framesPerHour * 24

    framesPer10Minutes =
      round ( tc.rate * 600 )

    framesPerMinute =
      ((round tc.rate) * 60) - dropFrames

    frameNumber =
      tc.frame - 1

    frameNumberNormalized =
      ( case (frameNumber < 0) of
          True ->  
            frameNumber + framesPer24Hour
          False ->
            frameNumber
      )
        |> remainderBy framesPer24Hour
        |> (\ n ->
          case tc.drop of
            True ->
              let
                d =
                  n // framesPer10Minutes
                m =
                  remainderBy framesPer10Minutes n
              in
                if ( m > dropFrames ) then
                  n + ( dropFrames * 9 * d )
                    + dropFrames * (( m - dropFrames ) // framesPerMinute)
                else
                  n + ( dropFrames * 9 * d )
            False ->
              n
        )
  in 
    TimecodeString
      (((frameNumberNormalized // (round tc.rate)) // 60) // 60)  
      (remainderBy 60 ((frameNumberNormalized // (round tc.rate)) // 60))
      (remainderBy 60 (frameNumberNormalized // (round tc.rate)))
      tc.drop
      (remainderBy (round tc.rate) frameNumberNormalized)


{-| Get a timecode
-}
toTimecode : Float -> String -> Maybe Timecode
toTimecode rate tc =
  let
    tc_ =
      parse tc
  in
    Maybe.map(\t -> Timecode (countFrames_ rate t) t.drop rate)  tc_


{-| Get duration in seconds from timecode string and frame rate
-}
toSeconds : Float -> String -> Maybe Float
toSeconds rate tc =
  Maybe.map (\t -> (toFloat <| countFrames_ rate t) / rate ) <| parse tc


{-| Count frames from timcode string with frame rate
-}
countFrames : Float -> String -> Maybe Int
countFrames rate tc =
  Maybe.map (\t -> countFrames_ rate t) <| parse tc


{-| Convert frame counter to timecode string
-}
fromFrames : Float -> Int -> Maybe String
fromFrames rate frames =
  case frames > 0 of
    True ->
      Timecode frames (List.member rate dropFrameRates) rate
        |> toTimecodeString
        |> toString
        |> Just
    False ->
      Nothing


{-| Convert duration to timecode string
-}
fromSeconds : Float -> Float -> Maybe String
fromSeconds duration rate =
   fromFrames rate  ( truncate (duration * ( toFloat ( ceiling rate ) ) ) )


{-| Operation on frames
-}
op : (Int -> Int -> Int) -> Float -> String -> String -> Maybe String
op f rate tcs1 tcs2 =
  Maybe.map2
    (\tc1 tc2 ->
      Timecode
        ( ( f ( countFrames_ rate tc1 ) ( countFrames_ rate tc2 ) ))
        ( List.member rate dropFrameRates )
        rate
      |> toTimecodeString
      |> toString
    )
    (parse tcs1) (parse tcs2)


{-| Add two timecodes
-}
add : Float -> String -> String -> Maybe String
add rate tcs1 tcs2 =
  op (+) rate tcs1 tcs2

{-| Subtract two timecodes
-}
subtract : Float -> String -> String -> Maybe String
subtract rate tcs1 tcs2 =
  op (-) rate tcs1 tcs2
