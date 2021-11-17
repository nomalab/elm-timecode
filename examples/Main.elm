module Main exposing (..)

import Html
import Result
import Timecode exposing (..)


main =
    lines
    [ fromFrameNumber 17982 FPS_29_97 True
        |> Result.map fullFormat
    , fromFrameNumber 17982 FPS_29_97 False
        |> Result.map fullFormat
    , fromString "00:10:00:00@29.97"
        |> Result.map fullFormat
    , fromString "00:10:00;00@29.97"
        |> Result.map fullFormat
    , fromString "00:00:00;00@29.97"
        |> Result.map (add (minutes 10))
        |> Result.map fullFormat
    ]


lines ls =
    Html.div []
        (List.map
            (\o ->
                Html.pre []
                    [ Html.text (Debug.toString o) ]
            )
            ls
        )