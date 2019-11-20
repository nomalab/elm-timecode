module TimecodeTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Timecode

suite : Test
suite =
  describe "The Timecode module"
    [ describe "Timecode.valid"
        [ test "valid timecode" <|
            \_ ->
              Expect.equal (Just "10:10:10:01") (Timecode.valid "10:10:10:01")
        
        , test "unvalid timecode" <|
            \_ ->
              Expect.equal Nothing (Timecode.valid "1:1:1:1")
        
        , test "unvalid timecode with negative hours" <|
            \_ ->
              Expect.equal Nothing (Timecode.valid "-10:10:10:10")
        
        , test "valid drop frame timecode" <|
            \_ ->
              Expect.equal (Just "10:10:10;23") (Timecode.valid "10:10:10;23")
        ]
    , describe "fromFrames"
        [ test "Timecode.fromFrames 24 1" <|
            \_ ->
              Expect.equal (Just "00:00:00:00") (Timecode.fromFrames 24 1)
        ]
    , describe "countFrames"
      [ test "'00:00:00;00' countFrames equals 1" <|
            \_ ->
              Expect.equal (Just 1) (Timecode.countFrames 29.97 "00:00:00;00")
      
      , test "'00:00:00:00' countFrames equals 1" <|
            \_ ->
              Expect.equal (Just 1) (Timecode.countFrames 29.97 "00:00:00:00")
      
      , test "'00:00:00;01' countFrames equals 2" <|
            \_ ->
              Expect.equal (Just 2) (Timecode.countFrames 29.97 "00:00:00;01")
      
      , test "'03:36:09;23' with 29.97 countFrames equals 388704" <|
            \_ ->
              Expect.equal (Just 388704) (Timecode.countFrames 29.97 "03:36:09;23")
      
      , test "'03:36:09;23' with 30 countFrames equals 389094" <|
            \_ ->
              Expect.equal (Just 389094) (Timecode.countFrames 30.0 "03:36:09:23")
      
      , test "'03:36:09:23' with 25 countFrames equals 389094" <|
            \_ ->
              Expect.equal (Just 324249) (Timecode.countFrames 25.0 "03:36:09:23")
      
      , test "'03:36:09;23' with 59.94 countFrames equals 389094" <|
            \_ ->
              Expect.equal (Just 777384) (Timecode.countFrames 59.94 "03:36:09;23")
      
      , test "'03:36:09:23' with 60 countFrames equals 389094" <|
            \_ ->
              Expect.equal (Just 778164) (Timecode.countFrames 60.0 "03:36:09:23")
      
      , test "'03:36:09:23' with 23.98 countFrames equals 311280" <| -- no drop frame for 23.98 frame rate
            \_ ->
              Expect.equal (Just 311280) (Timecode.countFrames 23.98 "03:36:09:23")
      
      , test "'03:36:09:23' with 24 countFrames equals 311280" <|
            \_ ->
              Expect.equal (Just 311280) (Timecode.countFrames 24.0 "03:36:09:23")
      ]
    , describe "Timecode.fromFrames"
        [ test "Timecode.fromFrames 24 12000" <|
            \_ ->
              Expect.equal (Just "00:08:19:23") (Timecode.fromFrames 24 12000)
        
        , test "Timecode.fromFrames 29.97 2589408" <|
            \_ ->
              Expect.equal (Just "23:59:59;29") (Timecode.fromFrames 29.97 2589408)
        
        , test "Timecode.fromFrames 29.97 2589409" <|
            \_ ->
              Expect.equal (Just "00:00:00;00") (Timecode.fromFrames 29.97 2589409)
        
        , test "Timecode.fromFrames 59.94 5178816" <|
            \_ ->
              Expect.equal (Just "23:59:59;59") (Timecode.fromFrames 59.94 5178816)
        
        , test "Timecode.fromFrames 59.94 5178817" <|
            \_ ->
              Expect.equal (Just "00:00:00;00") (Timecode.fromFrames 59.94 5178817)
        ]

    , describe "23.98 vs 23.976"
        [ test "04:01:45:23@23.98 and 04:01:45:23@23.976 have same number of frames" <|
            \_ ->
              Expect.equal (Timecode.countFrames 23.976 "04:01:45:23") (Timecode.countFrames 23.98 "04:01:45:23")
        ]
    , describe "representation overload"
        [ test "with 01:00:00:00@24" <|
            \_ ->
              Expect.equal (Just <| Just "01:00:00:00") (Maybe.map (Timecode.fromFrames 24) <| Timecode.countFrames 24 "01:00:00:00")
        , test "with 20:00:00:00@23.98" <|
            \_ ->
              Expect.equal (Just <| Just "20:00:00:00") (Maybe.map (Timecode.fromFrames 23.98) <| Timecode.countFrames 23.98 "20:00:00:00")
        , test "with 00:09:00;00@29.97" <|
            \_ ->
              Expect.equal (Just <| Just "00:08:59;28") (Maybe.map (Timecode.fromFrames 29.97) <| Timecode.countFrames 29.97 "00:09:00;00")
        , test "with 00:10:00:00@30" <|
            \_ ->
              Expect.equal (Just <| Just "00:10:00:00") (Maybe.map (Timecode.fromFrames 30) <| Timecode.countFrames 30 "00:10:00:00")
        , test "with 00:00:09:00@60" <|
            \_ ->
              Expect.equal (Just <| Just "00:00:09:00") (Maybe.map (Timecode.fromFrames 60) <| Timecode.countFrames 60 "00:00:09:00")
        , test "with 00:00:20;00@59.94" <|
            \_ ->
              Expect.equal (Just <| Just "00:00:20;00") (Maybe.map (Timecode.fromFrames 59.94) <| Timecode.countFrames 59.94 "00:00:20;00")
        
        , test "with 00:00:20:0059.94" <|
            \_ ->
              Expect.equal (Just <| Just "00:00:20;00") (Maybe.map (Timecode.fromFrames 59.94) <| Timecode.countFrames 59.94 "00:00:20:00")
        ]
    , describe "add"
        [ test "03:36:09;23@29.97 + 00:00:29;23@29.97" <|
            \_ ->
              Expect.equal (Just "03:36:39;17") (Timecode.add 29.97 "03:36:09;23" "00:00:29;23")
        , test "03:36:09:23@30 + 00:00:29:23@30" <|
            \_ ->
              Expect.equal (Just "03:36:39:17") (Timecode.add 30 "03:36:09:23" "00:00:29:23")
        , test "03:36:09:23@25 + 00:00:29:23@25" <|
            \_ ->
              Expect.equal (Just "03:36:39:22") (Timecode.add 25 "03:36:09:23" "00:00:29:23")
        , test "03:36:09;23@59.94 + 00:00:29;23@59.94" <|
            \_ ->
              Expect.equal (Just "03:36:38;47") (Timecode.add 59.94 "03:36:09;23" "00:00:29;23")
        , test "03:36:09:23@60 + 00:00:29:23@60" <|
            \_ ->
              Expect.equal (Just "03:36:38:47") (Timecode.add 60 "03:36:09:23" "00:00:29:23")
        , test "03:36:09:23@23.98 + 00:00:29:23@23.98" <|
            \_ ->
              Expect.equal (Just "03:36:39:23") (Timecode.add 23.98 "03:36:09:23" "00:00:29:23")
        ]
    , describe "subtract"
      [ test "03:36:09:23@29.97 - 00:00:29:23@29.97" <|
          \_ ->
            Expect.equal (Just "03:35:39;27") (Timecode.subtract 29.97 "03:36:09;23" "00:00:29;23")
      , test "03:36:09:23@30 - 00:00:29:23@30" <|
          \_ ->
            Expect.equal (Just "03:35:39:29") (Timecode.subtract 30 "03:36:09:23" "00:00:29:23")
      , test "03:36:09:23@25 - 00:00:29:23@25" <|
          \_ ->
            Expect.equal (Just "03:35:39:24") (Timecode.subtract 25 "03:36:09:23" "00:00:29:23")
      , test "03:36:09;23@59.94 - 00:00:29;23@59.94" <|
          \_ ->
            Expect.equal (Just "03:35:39;55") (Timecode.subtract 59.94 "03:36:09;23" "00:00:29;23")
      , test "03:36:09:23@60 - 00:00:29:23@60" <|
          \_ ->
            Expect.equal (Just "03:35:39:59") (Timecode.subtract 60 "03:36:09:23" "00:00:29:23")
      , test "03:36:09:23@23.98 - 00:00:29:23@23.98" <|
          \_ ->
            Expect.equal (Just "03:35:39:23") (Timecode.subtract 23.98 "03:36:09:23" "00:00:29:23")
      ]
    , describe "24 hour limit in 24fps"
      [ test "00:00:00:21@24 + 23:59:59:23@24" <|
          \_ ->
            Expect.equal (Just "00:00:00:21") (Timecode.add 24 "00:00:00:21" "23:59:59:23")
      , test "23:59:59:23@24 + 159840001 frames = 02:00:00:00" <|
          \_ ->
            Expect.equal (Just <| Just "02:00:00:00") (Maybe.map (Timecode.add 24 "23:59:59:23") (Timecode.fromFrames 24 159840001))
      ]
    , describe "24 hour limit in 29.97fps"
      [ test "00:00:00;21@29.97 + 23:59:59:29@29.97" <|
          \_ ->
            Expect.equal (Just "00:00:00;21") (Timecode.add 29.97 "00:00:00;21" "23:59:59;29")
      , test "23:59:59;29@29.97 + 215785 frames = 02:00:00;00" <|
          \_ ->
            Expect.equal (Just <| Just "02:00:00;00") (Maybe.map (Timecode.add 29.97 "23:59:59;29") (Timecode.fromFrames 29.97 215785))
      , test "23:59:59;29@29.97 + 215785 frames  + 2589408 frames = 02:00:00;00" <|
          \_ ->
            Expect.equal (Just <| Just "02:00:00;00") (Maybe.map (Timecode.add 29.97 "23:59:59;29") (Timecode.fromFrames 29.97 (215785 + 2589408)))
      , test "23:59:59;29@29.97 + 215785 frames + 2589408 frames + 2589408 frames = 02:00:00;00" <|
          \_ ->
            Expect.equal (Just <| Just "02:00:00;00") (Maybe.map (Timecode.add 29.97 "23:59:59;29") (Timecode.fromFrames 29.97 (215785 + 2589408 + 2589408)))
      ]
    , describe "24 hour limit"
      [ test "23:59:59;29@59.94 countFrames equals 5178786" <|
            \_ ->
              Expect.equal (Just 5178786) (Timecode.countFrames 59.94 "23:59:59;29")
      , test "23:59:59;29@29.97 countFrames equals 2589408" <|
            \_ ->
              Expect.equal (Just 2589408) (Timecode.countFrames 29.97 "23:59:59;29")
      , test "2589408 frames @ 29.97 == 23:59:59;29" <|
            \_ ->
              Expect.equal (Just "23:59:59;29") (Timecode.fromFrames 29.97 2589408)
      , test "23:59:59;29@29.97 + 1 frame == 00:00:00;00@29.97" <|
            \_ ->
              Expect.equal (Just <| Just "00:00:00;00") (Maybe.map (Timecode.add 29.97 "23:59:59;29") (Timecode.fromFrames 29.97 1))
      , test "23:59:59;29@29.97 + 21 frames == 00:00:00;00@29.97" <|
            \_ ->
              Expect.equal (Just <| Just "00:00:00;20") (Maybe.map (Timecode.add 29.97 "23:59:59;29") (Timecode.fromFrames 29.97 21))
      , test "23:59:59;29@29.97 + 00:00:00;21 == 00:00:00;21@29.97" <|
            \_ ->
              Expect.equal (Just "00:00:00;21") (Timecode.add 29.97 "23:59:59;29" "00:00:00;21")
      , test "04:20:13;21@29.97  == 467944 frames" <|
            \_ ->
              Expect.equal (Just "04:20:13;21") (Timecode.fromFrames 29.97 467944)
      , test "467944 frames @ 29.97 == 04:20:13;21" <|
            \_ ->
              Expect.equal (Just 467944) (Timecode.countFrames 29.97 "04:20:13;21")
      , test "23:59:59;29@29.97  == 2589408 frames" <|
            \_ ->
              Expect.equal (Just "23:59:59;29") (Timecode.fromFrames 29.97 2589408)
      , test "count 2589408 frames @ 29.97 == 23:59:59;29" <|
            \_ ->
              Expect.equal (Just 2589408) (Timecode.countFrames 29.97 "23:59:59;29")
      , test "04:20:13;21@29.97 + 23:59:59;29 == 04:20:13;21@29.97" <|
            \_ ->
              Expect.equal (Just "04:20:13;21") (Timecode.add 29.97 "04:20:13;21" "23:59:59;29")
      , test "04:20:13;21@59.94 + 23:59:59;59 == 04:20:13;21@29.97" <|
            \_ ->
              Expect.equal (Just "04:20:13;21") (Timecode.add 59.94 "04:20:13;21" "23:59:59;59")
      ]
    ]
