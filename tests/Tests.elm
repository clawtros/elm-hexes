module Tests exposing (all)

import Dict exposing (Dict)
import Expect
import Fuzz exposing (..)
import GameBoard
import Main
import String
import Test exposing (..)
import Types exposing (..)


pathTests : Test
pathTests =
    let
        smallBoardState =
            { size = 2, tiles = Dict.fromList [] }

        smallBoardStateWithPath =
            { size = 2
            , tiles =
                Dict.fromList
                    [ ( ( 0, 0 ), Filled Red )
                    , ( ( 1, 0 ), Filled Red )
                    , ( ( 0, 1 ), Filled Blue )
                    ]
            }
    in
    describe "pathing tests "
        [ test "pathAt with path" <|
            \() ->
                Expect.equal ( Red, [ ( 1, 0 ), ( 0, 0 ) ] ) <|
                    Main.pathAt smallBoardStateWithPath Red ( 0, 0 )
        , test "winning path wins" <|
            \() ->
                Expect.equal True <|
                    Main.pathIsWinning 2
                        ( Red, [ ( 1, 0 ), ( 0, 1 ), ( 1, 2 ) ] )
        , test "hue matters" <|
            \() ->
                Expect.equal False <|
                    Main.pathIsWinning 2
                        ( Blue, [ ( 1, 0 ), ( 0, 1 ), ( 1, 2 ) ] )
        ]


tupleSquareTests : Test
tupleSquareTests =
    describe "tupleSquare tests"
        [ test "zero size tupleSquare should be origin" <|
            \() ->
                Expect.equal [ ( 0, 0 ) ] <| GameBoard.tupleSquare 0
        , test "tuple square should generate all points" <|
            \() ->
                Expect.equal [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ) ] <| GameBoard.tupleSquare 1
        , test "tuple square should be square" <|
            \() ->
                Expect.equal (List.length <| GameBoard.tupleSquare 2) 9
        , fuzz (Fuzz.intRange 1 100) "square should be square count of size" <|
            \n ->
                Expect.equal (List.length <| GameBoard.tupleSquare n) ((1 + n) * (1 + n))
        ]


all : Test
all =
    describe "GameBoard tests"
        [ tupleSquareTests
        , pathTests
        ]
