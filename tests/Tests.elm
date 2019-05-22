module Tests exposing (all)

import Dict exposing (Dict)
import Expect
import Fuzz exposing (..)
import GameBoard
import Main exposing (allPaths, pathAt, evalBoard, bestMove)
import String
import Test exposing (..)
import Types exposing (..)
import Minimax exposing (IntegerExt(..), Node)


smallBoardState =
    { size = 2, tiles = Dict.fromList [] }


smallBoardStateWithPath =
    { size = 2
    , tiles =
        Dict.fromList
            [ ( ( 0, 0 ), Red )
            , ( ( 1, 0 ), Red )
            , ( ( 0, 1 ), Blue )
            ]
    }


smallBoardStateWithRedWinning =
    { size = 1
    , tiles =
        Dict.fromList
            [ ( ( 0, 0 ), Red )
            , ( ( 0, 1 ), Red )
            , ( ( 1, 0 ), Blue )
            ]
    }


smallBoardStateWithNearRedWin =
    { size = 3
    , tiles =
        Dict.fromList
            [ ( ( 0, 0 ), Red )
            , ( ( 0, 1 ), Red )
            , ( ( 1, 0 ), Blue )
            , ( ( 1, 2 ), Blue )
            ]
    }


aiTests : Test
aiTests =
    describe "test ai / board scoring"
        [ test "eval winning board" <|
            \() ->
                Expect.equal Pos_Inf <|
                    evalBoard smallBoardStateWithRedWinning Red
        , test "eval next move" <|
            \() ->
                let
                    { move } =
                        bestMove smallBoardStateWithRedWinning Red
                in
                    Expect.equal (Just ( ( 0, 2 ), Red )) move
        ]


pathTests : Test
pathTests =
    describe "pathing tests "
        [ test "single point path" <|
            \() ->
                Expect.equal ( Blue, [ ( 0, 1 ) ] ) <|
                    Main.pathAt smallBoardStateWithPath Blue ( 0, 1 )
        , test "pathAt with path" <|
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
                    Main.pathIsWinning 3
                        ( Blue, [ ( 1, 0 ), ( 0, 1 ), ( 1, 2 ) ] )
        , test "no paths should be empty list" <|
            \() ->
                Expect.equal [] <|
                    Main.allPaths smallBoardState
        , test "get all paths" <|
            \() ->
                Expect.equal [ ( Red, [ ( 1, 0 ), ( 0, 0 ) ] ), ( Blue, [ ( 0, 1 ) ] ) ] <|
                    Main.allPaths smallBoardStateWithPath
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
    describe "all tests"
        [ tupleSquareTests
        , pathTests
        , aiTests
        ]
