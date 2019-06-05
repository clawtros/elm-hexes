module Tests exposing (all)

import Dict exposing (Dict)
import Expect
import Fuzz exposing (..)
import GameBoard exposing (addMove, pathAt, neighbours)
import Main
import Evaluator exposing (allPaths, bestMove, evalBoard, won, pathIsWinning)
import Util exposing (uniqueList)
import Minimax exposing (IntegerExt(..), Node)
import String
import Test exposing (..)
import Types exposing (..)


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


smallBoardStateWithNearWin =
    { size = 3
    , tiles =
        Dict.fromList
            [ ( ( 0, 0 ), Red )
            , ( ( 0, 1 ), Red )
            , ( ( 0, 2 ), Blue )
            , ( ( 2, 2 ), Blue )
            ]
    }


blockableRedWin =
    { size = 3
    , tiles =
        Dict.fromList
            [ ( ( 0, 0 ), Red )
            , ( ( 0, 1 ), Red )
            , ( ( 2, 2 ), Blue )
            , ( ( 2, 1 ), Blue )
            ]
    }


aiTests : Test
aiTests =
    describe "test ai / board scoring"
        [ test "can win" <|
            \() ->
                let
                    move =
                        Maybe.withDefault ( ( -1, -1 ), Red )
                            (bestMove smallBoardStateWithNearWin Blue 3).move
                in
                    Expect.equal (Just Blue) <| won <| addMove smallBoardStateWithNearWin move
        , --     test "can block" <|
          --     \() ->
          --         let
          --             move =
          --                 Maybe.withDefault ( ( -1, -1 ), Red )
          --                     (bestMove smallBoardStateWithNearWin Blue 2).move
          --         in
          --             Expect.equal ( ( 0, 2 ), Blue ) move
          -- ,
          test "near win isn't actually a win" <|
            \() ->
                Expect.equal Nothing <| won smallBoardStateWithNearWin
        , test "uniques" <|
            \() ->
                uniqueList [ 1, 2, 2, 3, 3, 3, 3, 3, 3 ]
                    |> List.sort
                    |> Expect.equal [ 1, 2, 3 ]
        , test "position is boardstate" <|
            \() ->
                Expect.equal (GameBoard.debugBoard smallBoardStateWithNearWin) (bestMove smallBoardStateWithNearWin Blue 2).position
        ]


pathTests : Test
pathTests =
    describe "pathing tests "
        [ test "single point path" <|
            \() ->
                Expect.equal ( [ ( 0, 1 ) ], Blue ) <|
                    pathAt smallBoardStateWithPath Blue ( 0, 1 )
        , test "pathAt with path" <|
            \() ->
                Expect.equal ( [ ( 1, 0 ), ( 0, 0 ) ], Red ) <|
                    pathAt smallBoardStateWithPath Red ( 0, 0 )
        , test "winning path wins" <|
            \() ->
                Expect.equal True <|
                    pathIsWinning 2
                        ( [ ( 1, 0 ), ( 0, 1 ), ( 1, 2 ) ], Red )
        , test "hue matters" <|
            \() ->
                Expect.equal False <|
                    pathIsWinning 3
                        ( [ ( 1, 0 ), ( 0, 1 ), ( 1, 2 ) ], Blue )
        , test "no paths should be empty list" <|
            \() ->
                Expect.equal [] <|
                    allPaths smallBoardState
        , test "get all paths" <|
            \() ->
                Expect.equal [ ( [ ( 1, 0 ), ( 0, 0 ) ], Red ), ( [ ( 0, 1 ) ], Blue ) ] <|
                    allPaths smallBoardStateWithPath
        , test "won " <|
            \() ->
                Expect.equal (Just Red) <|
                    won
                        { size = 3
                        , tiles =
                            Dict.fromList
                                [ ( ( 0, 0 ), Red )
                                , ( ( 0, 1 ), Red )
                                , ( ( 0, 2 ), Red )
                                , ( ( 1, 0 ), Blue )
                                , ( ( 1, 2 ), Blue )
                                ]
                        }
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
        , test "neighbours only playable" <|
            \() ->
                Expect.equal [ ( 1, 0 ), ( 0, 1 ) ] <| neighbours 5 ( 0, 0 )

        -- , test
        --     "can win"
        --   <|
        --     \() ->
        --         let
        --             move =
        --                 Debug.log "MOEE!" <|
        --                     Maybe.withDefault ( ( -1, -1 ), Red )
        --                         (bestMove smallBoardStateWithNearWin Blue 3).move
        --             postMoveState =
        --                 addMove smallBoardStateWithNearWin move
        --         in
        --             Expect.equal (Just Blue) <| won (GameBoard.debugBoard postMoveState)
        ]
