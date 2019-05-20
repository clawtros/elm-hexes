module Tests exposing (all)

import Expect
import Fuzz exposing (..)
import GameBoard
import String
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "GameBoard tests"
        [ test "zero size tupleSquare should be origin" <|
            \() ->
                Expect.equal [ ( 0, 0 ) ] <| GameBoard.tupleSquare 0
        , test "tuple square should generate all points" <|
            \() ->
                Expect.equal [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ) ] <| GameBoard.tupleSquare 1
        ,test "tuple square should be square" <|
            \() ->
                Expect.equal (List.length <| GameBoard.tupleSquare 2) 9
        , fuzz (Fuzz.intRange 1 100) "square should be square count of size" <|
            \n ->
                Expect.equal (List.length <| GameBoard.tupleSquare n) ((1 + n) * (1 + n))
        ]
