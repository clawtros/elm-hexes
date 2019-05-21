module Types exposing (BoardState, Border(..), Direction(..), Model, Msg(..), Path, Side(..))

import Dict exposing (Dict)

type alias MoveLocation = (Int, Int)


type Side
    = Red
    | Blue


type Direction
    = Up
    | Down
    | Left
    | Right


type Border
    = Border Side Direction
    | NoBorder


type Msg
    = TileClick Int Int
    | Reset
    | SetCells Int

type alias BoardState =
    { tiles : Dict ( Int, Int ) Side
    , size : Int
    }


type alias Path =
    ( Side, List ( Int, Int ) )


type alias Model =
    { currentPlayer : Side
    , boardState : BoardState
    , cells : Int
    , vsAi : Bool
    }
