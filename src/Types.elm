module Types exposing (BoardState, Border(..), Direction(..), Model, Msg(..), Path, Side(..), TileState(..))

import Dict exposing (Dict)


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


type TileState
    = Filled Side
    | Empty


type Msg
    = TileClick Int Int
    | Reset
    | SetCells Int


type alias BoardState =
    { tiles : Dict ( Int, Int ) TileState
    , size : Int
    }


type alias Path =
    ( Side, List ( Int, Int ) )


type alias Model =
    { currentPlayer : Side
    , tiles : BoardState
    , lastPath : Path
    , cells : Int
    }
