module Types exposing (..)

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
    Dict ( Int, Int ) TileState


type alias Model =
    { currentPlayer : Side
    , tiles : BoardState
    , lastPath : List ( Int, Int )
    , cells : Int
    }
