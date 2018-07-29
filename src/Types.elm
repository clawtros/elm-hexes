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


type GameState
    = NotStarted
    | PlayerTurn Side
    | PlayerWon Side
    | FirstTurn
    | OptionToChoose


type TileState
    = Filled Side
    | Empty


type Msg
    = TileClick Int Int
    | Reset
    | CongratulateWinner Side
    | SetCells Int


type alias BoardState =
    Dict ( Int, Int ) TileState


type alias Model =
    { state : GameState
    , tiles : BoardState
    , lastPath : List ( Int, Int )
    , message : String
    , cells : Int
    }
