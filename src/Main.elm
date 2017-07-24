module Main exposing (..)

import Html exposing (Html, div, img, button)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Dict exposing (Dict)


---- MODEL ----


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
    | FirstTurn
    | OptionToChoose


type TileState
    = Filled Side
    | Empty


type alias Model =
    { state : GameState
    , tiles : Dict String TileState
    , message : String
    }


init : ( Model, Cmd Msg )
init =
    ( { state = PlayerTurn Red
      , tiles = Dict.empty
      , message = ""
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = TileClick Int Int
    | Reset


dictKey : Int -> Int -> String
dictKey x y =
    (toString x) ++ "_" ++ (toString y)


setTile : Dict String TileState -> Int -> Int -> TileState -> Dict String TileState
setTile collection x y state =
    let
        key =
            dictKey x y
    in
        case Dict.get key collection of
            Just val ->
                collection

            Nothing ->
                Dict.insert
                    (dictKey x y)
                    state
                    collection


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            { model | tiles = Dict.empty } ! []

        TileClick x y ->
            case model.state of
                PlayerTurn Red ->
                    { model
                        | tiles = setTile model.tiles x y (Filled Red)
                        , state = PlayerTurn Blue
                    }
                        ! []

                PlayerTurn Blue ->
                    { model
                        | tiles = setTile model.tiles x y (Filled Blue)
                        , state = PlayerTurn Red
                    }
                        ! []

                _ ->
                    model ! []



---- VIEW ----


getHexPoints : Float -> String
getHexPoints h =
    List.range 0 5
        |> List.map
            (\n -> (toFloat n) * (2 * pi / 6))
        |> List.map
            (\n -> ( sin n, cos n ))
        |> List.map (\( x, y ) -> (toString (x * h / 2)) ++ "," ++ (toString (y * h / 2)))
        |> String.join " "


hex : Float -> List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
hex h attrs children =
    polygon
        (attrs
            ++ [ class "hex", points <| getHexPoints h ]
        )
        (children)


hexGrid : Model -> Int -> Int -> Html Msg
hexGrid model xsize ysize =
    let
        tilesize =
            1000 / (toFloat (xsize + ysize))
    in
        svg
            [ version "1.1", viewBox ("0 0 1000 1000") ]
        <|
            getGrid model xsize ysize tilesize


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ Html.text model.message
        , hexGrid model 9 9
        ]


getRhombusTransform : Float -> Int -> Int -> String
getRhombusTransform h x y =
    let
        oddRow =
            y % 2 == 0

        xmod =
            h * ((sqrt 3) / 2)

        xOffset =
            (toFloat x)
                * xmod
                + (toFloat y)
                / 2
                * xmod
                + h

        ymod =
            h * 0.75

        yOffset =
            (toFloat y) * ymod + h
    in
        "translate("
            ++ (toString (xOffset))
            ++ ", "
            ++ (toString (yOffset))
            ++ ")"


getColorAt : Model -> Int -> Int -> String
getColorAt model x y =
    case Dict.get (dictKey x y) model.tiles of
        Just tile ->
            case tile of
                Filled Red ->
                    "red"

                Filled Blue ->
                    "blue"

                Empty ->
                    "transparent"

        Nothing ->
            "transparent"


chevronPoints : Float -> Int -> Int -> String
chevronPoints h start len =
    List.range start (start + len)
        |> List.map
            (\n -> (toFloat n) * (2 * pi / 6))
        |> List.map
            (\n -> ( sin n, cos n ))
        |> List.map (\( x, y ) -> (toString (x * h / 2)) ++ "," ++ (toString (y * h / 2)))
        |> String.join " "


chevron : Float -> Border -> Svg Msg
chevron size border =
    let
        color =
            case border of
                Border Red _ ->
                    "red"

                Border Blue _ ->
                    "blue"

                NoBorder ->
                    "transparent"

        ps =
            case border of
                Border _ Up ->
                    chevronPoints (size + 5) 2 2

                Border _ Left ->
                    chevronPoints (size + 5) 4 2

                Border _ Down ->
                    chevronPoints (size + 5) 5 2

                Border _ Right ->
                    chevronPoints (size + 5) 1 2

                _ ->
                    chevronPoints size 0 0
    in
        polyline
            [ strokeWidth "3"
            , stroke color
            , fill "transparent"
            , points ps
            , class <| "chevron " ++ color
            ]
            []


drawBorders : Float -> List Border -> List (Svg Msg)
drawBorders size borders =
    List.map (\border -> chevron size border) borders


getGrid : Model -> Int -> Int -> Float -> List (Svg Msg)
getGrid model xsize ysize size =
    List.range 0 ysize
        |> List.concatMap
            (\y ->
                List.range 0 xsize
                    |> List.map
                        (\x ->
                            g
                                [ transform <| getRhombusTransform size x y
                                ]
                            <|
                                [ hex
                                    size
                                    [ fill <| getColorAt model x y
                                    , onClick <|
                                        TileClick x y
                                    ]
                                    []
                                ]
                                    ++ (drawBorders size
                                            (List.concat
                                                [ if y == 0 then
                                                    [ Border Red Up ]
                                                  else
                                                    []
                                                , if x == xsize then
                                                    [ Border Blue Right ]
                                                  else
                                                    []
                                                , if y == ysize then
                                                    [ Border Red Down ]
                                                  else
                                                    []
                                                , if x == 0 then
                                                    [ Border Blue Left ]
                                                  else
                                                    []
                                                ]
                                            )
                                       )
                        )
            )



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
