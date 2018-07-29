module Main exposing (..)

import Dict exposing (Dict)
import Types exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (classList)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


---- MODEL ----


init : ( Model, Cmd Msg )
init =
    ( { state = PlayerTurn Red
      , tiles =
            Dict.fromList
                [ ( ( 1, 1 ), Filled Red )
                , ( ( 2, 1 ), Filled Blue )
                , ( ( 1, 2 ), Filled Red )
                , ( ( 2, 2 ), Filled Blue )
                ]
      , message = ""
      , cells = 11
      }
    , Cmd.none
    )


winningSide : Model -> Maybe Side
winningSide model =
    Nothing


nextState : Model -> GameState
nextState model =
    case winningSide model of
        Just side ->
            PlayerWon side

        Nothing ->
            currentSide model
                |> Maybe.map otherSide
                |> Maybe.withDefault Red
                |> PlayerTurn


setTile : BoardState -> Int -> Int -> TileState -> Result String BoardState
setTile collection x y state =
    case Dict.get ( x, y ) collection of
        Just _ ->
            Result.Err "Attempted to set occupied tile"

        Nothing ->
            Ok
                (Dict.insert
                    ( x, y )
                    state
                    collection
                )


otherSide : Side -> Side
otherSide side =
    if side == Red then
        Blue
    else
        Red


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            { model | tiles = Dict.empty } ! []

        TileClick x y ->
            case model.state of
                PlayerTurn side ->
                    case setTile model.tiles x y (Filled side) of
                        Err _ ->
                            model ! []

                        Ok tiles ->
                            { model
                                | tiles = tiles
                                , state =
                                    nextState model
                            }
                                ! []

                _ ->
                    model ! []

        CongratulateWinner side ->
            model ! []

        SetCells n ->
            { model | cells = n } ! []



---- VIEW ----


currentSide : Model -> Maybe Side
currentSide model =
    case model.state of
        PlayerTurn side ->
            Just side

        _ ->
            Nothing


hexPoints : Float -> String
hexPoints h =
    List.range 0 5
        |> List.map
            (\n -> toFloat n * (2 * pi / 6))
        |> List.map
            (\n -> ( sin n, cos n ))
        |> List.map (\( x, y ) -> toString (x * h / 2) ++ "," ++ toString (y * h / 2))
        |> String.join " "


hex : Float -> List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
hex h attrs children =
    polygon
        (attrs
            ++ [ class "hex", points <| hexPoints h ]
        )
        children


hexGrid : Model -> Int -> Int -> Html Msg
hexGrid model xsize ysize =
    let
        tilesize =
            1000 / (toFloat ysize * 1.5 * (sqrt 3))
    in
        svg
            [ Svg.Attributes.class "grid-container"
            , version "1.1"
            , viewBox ("0 0 1000 " ++ (toString <| round tilesize * (ysize + 1)))
            ]
        <|
            grid model xsize ysize tilesize


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ Html.text model.message
        , div [] <|
            List.map
                (\n ->
                    div
                        [ classList
                            [ ( "cell-sel", True )
                            , ( "active", model.cells == n )
                            ]
                        , onClick <| SetCells n
                        ]
                        [ text <| toString n ]
                )
            <|
                List.range 2 15
        , hexGrid model model.cells model.cells
        , div [ class "stats" ] [ text <| toString model.state ]
        ]


rhombusTransform : Float -> Int -> Int -> String
rhombusTransform h x y =
    let
        oddRow =
            y % 2 == 0

        xmod =
            h * (sqrt 3) / 2

        xOffset =
            (toFloat x)
                * xmod
                + toFloat y
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


colorAt : Model -> Int -> Int -> String
colorAt model x y =
    case Dict.get ( x, y ) model.tiles of
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
        |> List.map
            (\( x, y ) ->
                (toString (x * h / 2))
                    ++ ","
                    ++ (toString (y * h / 2))
            )
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


grid : Model -> Int -> Int -> Float -> List (Svg Msg)
grid model xsize ysize size =
    List.range 1 ysize
        |> List.concatMap
            (\y ->
                List.range 1 xsize
                    |> List.map
                        (\x ->
                            g
                                [ transform <| rhombusTransform size (x - 1) (y - 1)
                                ]
                            <|
                                [ hex
                                    size
                                    [ fill <| colorAt model x y
                                    , onClick <|
                                        TileClick x y
                                    ]
                                    []
                                ]
                                    ++ (drawBorders size
                                            (List.concat
                                                [ if y == 1 then
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
                                                , if x == 1 then
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
