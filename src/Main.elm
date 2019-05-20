module Main exposing (colorAt, emptyHtml, evalBoard, flip, init, main, neighbours, rightColour, setTile, update, updatePath, view, winningScreen, won)

import Browser
import Dict exposing (Dict)
import GameBoard exposing (..)
import Html exposing (Html, button, div, h1)
import Html.Attributes exposing (classList)
import Html.Events
import Minimax
import Svg
    exposing
        ( Svg
        , g
        , polygon
        , polyline
        , svg
        , text
        )
import Svg.Attributes
    exposing
        ( class
        , fill
        , points
        , stroke
        , strokeWidth
        , transform
        , version
        , viewBox
        )
import Tuple
import Types exposing (..)



---- MODEL ----


evalBoard : BoardState -> Side -> Int
evalBoard state side =
    0


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a


init : ( Model, Cmd Msg )
init =
    ( { currentPlayer = Red
      , tiles = { size = 4, tiles = Dict.empty }
      , lastPath = ( Red, [] )
      , cells = 4
      }
    , Cmd.none
    )


pathIsWinning : Int -> Path -> Bool
pathIsWinning boardSize ( side, path ) =
    let
        coord =
            case side of
                Blue ->
                    Tuple.first

                Red ->
                    Tuple.second
    in
    List.all (flip List.any path)
        [ coord >> (==) 1
        , coord >> (==) boardSize
        ]


won : Model -> Bool
won model =
    pathIsWinning model.cells model.lastPath


setTile : BoardState -> Int -> Int -> TileState -> Result String BoardState
setTile collection x y state =
    case Dict.get ( x, y ) collection.tiles of
        Just _ ->
            Err "Attempted to set occupied tile"

        Nothing ->
            Ok <| updateTiles collection <| Dict.insert ( x, y ) state collection.tiles


updateTiles : BoardState -> Dict ( Int, Int ) TileState -> BoardState
updateTiles state tiles =
    { state | tiles = tiles }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init

        TileClick x y ->
            case setTile model.tiles x y (Filled model.currentPlayer) of
                Err _ ->
                    ( model, Cmd.none )

                Ok tiles ->
                    let
                        newModel =
                            updatePath { model | tiles = tiles } ( x, y )
                    in
                    if won newModel then
                        ( newModel, Cmd.none )

                    else
                        ( { newModel
                            | currentPlayer =
                                if model.currentPlayer == Red then
                                    Blue

                                else
                                    Red
                          }
                        , Cmd.none
                        )

        SetCells n ->
            ( { model | cells = n }, Cmd.none )


colorAt : Model -> Int -> Int -> String
colorAt model x y =
    case Dict.get ( x, y ) model.tiles.tiles of
        Just tile ->
            case tile of
                Filled Red ->
                    if List.member ( x, y ) <| Tuple.second model.lastPath then
                        "red"

                    else
                        "rgba(255, 0, 0, 0.7)"

                Filled Blue ->
                    if List.member ( x, y ) <| Tuple.second model.lastPath then
                        "blue"

                    else
                        "rgba(0, 0, 255, 0.7)"

                Empty ->
                    "transparent"

        Nothing ->
            "transparent"


neighbours : ( Int, Int ) -> List ( Int, Int )
neighbours ( x, y ) =
    [ ( 0, -1 ), ( 1, -1 ), ( -1, 0 ), ( 1, 0 ), ( -1, 1 ), ( 0, 1 ) ]
        |> List.map (\( xoff, yoff ) -> ( x + xoff, y + yoff ))


rightColour : Model -> ( Int, Int ) -> Bool
rightColour model p =
    Dict.get p model.tiles.tiles == Just (Filled model.currentPlayer)


pathAt : BoardState -> Side -> ( Int, Int ) -> List ( Int, Int )
pathAt state side point =
    let
        check leaves_ path =
            case leaves_ of
                [] ->
                    path

                leaf :: leaves ->
                    let
                        newLeaves =
                            List.filter
                                (\p ->
                                    Dict.get p state.tiles
                                        == Just (Filled side)
                                        && not (List.member p path)
                                )
                            <|
                                neighbours leaf
                    in
                    check (leaves ++ newLeaves) <| path ++ newLeaves
    in
    check [ point ] []


updatePath : Model -> ( Int, Int ) -> Model
updatePath model point =
    { model | lastPath = ( model.currentPlayer, pathAt model.tiles model.currentPlayer point ) }


emptyHtml : Html msg
emptyHtml =
    text ""


winningScreen : Side -> Html Msg
winningScreen winner =
    div [ class "popup-container" ]
        [ div [ class "popup-contents" ]
            [ h1 [] [ text <| sideToString winner ++ " wins" ]
            , button [ Html.Events.onClick Reset ] [ text "New Game" ]
            ]
        ]


view : Model -> Html Msg
view model =
    let
        ( overlay, moveDisplay ) =
            if won model then
                ( winningScreen model.currentPlayer, emptyHtml )

            else
                ( emptyHtml
                , div []
                    [ text <| sideToString model.currentPlayer ++ "'s move"
                    , text <| String.fromInt (evalBoard model.tiles model.currentPlayer)
                    ]
                )
    in
    div [ class "container" ]
        [ overlay
        , div [] <|
            List.map
                (\n ->
                    div
                        [ classList
                            [ ( "cell-sel", True )
                            , ( "active", model.cells == n )
                            ]
                        , Html.Events.onClick <| SetCells n
                        ]
                        [ text <| String.fromInt n ]
                )
            <|
                List.range 7 19
        , hexGrid (colorAt model) model.cells
        , div [ class "stats" ] [ moveDisplay ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = always init
        , update = update
        , subscriptions = always Sub.none
        }
