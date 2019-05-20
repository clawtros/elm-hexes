module Main exposing (..)

import Browser
import Dict
import GameBoard exposing (..)
import Html exposing (Html, button, div, h1)
import Html.Attributes exposing (classList)
import Html.Events
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


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a


init : ( Model, Cmd Msg )
init =
    ( { currentPlayer = Red
      , tiles = Dict.empty
      , lastPath = []
      , cells = 11
      }
    , Cmd.none
    )


won : Model -> Bool
won model =
    let
        coord =
            case model.currentPlayer of
                Blue ->
                    Tuple.first

                Red ->
                    Tuple.second
    in
    List.all (flip List.any model.lastPath)
        [ rightColour model, coord >> (==) 1, coord >> (==) model.cells ]


setTile : BoardState -> Int -> Int -> TileState -> Result String BoardState
setTile collection x y state =
    case Dict.get ( x, y ) collection of
        Just _ ->
            Err "Attempted to set occupied tile"

        Nothing ->
            Ok <| Dict.insert ( x, y ) state collection


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
    case Dict.get ( x, y ) model.tiles of
        Just tile ->
            case tile of
                Filled Red ->
                    if List.member ( x, y ) model.lastPath then
                        "red"

                    else
                        "rgba(255, 0, 0, 0.7)"

                Filled Blue ->
                    if List.member ( x, y ) model.lastPath then
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
    Dict.get p model.tiles == Just (Filled model.currentPlayer)


updatePath : Model -> ( Int, Int ) -> Model
updatePath model point =
    let
        check leaves_ path =
            case leaves_ of
                [] ->
                    { model | lastPath = path }

                leaf :: leaves ->
                    let
                        newLeaves =
                            List.filter
                                (\p ->
                                    rightColour model p
                                        && not (List.member p path)
                                )
                            <|
                                neighbours leaf
                    in
                    check (leaves ++ newLeaves) <| path ++ newLeaves
    in
    check [ point ] []


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
                , text <| sideToString model.currentPlayer ++ "'s move"
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
