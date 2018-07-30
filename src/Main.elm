module Main exposing (view, update, init)

import Dict
import Tuple
import Types exposing (..)
import Html exposing (Html, div, button, h1)
import Html.Attributes exposing (classList)
import Svg
    exposing
        ( svg
        , Svg
        , g
        , polyline
        , text
        , polygon
        )
import Svg.Attributes
    exposing
        ( class
        , transform
        , fill
        , stroke
        , points
        , strokeWidth
        , viewBox
        , version
        )
import Html.Events
import GameBoard exposing (..)


---- MODEL ----


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
                    model ! []

                Ok tiles ->
                    let
                        newModel =
                            updatePath { model | tiles = tiles } ( x, y )
                    in
                        if won newModel then
                            newModel ! []
                        else
                            { newModel
                                | currentPlayer =
                                    if model.currentPlayer == Red then
                                        Blue
                                    else
                                        Red
                            }
                                ! []

        SetCells n ->
            { model | cells = n } ! []


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
        check leaves path =
            case leaves of
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
            [ h1 [] [ text <| toString winner ++ " wins" ]
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
                , text <| toString model.currentPlayer ++ "'s move"
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
                            [ text <| toString n ]
                    )
                <|
                    List.range 7 19
            , hexGrid (colorAt model) model.cells
            , div [ class "stats" ] [ moveDisplay ]
            ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
