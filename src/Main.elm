module Main exposing (..)

import Browser
import Dict exposing (Dict)
import GameBoard exposing (..)
import Html exposing (Html, button, div, h1)
import Html.Attributes exposing (classList)
import Html.Events
import Minimax exposing (IntegerExt(..), Node, NodeType(..))
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
import Task exposing (Task)
import Tuple
import Types exposing (..)
import Set exposing (Set)
import Process
import Evaluator exposing (evalBoard, won, allPaths, bestMove)
import Util exposing (uniqueList)


---- MODEL ----


init : ( Model, Cmd Msg )
init =
    ( { currentPlayer = Red
      , boardState =
            { size = 5
            , tiles = Dict.empty
            }
      , vsAi = True
      , thinking = False
      }
    , Cmd.none
    )


getMoveTask : BoardState -> Side -> Cmd Msg
getMoveTask state side =
    Task.perform GotAiMove (Task.succeed (bestMove state side 5).move)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init

        GotAiMove move_ ->
            case move_ of
                Just move ->
                    let
                        newModel =
                            { model
                                | boardState = addMove model.boardState move

                                --       , currentPlayer = notSide model.currentPlayer
                                , thinking = False
                            }
                    in
                        ( newModel
                          -- ,
                          --     if won newModel.boardState /= Nothing then
                          --     Cmd.none
                          --   else
                          --     Task.perform GotAiMove <|
                          --         getMoveTask newModel.boardState model.currentPlayer
                        , Cmd.none
                        )

                Nothing ->
                    ( model, Cmd.none )

        TileClick x y ->
            if not model.thinking then
                case setTile model.boardState ( ( x, y ), model.currentPlayer ) of
                    Err _ ->
                        ( model, Cmd.none )

                    Ok tiles ->
                        let
                            newModel =
                                { model | boardState = updateTiles model.boardState tiles.tiles }
                        in
                            if not model.vsAi then
                                ( { newModel | currentPlayer = notSide model.currentPlayer }, Cmd.none )
                            else
                                case won newModel.boardState of
                                    Nothing ->
                                        ( Debug.log "NM" { newModel | thinking = True }
                                        , Task.perform (always NoOp) <| Process.sleep 0
                                        )

                                    Just _ ->
                                        ( newModel, Cmd.none )
            else
                ( model, Cmd.none )

        DoAI ->
            ( { model | thinking = True, currentPlayer = notSide model.currentPlayer }
            , getMoveTask model.boardState model.currentPlayer
            )

        SetCells n ->
            let
                boardState =
                    model.boardState

                newBoardState =
                    { boardState | size = n, tiles = Dict.empty }
            in
                ( { model | boardState = newBoardState }, Cmd.none )

        NoOp ->
            ( model
            , getMoveTask model.boardState Blue
            )


colorAt : Dict ( Int, Int ) Side -> Int -> Int -> String
colorAt tiles x y =
    case Dict.get ( x, y ) tiles of
        Just tile ->
            case tile of
                Red ->
                    "red"

                Blue ->
                    "blue"

        Nothing ->
            "transparent"


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
            case won model.boardState of
                Just side ->
                    ( winningScreen side, emptyHtml )

                Nothing ->
                    ( emptyHtml
                    , emptyHtml
                    )
    in
        div
            [ class <|
                "container "
                    ++ if model.thinking then
                        " thinking "
                       else
                        ""
            ]
            [ overlay
            , Html.button [ Html.Events.onClick DoAI ] [ text "GO AI" ]
            , div [] <|
                List.map
                    (\n ->
                        div
                            [ classList
                                [ ( "cell-sel", True )
                                , ( "active", model.boardState.size == n )
                                ]
                            , Html.Events.onClick <| SetCells n
                            ]
                            [ text <| String.fromInt n ]
                    )
                <|
                    List.range 3 11
            , hexGrid (colorAt model.boardState.tiles) model.boardState.size
            , div [ class "stats" ] [ moveDisplay ]
            , div []
                [ div [] [ text <| sideToString model.currentPlayer ++ "'s move" ]
                , div [] [ text <| String.fromInt <| evalBoard model.boardState (notSide model.currentPlayer) ]
                , div []
                    [ text <|
                        String.fromInt <|
                            List.length <|
                                allPaths model.boardState
                    ]
                ]
            ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = always init
        , update = update
        , subscriptions = always Sub.none
        }
