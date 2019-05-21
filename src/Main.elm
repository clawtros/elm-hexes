module Main exposing (allPaths, colorAt, emptyHtml, evalBoard, flip, init, main, neighbours, pathAt, pathIsWinning, rightColour, setTile, update, updatePath, updateTiles, view, winningScreen, won)

import Browser
import Dict exposing (Dict)
import GameBoard exposing (..)
import Html exposing (Html, button, div, h1)
import Html.Attributes exposing (classList)
import Html.Events
import Minimax exposing (IntegerExt(..))
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


allPaths : BoardState -> List Path
allPaths state =
    Dict.foldl
        (\location val ( result, visited ) ->
            if List.filter (\v -> v == location) visited /= [] then
                ( result, visited )
            else
                let
                    path =
                        pathAt state val location
                in
                    ( result ++ [ path ], visited ++ Tuple.second path )
        )
        ( [], [] )
        state.tiles
        |> Tuple.first


showIntegerExt : IntegerExt Int -> String
showIntegerExt intext =
    case intext of
        Pos_Inf ->
            "∞"

        Neg_Inf ->
            "-∞"

        Number n ->
            String.fromInt n


evalBoard : BoardState -> Side -> IntegerExt Int
evalBoard state side =
    if
        List.any (pathIsWinning state.size) <|
            List.filter (\( s, _ ) -> s == side) <|
                allPaths state
    then
        Pos_Inf
    else
        Number 0


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


setTile : BoardState -> Int -> Int -> Side -> Result String BoardState
setTile collection x y state =
    case Dict.get ( x, y ) collection.tiles of
        Just _ ->
            Err "Attempted to set occupied tile"

        Nothing ->
            Ok <| updateTiles collection <| Dict.insert ( x, y ) state collection.tiles


updateTiles : BoardState -> Dict ( Int, Int ) Side -> BoardState
updateTiles state tiles =
    { state | tiles = tiles }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init

        TileClick x y ->
            case setTile model.tiles x y model.currentPlayer of
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
                Red ->
                    if List.member ( x, y ) <| Tuple.second model.lastPath then
                        "red"
                    else
                        "rgba(255, 0, 0, 0.7)"

                Blue ->
                    if List.member ( x, y ) <| Tuple.second model.lastPath then
                        "blue"
                    else
                        "rgba(0, 0, 255, 0.7)"

        Nothing ->
            "transparent"


neighbours : ( Int, Int ) -> List ( Int, Int )
neighbours ( x, y ) =
    [ ( 0, -1 ), ( 1, -1 ), ( -1, 0 ), ( 1, 0 ), ( -1, 1 ), ( 0, 1 ) ]
        |> List.map (\( xoff, yoff ) -> ( x + xoff, y + yoff ))


rightColour : Model -> ( Int, Int ) -> Bool
rightColour model p =
    Dict.get p model.tiles.tiles == Just model.currentPlayer


pathAt : BoardState -> Side -> ( Int, Int ) -> Path
pathAt state side point =
    let
        check leaves_ path =
            case leaves_ of
                [] ->
                    -- TODO: whaaa?
                    if path == [] then
                        [ point ]
                    else
                        path

                leaf :: leaves ->
                    let
                        newLeaves =
                            List.filter
                                (\p ->
                                    Dict.get p state.tiles
                                        == Just side
                                        && not (List.member p path)
                                )
                            <|
                                neighbours leaf
                    in
                        check (leaves ++ newLeaves) <| path ++ newLeaves
    in
        ( side, check [ point ] [] )


updatePath : Model -> ( Int, Int ) -> Model
updatePath model point =
    { model | lastPath = pathAt model.tiles model.currentPlayer point }


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
                    [ div [] [ text <| sideToString model.currentPlayer ++ "'s move" ]
                    , div [] [ text <| showIntegerExt <| evalBoard model.tiles model.currentPlayer ]
                    , div []
                        [ text <|
                            String.fromInt <|
                                List.length <|
                                    allPaths model.tiles
                        ]
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
