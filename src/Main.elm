module Main exposing (allPaths, bestMove, colorAt, emptyHtml, evalBoard, flip, init, main, neighbours, pathAt, pathIsWinning, setTile, update, updateTiles, view, winningScreen, won)

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


evalBoard : BoardState -> Side -> Int
evalBoard state side =
    let
        paths =
            allPaths state

        yourPaths =
            Debug.log "YP" <|
                List.filter (\( s, _ ) -> s == side) paths

        winningPaths =
            List.filter (pathIsWinning state.size) paths

        winningScore =
            case winningPaths of
                ( side_, _ ) :: rest ->
                    if side_ == side then
                        10000

                    else
                        -10000

                _ ->
                    0
    in
    winningScore + List.length yourPaths


notSide : Side -> Side
notSide side =
    case side of
        Red ->
            Blue

        Blue ->
            Red


bestMove : BoardState -> Side -> Node BoardState ( ( Int, Int ), Side )
bestMove state side =
    let
        moveFunc : Node BoardState ( ( Int, Int ), Side ) -> ( ( Int, Int ), Side ) -> BoardState
        moveFunc node taken =
            let
                bs =
                    node.position

                ( pos, s ) =
                    taken
            in
            { bs | tiles = Dict.insert pos s node.position.tiles }

        valueFunc : Node BoardState ( ( Int, Int ), Side ) -> Int
        valueFunc node =
            evalBoard node.position side
                |> (*)
                    (if node.nodeType == Min then
                        -1

                     else
                        1
                    )

        possibleMovesFunc : Node BoardState ( ( Int, Int ), Side ) -> List ( ( Int, Int ), Side )
        possibleMovesFunc node =
            tupleSquare (node.position.size - 1)
                |> List.map
                    (\( a, b ) ->
                        ( ( a, b )
                        , if node.nodeType == Min then
                            notSide side

                          else
                            side
                        )
                    )
                |> List.filter
                    (\( p, _ ) ->
                        not
                            (List.member p <|
                                Dict.keys node.position.tiles
                            )
                    )
    in
    Minimax.minimax moveFunc valueFunc possibleMovesFunc state 2


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a


init : ( Model, Cmd Msg )
init =
    ( { currentPlayer = Red
      , boardState = { size = 4, tiles = Dict.empty }
      , cells = 4
      , vsAi = True
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
        [ coord >> (==) 0
        , coord >> (==) (boardSize - 1)
        ]


won : Model -> Maybe Side
won model =
    allPaths model.boardState
        |> List.filter (pathIsWinning model.cells)
        |> List.head
        |> Maybe.map Tuple.first


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
            case setTile model.boardState x y model.currentPlayer of
                Err _ ->
                    ( model, Cmd.none )

                Ok tiles ->
                    let
                        newModel =
                            { model | boardState = updateTiles model.boardState tiles.tiles }

                        newerModel =
                            if not model.vsAi then
                                { newModel | currentPlayer = notSide model.currentPlayer }

                            else
                                let
                                    { move } =
                                        Debug.log "move" <| bestMove newModel.boardState Blue
                                in
                                case move of
                                    Just ( ( x_, y_ ), _ ) ->
                                        { model
                                            | boardState =
                                                updateTiles
                                                    newModel.boardState
                                                <|
                                                    Dict.insert ( x_, y_ ) Blue newModel.boardState.tiles
                                        }

                                    Nothing ->
                                        newModel
                    in
                    ( newerModel, Cmd.none )

        SetCells n ->
            ( { model | cells = n }, Cmd.none )


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


neighbours : ( Int, Int ) -> List ( Int, Int )
neighbours ( x, y ) =
    [ ( 0, -1 ), ( 1, -1 ), ( -1, 0 ), ( 1, 0 ), ( -1, 1 ), ( 0, 1 ) ]
        |> List.map (\( xoff, yoff ) -> ( x + xoff, y + yoff ))


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
        _ =
            Debug.log "!" model.boardState

        ( overlay, moveDisplay ) =
            case won model of
                Just side ->
                    ( winningScreen side, emptyHtml )

                Nothing ->
                    ( emptyHtml
                    , emptyHtml
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
        , hexGrid (colorAt <| Debug.log "!!!" model.boardState.tiles) model.boardState.size
        , div [ class "stats" ] [ moveDisplay ]
        , div []
            [ div [] [ text <| sideToString model.currentPlayer ++ "'s move" ]
            , div [] [ text <| String.fromInt <| evalBoard model.boardState model.currentPlayer ]
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
