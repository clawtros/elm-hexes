module Evaluator exposing (..)

import Types exposing (..)
import Dict exposing (Dict)
import Util exposing (flip, centerDistance, uniqueList)
import Minimax exposing (IntegerExt(..), Node, NodeType(..))
import GameBoard exposing (addMove, tupleSquare, notSide, neighbours, pathAt)


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
                    ( result ++ [ path ], visited ++ Tuple.first path )
        )
        ( [], [] )
        state.tiles
        |> Tuple.first


evalBoard : BoardState -> Side -> Int
evalBoard state side =
    let
        paths =
            allPaths state

        coord =
            case side of
                Blue ->
                    Tuple.first

                Red ->
                    Tuple.second

        yourCells =
            Dict.toList state.tiles
                |> List.filterMap
                    (\( p, s ) ->
                        if s == side then
                            Just p
                        else
                            Nothing
                    )

        allNeighbours =
            List.concatMap (neighbours state.size) yourCells

        emptyNeighbours =
            allNeighbours
                |> List.filter (flip Dict.get state.tiles >> (==) Nothing)

        ( yourPaths, otherPaths ) =
            List.partition (\( _, s ) -> s == side) paths

        winningPaths =
            List.filter (pathIsWinning state.size)

        winningScore =
            (List.length (winningPaths yourPaths)
                |> (*) 100000
            )
                + (List.length (winningPaths otherPaths)
                    |> (*) -100000
                  )
    in
        winningScore
            + (List.length emptyNeighbours
                - List.length (uniqueList emptyNeighbours)
              )


pathIsWinning : Int -> Path -> Bool
pathIsWinning boardSize ( path, side ) =
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


won : BoardState -> Maybe Side
won model =
    allPaths model
        |> List.filter (pathIsWinning model.size)
        |> List.head
        |> Maybe.map Tuple.second


bestMove : BoardState -> Side -> Int -> Node BoardState ( ( Int, Int ), Side )
bestMove state side recursionDepth =
    let
        moveFunc : Node BoardState ( ( Int, Int ), Side ) -> ( ( Int, Int ), Side ) -> BoardState
        moveFunc node taken =
            addMove node.position taken

        valueFunc : Node BoardState ( ( Int, Int ), Side ) -> Int
        valueFunc node =
            evalBoard node.position side

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
        Minimax.minimax moveFunc valueFunc possibleMovesFunc state recursionDepth
