module Main exposing (view, update, init)

import Dict
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
    ( { state = PlayerTurn Red
      , tiles = Dict.empty
      , lastPath = []
      , cells = 11
      }
    , Cmd.none
    )


winningSide : Model -> Maybe Side
winningSide model =
    let
        currentPath =
            model.lastPath

        side =
            currentSide model

        ( start, end ) =
            if side == Red then
                ( Border Red Up, Border Red Down )
            else
                ( Border Blue Left, Border Blue Right )

        borderMap =
            List.concatMap (\( x, y ) -> borders model.cells x y) currentPath
    in
        case
            ( elemOf borderMap start
            , elemOf borderMap end
            )
        of
            ( True, True ) ->
                Just side

            ( _, _ ) ->
                Nothing


nextState : Model -> GameState
nextState model =
    case winningSide model of
        Just side ->
            PlayerWon side

        Nothing ->
            currentSide model
                |> otherSide
                |> PlayerTurn


setTile : BoardState -> Int -> Int -> TileState -> Result String BoardState
setTile collection x y state =
    case Dict.get ( x, y ) collection of
        Just _ ->
            Result.Err "Attempted to set occupied tile"

        Nothing ->
            Ok <|
                Dict.insert
                    ( x, y )
                    state
                    collection


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
            init

        TileClick x y ->
            case model.state of
                PlayerTurn side ->
                    case setTile model.tiles x y (Filled side) of
                        Err _ ->
                            model ! []

                        Ok tiles ->
                            -- FIXME: messy
                            let
                                nextModel =
                                    { model
                                        | lastPath =
                                            path
                                                model.tiles
                                                (currentSide model)
                                                ( x, y )
                                    }
                            in
                                { nextModel
                                    | tiles = tiles
                                    , state =
                                        nextState nextModel
                                }
                                    ! []

                _ ->
                    model ! []

        SetCells n ->
            { model | cells = n } ! []


currentSide : Model -> Side
currentSide model =
    -- FIXME: Make illegal states unrepresentable
    case model.state of
        PlayerTurn side ->
            side

        _ ->
            Red


colorAt : Model -> Int -> Int -> String
colorAt model x y =
    case Dict.get ( x, y ) model.tiles of
        Just tile ->
            case tile of
                Filled Red ->
                    if List.any ((==) ( x, y )) model.lastPath then
                        "red"
                    else
                        "rgba(255, 0, 0, 0.7)"

                Filled Blue ->
                    if List.any ((==) ( x, y )) model.lastPath then
                        "blue"
                    else
                        "rgba(0, 0, 255, 0.7)"

                Empty ->
                    "transparent"

        Nothing ->
            "transparent"


neighbours : BoardState -> ( Int, Int ) -> List ( Int, Int )
neighbours board ( x, y ) =
    [ ( 0, -1 ), ( 1, -1 ), ( -1, 0 ), ( 1, 0 ), ( -1, 1 ), ( 0, 1 ) ]
        |> List.map
            (\( xoff, yoff ) -> ( x + xoff, y + yoff ))
        |> List.filter
            (\c -> Dict.get c board |> (/=) Nothing)



-- support language variants


neighbors =
    neighbours


path : BoardState -> Side -> ( Int, Int ) -> List ( Int, Int )
path board side ( x, y ) =
    let
        pathAcc board side point visited =
            let
                nears =
                    validNeighbors visited point
            in
                if nears == [] then
                    visited
                else
                    List.foldl
                        (\point visited ->
                            pathAcc board side point (point :: visited)
                        )
                        visited
                        nears

        validNeighbors visited ( x_, y_ ) =
            neighbours board ( x_, y_ )
                |> List.filter
                    (\p ->
                        case Dict.get p board of
                            Just (Filled s) ->
                                s == side

                            _ ->
                                False
                    )
                |> List.filter (elemOf visited >> not)
    in
        ( x, y ) :: pathAcc board side ( x, y ) []


elemOf : List a -> a -> Bool
elemOf l e =
    List.filter ((==) e) l
        |> List.head
        |> (/=) Nothing


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
    div [ class "container" ]
        [ case model.state of
            PlayerWon winner ->
                winningScreen winner

            _ ->
                emptyHtml
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
        , div [ class "stats" ]
            [ text <|
                case model.state of
                    PlayerTurn side ->
                        toString side ++ "'s move"

                    _ ->
                        ""
            ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
