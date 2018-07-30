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
import Svg.Events
import Html.Events


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
            -- FIXME: Make illegal states unrepresentable
            Maybe.withDefault Red (currentSide model)

        ( start, end ) =
            if side == Red then
                ( Border Red Up, Border Red Down )
            else
                ( Border Blue Left, Border Blue Right )

        borderMap =
            List.concatMap (\( x, y ) -> borders model.cells model.cells x y) currentPath
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
                |> Maybe.map otherSide
                |> Maybe.withDefault Red
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
                                                (Maybe.withDefault
                                                    Red
                                                 <|
                                                    currentSide model
                                                )
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


currentSide : Model -> Maybe Side
currentSide model =
    -- FIXME: Make illegal states unrepresentable
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
            100

        ( height, width ) =
            rhombusTransform tilesize xsize ysize
    in
        svg
            [ Svg.Attributes.class "grid-container"
            , version "1.1"
            , viewBox
                ("0 0 "
                    ++ (toString height)
                    ++ " "
                    ++ (toString <| width + (tilesize / 2))
                )
            ]
        <|
            grid model xsize ysize tilesize


rhombusTransform : Float -> Int -> Int -> ( Float, Float )
rhombusTransform h x y =
    let
        xmod =
            h * sqrt 3 / 2

        xOffset =
            toFloat x
                * xmod
                + toFloat y
                / 2
                * xmod
                + h

        ymod =
            h * 0.75

        yOffset =
            toFloat y * ymod + h
    in
        ( xOffset, yOffset )


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


chevronPoints : Float -> Int -> Int -> String
chevronPoints h start len =
    List.range start (start + len)
        |> List.map
            (\n -> toFloat n * (2 * pi / 6))
        |> List.map
            (\n -> ( sin n, cos n ))
        |> List.map
            (\( x, y ) ->
                toString (x * h / 2)
                    ++ ","
                    ++ toString (y * h / 2)
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


neighbours : BoardState -> ( Int, Int ) -> List ( Int, Int )
neighbours board ( x, y ) =
    [ ( 0, -1 ), ( 1, -1 ), ( -1, 0 ), ( 1, 0 ), ( -1, 1 ), ( 0, 1 ) ]
        |> List.map
            (\( xoff, yoff ) -> ( x + xoff, y + yoff ))
        |> List.filter
            (\c -> Dict.get c board |> (/=) Nothing)


path : BoardState -> Side -> ( Int, Int ) -> List ( Int, Int )
path board side ( x, y ) =
    ( x, y ) :: pathAcc board side ( x, y ) []


elemOf : List a -> a -> Bool
elemOf l e =
    List.filter ((==) e) l
        |> List.head
        |> (/=) Nothing


pathAcc : BoardState -> Side -> ( Int, Int ) -> List ( Int, Int ) -> List ( Int, Int )
pathAcc board side ( x, y ) visited =
    case
        neighbours board ( x, y )
            |> List.filter
                (\p ->
                    case Dict.get p board of
                        Just (Filled s) ->
                            s == side

                        _ ->
                            False
                )
            |> List.filter (elemOf visited >> not)
    of
        [] ->
            visited

        x_ :: xs ->
            pathAcc board side x_ (x_ :: visited)
                ++ List.concatMap
                    (\p -> pathAcc board side p (p :: (x_ :: visited)))
                    xs


drawBorders : Float -> List Border -> List (Svg Msg)
drawBorders size borders =
    List.map (chevron size) borders


borders : Int -> Int -> Int -> Int -> List Border
borders xsize ysize x y =
    List.concat
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


grid : Model -> Int -> Int -> Float -> List (Svg Msg)
grid model xsize ysize size =
    List.range 1 ysize
        |> List.concatMap
            (\y ->
                List.range 1 xsize
                    |> List.map
                        (\x ->
                            g
                                [ transform <|
                                    "translate"
                                        ++ (toString <| rhombusTransform size (x - 1) (y - 1))
                                ]
                            <|
                                [ hex
                                    size
                                    [ fill <| colorAt model x y
                                    , Svg.Events.onClick <|
                                        TileClick x y
                                    ]
                                    []
                                ]
                                    ++ (drawBorders size <|
                                            borders xsize ysize x y
                                       )
                        )
            )


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
        , hexGrid model model.cells model.cells
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
