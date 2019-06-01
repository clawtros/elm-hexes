module Util exposing (..)

import List
import Set exposing (Set)


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a


uniqueList : List comparable -> List comparable
uniqueList =
    List.foldl
        (\a ( acc, seen ) ->
            if Set.member a seen then
                ( acc, seen )
            else
                ( a :: acc, Set.insert a seen )
        )
        ( [], Set.empty )
        >> Tuple.first


centerDistance : Int -> ( Int, Int ) -> Float
centerDistance size ( x, y ) =
    let
        hs =
            (toFloat size) / 2

        dx =
            toFloat x - hs

        dy =
            toFloat y - hs
    in
        sqrt <| dx * dx + dy * dy
