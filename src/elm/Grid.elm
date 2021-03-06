module Grid exposing (Box, Grid, GridItem, fromFunction, merge, repeat, view)

import Html exposing (..)
import Html.Attributes exposing (style)


type alias Grid a =
    { rows : Int
    , cols : Int
    , items : List (GridItem a)
    }


type alias GridItem a =
    { item : a
    , loc : Box
    }


type alias Box =
    { x : Int
    , y : Int
    , w : Int
    , h : Int
    }


repeat : Int -> Int -> a -> Grid a
repeat x y v =
    fromFunction (\_ _ -> Just v) x y


merge : Grid a -> Grid a -> Grid a
merge a b =
    { rows = max a.rows b.rows
    , cols = max a.cols b.cols
    , items = a.items ++ b.items
    }


fromFunction : (Int -> Int -> Maybe a) -> Int -> Int -> Grid a
fromFunction f w h =
    { rows = h
    , cols = w
    , items =
        List.range 1 w
            |> List.map
                (\x ->
                    List.range 1 h
                        |> List.map
                            (\y ->
                                { loc =
                                    { x = x
                                    , y = y
                                    , w = 1
                                    , h = 1
                                    }
                                , item = f x y
                                }
                            )
                )
            |> List.concat
            |> List.filterMap
                (\{ loc, item } ->
                    item
                        |> Maybe.map (\i -> { loc = loc, item = i })
                )
    }


view : List (Attribute msg) -> Grid (Html msg) -> Html msg
view attrs { rows, cols, items } =
    let
        repeat1Fr n =
            "repeat(" ++ String.fromInt n ++ ", 1fr)"
    in
    div
        ([ style "display" "grid"
         , style "gap" "0px"
         , style "grid-template-rows" (repeat1Fr rows)
         , style "grid-template-columns" (repeat1Fr cols)
         ]
            ++ attrs
        )
        (List.map viewGridItem items)


viewGridItem : GridItem (Html msg) -> Html msg
viewGridItem { item, loc } =
    div
        [ style "grid-row-start" (String.fromInt loc.y)
        , style "grid-row-end" (String.fromInt (loc.y + loc.h))
        , style "grid-column-start" (String.fromInt loc.x)
        , style "grid-column-end" (String.fromInt (loc.x + loc.w))
        ]
        [ item ]
