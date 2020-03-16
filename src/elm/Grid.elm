module Grid exposing (Box, Grid, GridItem, view)

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


view : (a -> Html msg) -> Grid a -> Html msg
view viewItem { rows, cols, items } =
    let
        repeat1Fr n =
            "repeat(" ++ String.fromInt n ++ ", 1fr)"
    in
    div
        [ style "display" "grid"
        , style "grid-template-rows" (repeat1Fr rows)
        , style "grid-template-columns" (repeat1Fr cols)
        ]
        (List.map (viewGridItem viewItem) items)


viewGridItem : (a -> Html msg) -> GridItem a -> Html msg
viewGridItem viewItem { item, loc } =
    div
        [ style "grid-row-start" (String.fromInt loc.y)
        , style "grid-row-end" (String.fromInt (loc.y + loc.h))
        , style "grid-column-start" (String.fromInt loc.x)
        , style "grid-column-end" (String.fromInt (loc.x + loc.w))
        ]
        [ viewItem item ]
