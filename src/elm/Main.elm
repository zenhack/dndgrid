module Main exposing (main)

import Browser
import Grid
import Html exposing (..)
import Html.Attributes exposing (style)


centered : Html msg -> Html msg
centered item =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "space-around"
        ]
        [ item ]


init =
    Grid.merge
        (Grid.repeat 4 4 cell)
        (Grid.fromFunction
            (\x y ->
                if x == y then
                    Just (text "1")

                else
                    Just (text "0")
            )
            4
            4
        )


cell =
    div
        [ style "border" "solid black 1px"
        , style "margin" "0px"
        , style "height" "96px"
        , style "width" "96px"
        ]
        []


main =
    Browser.sandbox
        { init = init
        , update = \() m -> m
        , view = Grid.view identity >> centered
        }
