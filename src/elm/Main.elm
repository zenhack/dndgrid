module Main exposing (main)

import Browser
import Grid
import Html exposing (..)
import Html.Attributes exposing (style)


nByM v =
    Grid.fromFunction (\_ _ -> Just v)


theGrid =
    Grid.merge
        (nByM cell 4 4)
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
        { init = theGrid
        , update = \_ _ -> theGrid
        , view = Grid.view identity
        }
