module Main exposing (main)

import Browser
import Grid
import Html exposing (..)
import Html.Attributes exposing (style)


centered : Html msg -> Html msg
centered item =
    Grid.view identity
        { rows = 1
        , cols = 3
        , items =
            [ { item = item
              , loc =
                    { x = 2
                    , y = 1
                    , w = 1
                    , h = 1
                    }
              }
            ]
        }


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
