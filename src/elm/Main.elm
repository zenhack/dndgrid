module Main exposing (main)

import Browser
import Grid
import Html exposing (..)


main =
    Browser.sandbox
        { init = ()
        , update = \_ _ -> ()
        , view =
            \_ ->
                Grid.view text
                    { rows = 4
                    , cols = 6
                    , items =
                        [ { item = "One!", loc = { x = 1, y = 2, w = 3, h = 2 } }
                        , { item = "Two!", loc = { x = 1, y = 2, w = 2, h = 1 } }
                        ]
                    }
        }
