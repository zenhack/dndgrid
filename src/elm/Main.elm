module Main exposing (main)

import Browser
import Html exposing (..)


main =
    Browser.sandbox
        { init = ()
        , update = \_ _ -> ()
        , view = \_ -> text "Test."
        }
