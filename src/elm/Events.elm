module Events exposing (onDragEnter, onDragOver, onDragStart, onDrop)

import Html exposing (Attribute)
import Html.Events exposing (on, preventDefaultOn)
import Json.Decode as D


onDragStart : msg -> Attribute msg
onDragStart msg =
    on "dragstart" (D.succeed msg)


onDragEnter : msg -> Attribute msg
onDragEnter msg =
    preventDefaultOn "dragenter" (D.succeed ( msg, True ))


onDragOver : msg -> Attribute msg
onDragOver msg =
    preventDefaultOn "dragover" (D.succeed ( msg, True ))


onDrop : msg -> Attribute msg
onDrop msg =
    on "drop" (D.succeed msg)
