module Events exposing
    ( Point
    , onChange
    , onDragEnter
    , onDragOver
    , onDragStart
    , onDrop
    , onMouseMove
    )

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
    preventDefaultOn "drop" (D.succeed ( msg, True ))


onChange : (String -> msg) -> Attribute msg
onChange mkMsg =
    on "change" (D.map mkMsg (D.field "target" (D.field "value" D.string)))


type alias Point =
    { x : Int
    , y : Int
    }


decodePoint : D.Decoder Point
decodePoint =
    D.map2 Point
        (D.field "offsetX" D.int)
        (D.field "offsetY" D.int)


onMouseMove : (Point -> msg) -> Attribute msg
onMouseMove mkMsg =
    on "mousemove" (D.map mkMsg decodePoint)


{-| Handle the "mousedown" event. `elm/html` has a wrapper for this already,
but it doesn't provide access to the location, so we have our own.
-}
onMouseDown : (Point -> msg) -> Attribute msg
onMouseDown mkMsg =
    on "mousedown" (D.map mkMsg decodePoint)
