module Layer exposing (Layer, bg, gridActive, gridPassive, layer, units)

import Html exposing (Attribute)
import Html.Attributes exposing (style)


type Layer
    = Layer Int


layer : Layer -> Attribute msg
layer (Layer n) =
    style "z-index" (String.fromInt n)


bg : Layer
bg =
    Layer 0


gridPassive : Layer
gridPassive =
    Layer 1


units : Layer
units =
    Layer 2


gridActive : Layer
gridActive =
    Layer 3
