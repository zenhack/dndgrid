module Layer exposing
    ( Layer
    , gridActive
    , gridPassive
    , layer
    , units
    )

{-| Helpers for managing the stacking order of rendered components.
-}

import Html exposing (Attribute)
import Html.Attributes exposing (style)


type Layer
    = Layer Int


{-| Convert a layer into an attribute that can be placed on an html
element to set it's place in the stacking order.
-}
layer : Layer -> Attribute msg
layer (Layer n) =
    style "z-index" (String.fromInt n)


{-| The layer on which grid elements are placed if they can't be interacted
with. Underneat the units.
-}
gridPassive : Layer
gridPassive =
    Layer 1


{-| The layer on which units are placed
-}
units : Layer
units =
    Layer 2


{-| The layer on which grid elements that can be interacted with
are placed. Above the units.
-}
gridActive : Layer
gridActive =
    Layer 3
