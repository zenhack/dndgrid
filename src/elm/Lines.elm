module Lines exposing (Config, Line, Point, linesToSvg, reverse)

import Html exposing (Html)
import Html.Attributes
import Layer
import Svg exposing (Svg, path, svg)
import Svg.Attributes exposing (d, style)


type alias Line =
    ( Point, List Point )


type alias Point =
    { x : Int
    , y : Int
    }


type alias Config =
    { cellSize : Float
    , gridSize : Point
    }


reverse : Line -> Line
reverse ( x, xs ) =
    case List.reverse (x :: xs) of
        y :: ys ->
            ( y, ys )

        [] ->
            -- impossible
            reverse ( x, xs )


linesToSvg : List (Html.Attribute msg) -> Config -> List Line -> Html msg
linesToSvg attrs config lines =
    svg
        attrs
        (List.map (lineToPath config) lines)


lineToPath : Config -> Line -> Svg msg
lineToPath config line =
    path
        [ style "fill:none;stroke:black;"
        , d (lineToPathStr config line)
        ]
        []


lineToPathStr : Config -> Line -> String
lineToPathStr config ( p, ps ) =
    let
        scale n dim =
            toFloat n

        -- (toFloat n * config.cellSize * toFloat dim) / 100
        point { x, y } =
            String.concat
                [ " "
                , String.fromFloat (scale x config.gridSize.x)
                , ","
                , String.fromFloat (scale y config.gridSize.y)
                ]
    in
    "M " ++ String.concat (List.map point (p :: ps))



-- String.concat <| cmd "m" p :: List.map (\pt -> cmd " l" pt ++ cmd " m" pt) ps
