module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Grid
import Html exposing (..)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)


type alias IDUnit =
    Int


type Msg
    = ChooseUnit IDUnit
    | ChooseSquare Int Int


type alias Unit =
    { x : Int
    , y : Int
    , name : String
    }


type Model
    = Model
        { currentUnit : Maybe IDUnit
        , units : Dict IDUnit Unit
        }


unitGridItem : ( IDUnit, Unit ) -> Grid.GridItem (Html Msg)
unitGridItem ( id, { x, y, name } ) =
    { item =
        a [ href "#", onClick (ChooseUnit id) ] [ text name ]
    , loc =
        { x = x
        , y = y
        , w = 1
        , h = 1
        }
    }


centered : Html msg -> Html msg
centered item =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "space-around"
        ]
        [ item ]


init =
    Model
        { currentUnit = Nothing
        , units =
            Dict.fromList
                [ ( 1, { x = 1, y = 1, name = "Alice" } )
                , ( 2, { x = 2, y = 2, name = "Bob" } )
                , ( 4, { x = 4, y = 4, name = "Dave" } )
                ]
        }


view : Model -> Html Msg
view (Model m) =
    let
        cells =
            Grid.fromFunction viewCell 4 4

        grid =
            { cells
                | items =
                    cells.items
                        ++ (Dict.toList m.units
                                |> List.map unitGridItem
                           )
            }
    in
    centered <| Grid.view identity grid


viewCell x y =
    Just <|
        div
            [ style "border" "solid black 1px"
            , style "margin" "0px"
            , style "height" "96px"
            , style "width" "96px"
            , onClick (ChooseSquare x y)
            ]
            []


update : Msg -> Model -> Model
update msg (Model m) =
    case msg of
        ChooseUnit id ->
            Model { m | currentUnit = Just id }

        ChooseSquare x y ->
            case m.currentUnit of
                Nothing ->
                    Model m

                Just id ->
                    Model
                        { m
                            | currentUnit = Nothing
                            , units =
                                Dict.update
                                    id
                                    (Maybe.map (\u -> { u | x = x, y = y }))
                                    m.units
                        }


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
