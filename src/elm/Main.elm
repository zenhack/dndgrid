module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Grid
import Html exposing (..)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick, onInput)
import Protocol


gridSize =
    10


type alias IDUnit =
    Int


type Msg
    = ChooseUnit IDUnit
    | ChooseSquare Int Int
    | SetUnitName String
    | DeployUnit
    | GotServerMsg (Result Protocol.Error Protocol.ServerMsg)


type alias Unit =
    { x : Int
    , y : Int
    , name : String
    }


type Model
    = Model
        { currentUnit : Maybe IDUnit
        , units : Dict IDUnit Unit
        , nextUnit :
            { id : IDUnit
            , name : String
            }
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


init : {} -> ( Model, Cmd Msg )
init _ =
    ( Model
        { currentUnit = Nothing
        , units = Dict.empty
        , nextUnit =
            { id = 0
            , name = ""
            }
        }
    , Protocol.connect
    )


view : Model -> Html Msg
view (Model m) =
    let
        cells =
            Grid.fromFunction viewCell gridSize gridSize

        grid =
            { cells
                | items =
                    cells.items
                        ++ (Dict.toList m.units
                                |> List.map unitGridItem
                           )
            }
    in
    div []
        [ centered <|
            div []
                [ input [ onInput SetUnitName ] []
                , button [ onClick DeployUnit ] [ text "Add Unit" ]
                ]
        , centered <| Grid.view identity grid
        ]


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model m) =
    case msg of
        DeployUnit ->
            if m.nextUnit.name == "" then
                ( Model m, Cmd.none )

            else
                ( Model
                    { m
                        | currentUnit = Just m.nextUnit.id
                        , units =
                            Dict.insert
                                m.nextUnit.id
                                { x = 1, y = 1, name = m.nextUnit.name }
                                m.units
                        , nextUnit =
                            { id = m.nextUnit.id + 1
                            , name = ""
                            }
                    }
                , Cmd.none
                )

        SetUnitName name ->
            let
                unit =
                    m.nextUnit
            in
            ( Model { m | nextUnit = { unit | name = name } }
            , Cmd.none
            )

        ChooseUnit id ->
            ( Model { m | currentUnit = Just id }
            , Cmd.none
            )

        ChooseSquare x y ->
            case m.currentUnit of
                Nothing ->
                    ( Model m, Cmd.none )

                Just id ->
                    ( Model
                        { m
                            | currentUnit = Nothing
                            , units =
                                Dict.update
                                    id
                                    (Maybe.map (\u -> { u | x = x, y = y }))
                                    m.units
                        }
                    , Cmd.none
                    )

        GotServerMsg result ->
            Debug.log "result" ( Model m, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Protocol.recv GotServerMsg


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
