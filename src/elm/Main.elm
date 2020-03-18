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


{-| A globally unique identifier for a unit.

Ideally the type would be:

       { clientId : Int
       , localId : Int
       }

...but the record version is not comparable, so we can't use it
as a dictionary key. so instead we encode it as (clientId, localId)

-}
type alias UnitID =
    ( Int, Int )


type Msg
    = ChooseUnit UnitID
    | ChooseSquare Protocol.Point
    | SetUnitName String
    | DeployUnit
    | GotServerMsg (Result Protocol.Error Protocol.ServerMsg)


type alias Unit =
    { loc : Protocol.Point
    , name : String
    }


type Model
    = NeedWelcome
    | Ready
        { currentUnit : Maybe UnitID
        , units : Dict UnitID Unit
        , nextUnit :
            { id : Int
            , name : String
            }
        , clientId : Int
        }


unitGridItem : ( UnitID, Unit ) -> Grid.GridItem (Html Msg)
unitGridItem ( id, { loc, name } ) =
    { item =
        a [ href "#", onClick (ChooseUnit id) ] [ text name ]
    , loc =
        { x = loc.x
        , y = loc.y
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
    ( NeedWelcome
    , Protocol.connect
    )


view : Model -> Html Msg
view model =
    case model of
        NeedWelcome ->
            text "Loading..."

        Ready m ->
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
            , onClick (ChooseSquare { x = x, y = y })
            ]
            []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotServerMsg (Ok (Protocol.Welcome { yourClientId, unitInfo })), NeedWelcome ) ->
            ( Ready
                { currentUnit = Nothing
                , units =
                    unitInfo
                        |> List.map
                            (\{ loc, name, id } ->
                                ( ( id.clientId, id.localId )
                                , { loc = loc, name = name }
                                )
                            )
                        |> Dict.fromList
                , nextUnit = { id = 0, name = "" }
                , clientId = yourClientId
                }
            , Cmd.none
            )

        ( _, NeedWelcome ) ->
            Debug.log "Unexpected message" ( NeedWelcome, Cmd.none )

        ( GotServerMsg (Ok (Protocol.UnitMoved { unitId, loc })), Ready m ) ->
            let
                key =
                    ( unitId.clientId, unitId.localId )
            in
            ( Ready
                { m
                    | units =
                        Dict.update
                            key
                            (Maybe.map (\u -> { u | loc = loc }))
                            m.units
                }
            , Cmd.none
            )

        ( DeployUnit, Ready m ) ->
            if m.nextUnit.name == "" then
                ( model, Cmd.none )

            else
                let
                    unitId =
                        ( m.clientId, m.nextUnit.id )

                    loc =
                        { x = 1, y = 1 }
                in
                ( Ready
                    { m
                        | currentUnit =
                            Just unitId
                        , units =
                            Dict.insert
                                unitId
                                { loc = loc, name = m.nextUnit.name }
                                m.units
                        , nextUnit =
                            { id =
                                m.nextUnit.id + 1
                            , name = ""
                            }
                    }
                , Protocol.send <|
                    Protocol.AddUnit
                        { localId = m.nextUnit.id
                        , name = m.nextUnit.name
                        , loc = loc
                        }
                )

        ( SetUnitName name, Ready m ) ->
            let
                unit =
                    m.nextUnit
            in
            ( Ready { m | nextUnit = { unit | name = name } }
            , Cmd.none
            )

        ( ChooseUnit id, Ready m ) ->
            ( Ready { m | currentUnit = Just id }
            , Cmd.none
            )

        ( ChooseSquare loc, Ready m ) ->
            case m.currentUnit of
                Nothing ->
                    ( model, Cmd.none )

                Just (( clientId, localId ) as id) ->
                    ( Ready
                        { m
                            | currentUnit = Nothing
                            , units =
                                Dict.update
                                    id
                                    (Maybe.map (\u -> { u | loc = loc }))
                                    m.units
                        }
                    , Protocol.send <|
                        Protocol.MoveUnit
                            { loc = loc
                            , unitId =
                                { clientId = clientId
                                , localId = localId
                                }
                            }
                    )

        ( GotServerMsg result, _ ) ->
            Debug.log "result" ( model, Cmd.none )


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
