module Main exposing (main)

import Browser
import Dict exposing (Dict)
import File exposing (File)
import File.Select
import Grid
import Html exposing (..)
import Html.Attributes exposing (for, href, name, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Layer
import Protocol
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)



-- MODEL


{-| A globally unique identifier for a unit.

Ideally the type would be:

       { clientId : Int
       , localId : Int
       }

...like Protocol.UnitId, but the record version is not comparable, so we can't
use it as a dictionary key. so instead we encode it as (clientId, localId)

-}
type alias UnitID =
    ( Int, Int )


unitIdFromProtocol : Protocol.UnitId -> UnitID
unitIdFromProtocol { clientId, localId } =
    ( clientId, localId )


unitIdToProtocol : UnitID -> Protocol.UnitId
unitIdToProtocol ( clientId, localId ) =
    { clientId = clientId, localId = localId }


type Msg
    = ChooseUnit UnitID
    | ChooseSquare Protocol.Point
    | SetUnitName String
    | DeployUnit
    | GotServerMsg (Result Protocol.Error Protocol.ServerMsg)
    | SelectedImg ImgPurpose File
    | UploadBgResult (Maybe Http.Error)
    | RequestImg ImgPurpose
    | SetGridWidth String
    | SetGridHeight String
    | SetZoom String
    | SetTab TabId


type ImgPurpose
    = Bg
    | UnitSprite


type alias Unit =
    { loc : Protocol.Point
    , name : String
    }


type TabId
    = AddUnit
    | GridSettings


type Model
    = NeedWelcome
    | Ready ReadyModel


type alias ReadyModel =
    { currentUnit : Maybe UnitID
    , units : Dict UnitID Unit
    , tabId : Maybe TabId
    , nextUnit :
        { id : Int
        , name : String
        , img : Maybe File
        }
    , clientId : Int
    , bgImg : Int
    , gridSize : Protocol.Point
    , zoom : Float
    }


init : {} -> ( Model, Cmd Msg )
init _ =
    ( NeedWelcome
    , Protocol.connect
    )



-- VIEW


cellSizePx : Float
cellSizePx =
    96


zoomPx : Float -> String
zoomPx zoom =
    String.fromInt (floor (zoom * cellSizePx)) ++ "px"


unitGridItem : ( UnitID, Unit ) -> Grid.GridItem (Html Msg)
unitGridItem ( id, { loc, name } ) =
    { item =
        centered <|
            a
                [ href "#"
                , onClick (ChooseUnit id)
                , Layer.layer Layer.units
                ]
                [ text name ]
    , loc =
        { x = loc.x
        , y = loc.y
        , w = 1
        , h = 1
        }
    }


centeredDir : String -> Html msg -> Html msg
centeredDir dir item =
    div
        [ style "display" "flex"
        , style "flex-direction" dir
        , style "align-items" "center"
        , style "justify-content" "space-around"
        ]
        [ item ]


centeredX : Html msg -> Html msg
centeredX =
    centeredDir "row"


centeredY : Html msg -> Html msg
centeredY =
    centeredDir "column"


centered : Html msg -> Html msg
centered =
    centeredX >> centeredY


view : Model -> Html Msg
view model =
    case model of
        NeedWelcome ->
            text "Loading..."

        Ready m ->
            div []
                [ centeredX <| viewTabs m
                , centeredX <| viewGrid m
                ]


viewTabs : ReadyModel -> Html Msg
viewTabs m =
    div [ style "margin" "1rem" ]
        [ centeredX <|
            nav []
                [ ul [ style "list-style" "none" ]
                    [ viewTab m.tabId AddUnit
                    , viewTab m.tabId GridSettings
                    ]
                ]
        , div []
            (List.map (\( t, v ) -> viewTabContents m.tabId t (centeredX v))
                [ ( AddUnit, viewAddUnit )
                , ( GridSettings, viewGridSettings m.gridSize )
                ]
                |> List.concat
            )
        ]


viewGrid : ReadyModel -> Html Msg
viewGrid m =
    let
        cells =
            Grid.fromFunction
                (viewCell m.zoom Layer.gridPassive (div [] []))
                m.gridSize.x
                m.gridSize.y

        cellButtons =
            case m.currentUnit of
                Nothing ->
                    []

                Just _ ->
                    Grid.fromFunction
                        (\x y ->
                            viewCell
                                m.zoom
                                Layer.gridActive
                                (gridButton m.zoom (ChooseSquare { x = x, y = y }))
                                x
                                y
                        )
                        m.gridSize.x
                        m.gridSize.y
                        |> .items

        units =
            Dict.toList m.units
                |> List.map unitGridItem

        grid =
            { cells
                | items =
                    cells.items
                        ++ units
                        ++ cellButtons
            }
    in
    Grid.view
        (bgAttrs m.gridSize m.bgImg)
        grid


bgAttrs : Protocol.Point -> Int -> List (Attribute msg)
bgAttrs gridSize bgImg =
    [ style "background-image" <| "url(\"/bg/" ++ String.fromInt bgImg ++ "/bg.png\")"
    , style "background-size" "contain"
    , style "background-repeat" "no-repeat"
    ]


labeled :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> String
    -> String
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
labeled element inputName labelText attrs kids =
    let
        tableCol =
            div [ style "display" "table-col" ]

        tableRow =
            div [ style "display" "table-row" ]
    in
    tableRow
        [ tableCol [ label [ for inputName ] [ text labelText ] ]
        , tableCol [ element (name inputName :: attrs) kids ]
        ]


tblForm : List (Attribute msg) -> List (Html msg) -> Html msg
tblForm attrs kids =
    form (style "display" "table" :: attrs) kids


tabText : TabId -> String
tabText tabId =
    case tabId of
        AddUnit ->
            "Add Unit"

        GridSettings ->
            "Grid Settings"


viewTab : Maybe TabId -> TabId -> Html Msg
viewTab old new =
    li
        [ style "display" "inline-block"
        , style "margin" "0.25rem"
        ]
        [ a [ href "#", onClick (SetTab new) ] [ text (tabText new) ] ]


viewTabContents : Maybe TabId -> TabId -> Html msg -> List (Html msg)
viewTabContents selected rendering output =
    if selected == Just rendering then
        [ output ]

    else
        []


viewAddUnit : Html Msg
viewAddUnit =
    tblForm []
        [ h1 [] [ text "Add Unit" ]
        , labeled input "name" "Name" [ onInput SetUnitName ] []
        , labeled button "image" "Image" [ onClick (RequestImg UnitSprite) ] [ text "Choose..." ]
        , button [ onClick DeployUnit ] [ text "Add" ]
        ]


viewGridSettings : Protocol.Point -> Html Msg
viewGridSettings { x, y } =
    tblForm []
        [ h1 [] [ text "Grid Settings" ]
        , labeled input
            "height"
            "Grid height"
            [ type_ "number"
            , onInput SetGridHeight
            ]
            []
        , labeled input
            "width"
            "Grid width"
            [ type_ "number"
            , onInput SetGridWidth
            ]
            []
        , labeled button "bg" "Background Image" [ onClick (RequestImg Bg) ] [ text "Choose..." ]
        , hr [] []
        , labeled input "zoom" "Zoom %" [ type_ "number", onInput SetZoom ] []
        ]


viewCell zoom layer contents x y =
    let
        size =
            zoomPx zoom
    in
    Just <|
        div
            [ style "border" "solid black 1px"
            , style "margin" "0px"
            , style "height" size
            , style "width" size
            , Layer.layer Layer.gridPassive
            ]
            [ contents ]


{-| A transparent "button" that we place over a grid cell,
because some browsers (chromium) do not recognize onClick events for div
tags.
-}
gridButton : Float -> msg -> Html msg
gridButton zoom msg =
    let
        cellSize =
            zoomPx zoom
    in
    -- TODO: aria role? I really don't like where this
    -- app is in terms of a11y in general, but given
    -- the whole point is to be a *grid* I'm not sure
    -- how to go about making this useful for folks
    -- who can't see it...
    a
        [ href "#"
        , onClick msg
        , style "height" "100%"
        , style "width" "100%"
        ]
        [ svg
            [ width cellSize
            , height cellSize
            , viewBox <| "0 0 " ++ cellSize ++ " " ++ cellSize
            ]
            []
        ]



-- UPDATE


setGridSize :
    String
    -> ReadyModel
    -> (Int -> Protocol.Point -> Protocol.Point)
    -> ( Model, Cmd Msg )
setGridSize str m updateSize =
    case String.toInt str of
        Nothing ->
            ( Ready m, Cmd.none )

        Just n ->
            if n < 1 then
                ( Ready m, Cmd.none )

            else
                let
                    newSize =
                        updateSize n m.gridSize
                in
                ( Ready { m | gridSize = newSize }
                , Protocol.send <|
                    Protocol.SetGridSize newSize
                )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotServerMsg (Ok serverMsg), _ ) ->
            applyServerMsg serverMsg model

        ( GotServerMsg (Err err), _ ) ->
            Debug.log "server error" ( model, Cmd.none )

        ( _, NeedWelcome ) ->
            Debug.log "unexpected non-server msg" ( model, Cmd.none )

        ( SetTab tabId, Ready m ) ->
            ( Ready
                { m
                    | tabId =
                        if m.tabId == Just tabId then
                            Nothing

                        else
                            Just tabId
                }
            , Cmd.none
            )

        ( SetGridWidth str, Ready m ) ->
            setGridSize str m (\x sz -> { sz | x = x })

        ( SetGridHeight str, Ready m ) ->
            setGridSize str m (\y sz -> { sz | y = y })

        ( SetZoom str, Ready m ) ->
            case String.toInt str of
                Nothing ->
                    ( model, Cmd.none )

                Just n ->
                    ( Ready { m | zoom = toFloat n / 100 }
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
                        | tabId = Nothing
                        , currentUnit =
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
                            , img = Nothing
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

        ( RequestImg purpose, _ ) ->
            ( model
            , File.Select.file [ "image/png" ] (SelectedImg purpose)
            )

        ( SelectedImg UnitSprite file, Ready m ) ->
            let
                nextUnit =
                    m.nextUnit
            in
            ( Ready { m | nextUnit = { nextUnit | img = Just file } }
            , Cmd.none
            )

        ( SelectedImg Bg file, _ ) ->
            ( model
            , Protocol.uploadBg file
                (\res ->
                    UploadBgResult <|
                        case res of
                            Ok () ->
                                Nothing

                            Err e ->
                                Just e
                )
            )

        ( UploadBgResult result, _ ) ->
            Debug.log
                ("upload result: " ++ Debug.toString result)
                ( model, Cmd.none )


applyServerMsg : Protocol.ServerMsg -> Model -> ( Model, Cmd Msg )
applyServerMsg msg model =
    case ( msg, model ) of
        ( Protocol.Welcome { yourClientId, unitInfo, bgImg, gridSize }, _ ) ->
            ( Ready
                { currentUnit = Nothing
                , tabId = Nothing
                , units =
                    unitInfo
                        |> List.map
                            (\{ loc, name, id } ->
                                ( ( id.clientId, id.localId )
                                , { loc = loc, name = name }
                                )
                            )
                        |> Dict.fromList
                , nextUnit =
                    { id = 0
                    , name = ""
                    , img = Nothing
                    }
                , clientId = yourClientId
                , bgImg = bgImg
                , gridSize = gridSize
                , zoom = 1
                }
            , Cmd.none
            )

        ( _, NeedWelcome ) ->
            Debug.log "Unexpected message" ( NeedWelcome, Cmd.none )

        ( Protocol.RefreshBg bg, Ready m ) ->
            ( Ready { m | bgImg = bg }
            , Cmd.none
            )

        ( Protocol.GridSizeChanged sz, Ready m ) ->
            ( Ready { m | gridSize = sz }
            , Cmd.none
            )

        ( Protocol.UnitMoved { unitId, loc }, Ready m ) ->
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

        ( Protocol.UnitAdded { id, loc, name }, Ready m ) ->
            ( Ready
                { m
                    | units =
                        Dict.update
                            (unitIdFromProtocol id)
                            (\_ -> Just { loc = loc, name = name })
                            m.units
                }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Protocol.recv GotServerMsg



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
