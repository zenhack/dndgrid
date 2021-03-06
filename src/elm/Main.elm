module Main exposing (main)

import Browser
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Events
import File exposing (File)
import File.Select
import Grid
import Html exposing (..)
import Html.Attributes exposing (disabled, for, href, name, placeholder, selected, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onMouseUp)
import Http
import Layer
import Lines exposing (Line)
import Protocol
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)
import Task



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
    | ChooseSquare (Protocol.Point Int)
    | SetUnitName String
    | SetUnitSize String
    | DeployUnit
    | DeleteUnit UnitID
    | GotServerMsg (Result Protocol.Error Protocol.ServerMsg)
    | SelectedImg ImgPurpose File
    | GotLocalUnitImgData UnitImgData
    | UploadBgResult (Maybe Http.Error)
    | RequestImg ImgPurpose
    | SetGridWidth String
    | SetGridHeight String
    | SetZoom String
    | SetTab TabId
    | ClearBg
      -- Don't do anything. We use this in a couple places where the dom api needs
      -- an event handler, but we don't actually want it to *do* anything:
    | IgnoreMsg
      -- Used for drawing:
    | StartDraw DrawPoint
    | MoveDraw DrawPoint
    | StopDraw
    | ClearDraw


{-| A point (pixel) on the grid.
-}
type alias DrawPoint =
    { cell : { x : Int, y : Int } -- The cell on the grid.
    , px : { x : Float, y : Float } -- coordinates within the cell, in range [0,1]
    }


type alias UnitImgData =
    { file : File
    , url : String
    , bytes : Bytes
    }


type ImgPurpose
    = Bg
    | UnitSprite


type alias Unit =
    { loc : Protocol.Point Int
    , name : String
    , size : Int
    , image : Maybe Int
    }


type TabId
    = AddUnit
    | GridSettings
    | DrawTab


type Model
    = NeedWelcome
    | Ready ReadyModel


type alias ReadyModel =
    { currentUnit : Maybe UnitID
    , units : Dict UnitID Unit
    , tabId : Maybe TabId
    , nextUnit : NextUnitState
    , clientId : Int
    , grid : Protocol.GridInfo
    , zoom : Float
    , draw : Draw
    }


type alias Draw =
    { oldLines : List ( DrawPoint, List DrawPoint )
    , currentLine :
        Maybe
            { pos : DrawPoint
            , points : List DrawPoint
            }
    }


type alias NextUnitState =
    { id : Int
    , name : String
    , size : Int
    , img :
        Maybe
            { file : File
            , data :
                Maybe
                    { url : String
                    , bytes : Bytes
                    }
            }
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


subCellPoint : Float -> Lines.Point -> Lines.Point -> DrawPoint
subCellPoint zoom cellXY pxXY =
    let
        makePx numer =
            toFloat numer / (zoom * cellSizePx)
    in
    { cell = cellXY
    , px =
        { x = makePx pxXY.x
        , y = makePx pxXY.y
        }
    }


unitGridItem : Float -> ( UnitID, Unit ) -> Grid.GridItem (Html Msg)
unitGridItem zoom ( id, { loc, name, size, image } ) =
    let
        unitSize =
            max size 1

        imgSize =
            zoomPx (toFloat unitSize * zoom)

        linkStyle =
            [ -- Black text with a 50% transparent white background. This lets
              -- the unit's image show up underneath while still giving us some
              -- control over the readability of the text.
              style "color" "black"
            , style "font-weight" "bold"
            , style "text-decoration" "none"
            , style "background-color" "rgba(255,255,255,0.5)"

            -- Serifs make things visually too busy at this scale, especially
            -- with the stroke outline.
            , style "font-family" "sans-serif"
            ]
    in
    -- Each unit on the grid is itself layed out using css grid. It uses
    -- a 2x2 grid, with the upper left containing the unit name, the upper
    -- right containing the delete button, and the image taking up all
    -- four cells. This means the text appears above the image.
    { item =
        Grid.view
            [ style "width" <| imgSize
            , style "height" <| imgSize
            , Layer.layer Layer.units
            ]
            { rows = 2
            , cols = 2
            , items =
                [ { loc = { x = 1, y = 1, w = 1, h = 1 }
                  , item =
                        a
                            ([ href "#"
                             , Events.onDragStart (ChooseUnit id)
                             , onClick (ChooseUnit id)
                             , style "display" "block"
                             , style "z-index" "2"
                             , style "position" "relative"
                             ]
                                ++ linkStyle
                            )
                            [ text name ]
                  }
                , { loc = { x = 2, y = 1, w = 1, h = 1 }
                  , item =
                        a
                            ([ href "#"
                             , onClick (DeleteUnit id)
                             , style "display" "block"
                             , style "text-align" "right"
                             , style "z-index" "2"
                             , style "position" "relative"
                             ]
                                ++ linkStyle
                            )
                            [ text "×" ]
                  }
                , { loc = { x = 1, y = 1, w = 2, h = 2 }
                  , item =
                        a
                            [ href "#"
                            , Events.onDragStart (ChooseUnit id)
                            , onClick (ChooseUnit id)
                            , style "z-index" "1"
                            , style "position" "relative"
                            ]
                            [ img
                                [ src <|
                                    (Maybe.map Protocol.imageUrl image
                                        |> Maybe.withDefault "/default-unit.png"
                                    )
                                , style "width" "100%"
                                , style "height" "100%"
                                ]
                                []
                            ]
                  }
                ]
            }
    , loc =
        { x = loc.x
        , y = loc.y
        , w = unitSize
        , h = unitSize
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
                    , viewTab m.tabId DrawTab
                    ]
                ]
        , div []
            (List.map (\( t, v ) -> viewTabContents m.tabId t (centeredX v))
                [ ( AddUnit, viewAddUnit m )
                , ( GridSettings, viewGridSettings m.grid )
                , ( DrawTab, viewDrawTab m.draw )
                ]
                |> List.concat
            )
        ]


viewDrawTab : Draw -> Html Msg
viewDrawTab _ =
    button [ onClick ClearDraw ] [ text "Clear" ]


viewDraw : Float -> Lines.Point -> Draw -> Html msg
viewDraw zoom size { currentLine, oldLines } =
    let
        pxPerCell =
            zoom * cellSizePx

        wh n =
            String.fromInt (floor (toFloat n * pxPerCell)) ++ "px"

        drawLines =
            case currentLine of
                Nothing ->
                    oldLines

                Just { pos, points } ->
                    ( pos, points ) :: oldLines

        rasterizePoint { cell, px } =
            { x = floor <| pxPerCell * (toFloat cell.x + px.x)
            , y = floor <| pxPerCell * (toFloat cell.y + px.y)
            }

        lines =
            drawLines
                |> List.map
                    (\( p, ps ) ->
                        ( rasterizePoint p, List.map rasterizePoint ps )
                    )
    in
    lines
        |> List.map Lines.reverse
        |> Lines.linesToSvg
            [ Layer.layer Layer.gridPassive
            , style "width" (wh size.x)
            , style "height" (wh size.y)
            ]
            { cellSize = cellSizePx
            , gridSize = size
            }


viewGrid : ReadyModel -> Html Msg
viewGrid m =
    let
        draw =
            { item = viewDraw m.zoom m.grid.size m.draw
            , loc =
                { x = 1
                , y = 1
                , w = m.grid.size.x
                , h = m.grid.size.y
                }
            }

        cells =
            Grid.fromFunction
                (viewCell m.zoom Layer.gridPassive (div [] []))
                m.grid.size.x
                m.grid.size.y

        cellButtons =
            case ( m.currentUnit, m.tabId ) of
                ( Just _, _ ) ->
                    Grid.fromFunction
                        (\x y ->
                            viewCell
                                m.zoom
                                Layer.gridActive
                                (gridButton
                                    m.zoom
                                    (unitButtonAttrs (ChooseSquare { x = x, y = y }))
                                )
                                x
                                y
                        )
                        m.grid.size.x
                        m.grid.size.y
                        |> .items

                ( _, Just DrawTab ) ->
                    -- TODO: reduce copypasta.
                    Grid.fromFunction
                        (\x y ->
                            viewCell
                                m.zoom
                                Layer.gridActive
                                (gridButton
                                    m.zoom
                                    (case m.draw.currentLine of
                                        Nothing ->
                                            [ Events.onMouseDown
                                                (\pxXY ->
                                                    StartDraw
                                                        (subCellPoint
                                                            m.zoom
                                                            { x = x - 1, y = y - 1 }
                                                            pxXY
                                                        )
                                                )
                                            ]

                                        Just _ ->
                                            [ Events.onMouseMove
                                                (\pxXY ->
                                                    MoveDraw
                                                        (subCellPoint
                                                            m.zoom
                                                            { x = x - 1, y = y - 1 }
                                                            pxXY
                                                        )
                                                )
                                            , onMouseUp StopDraw
                                            ]
                                    )
                                )
                                x
                                y
                        )
                        m.grid.size.x
                        m.grid.size.y
                        |> .items

                _ ->
                    []

        units =
            Dict.toList m.units
                |> List.map (unitGridItem m.zoom)

        grid =
            { cells
                | items =
                    draw
                        :: cells.items
                        ++ units
                        ++ cellButtons
            }
    in
    div [ style "overflow" "scroll" ]
        [ Grid.view
            (bgAttrs m.grid)
            grid
        ]


bgAttrs : Protocol.GridInfo -> List (Attribute msg)
bgAttrs { bgImg } =
    case bgImg of
        Nothing ->
            []

        Just imgId ->
            [ style "background-image" <| "url(\"" ++ Protocol.imageUrl imgId ++ "\")"
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
    div (style "display" "table" :: attrs) kids


tabText : TabId -> String
tabText tabId =
    case tabId of
        AddUnit ->
            "Add Unit"

        GridSettings ->
            "Grid Settings"

        DrawTab ->
            "Draw"


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


getUnitImgData : NextUnitState -> Maybe UnitImgData
getUnitImgData { img } =
    img
        |> Maybe.andThen
            (\{ file, data } ->
                data
                    |> Maybe.map
                        (\{ url, bytes } ->
                            { file = file
                            , url = url
                            , bytes = bytes
                            }
                        )
            )


viewAddUnit : { a | nextUnit : NextUnitState, zoom : Float } -> Html Msg
viewAddUnit { nextUnit, zoom } =
    let
        imgData =
            getUnitImgData nextUnit
    in
    tblForm [] <|
        List.concat
            [ [ h1 [] [ text "Add Unit" ]
              , labeled input "name" "Name" [ onInput SetUnitName ] []
              , labeled select
                    "size"
                    "Size"
                    [ Events.onChange SetUnitSize
                    ]
                    ([ ( "Small/Medium", 1 )
                     , ( "Large", 2 )
                     , ( "Huge", 3 )
                     , ( "Guargantuan", 4 )
                     , ( "Colossal", 6 )
                     ]
                        |> List.map
                            (\( lbl, val ) ->
                                let
                                    valStr =
                                        String.fromInt val
                                in
                                option
                                    [ selected (val == nextUnit.size)
                                    , value valStr
                                    ]
                                    [ text <| lbl ++ " (" ++ valStr ++ "x" ++ valStr ++ ")" ]
                            )
                    )
              ]
            , imgData
                |> Maybe.map
                    (\{ url } ->
                        let
                            size =
                                zoomPx zoom
                        in
                        [ img
                            [ src url
                            , style "height" size
                            , style "width" size
                            ]
                            []
                        ]
                    )
                |> Maybe.withDefault []
            , [ labeled button "image" "Image" [ onClick (RequestImg UnitSprite) ] [ text "Choose..." ]
              , button
                    [ onClick DeployUnit ]
                    [ text "Add" ]
              ]
            ]


viewGridSettings : Protocol.GridInfo -> Html Msg
viewGridSettings { bgImg, size } =
    tblForm []
        [ h1 [] [ text "Grid Settings" ]
        , labeled input
            "height"
            "Grid height"
            [ type_ "number"
            , placeholder (String.fromInt size.y)
            , onInput SetGridHeight
            ]
            []
        , labeled input
            "width"
            "Grid width"
            [ type_ "number"
            , placeholder (String.fromInt size.x)
            , onInput SetGridWidth
            ]
            []
        , case bgImg of
            Nothing ->
                labeled button "bg" "Background Image" [ onClick (RequestImg Bg) ] [ text "Choose..." ]

            Just _ ->
                labeled button "bg" "Background Image" [ onClick ClearBg ] [ text "Clear" ]
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


unitButtonAttrs : Msg -> List (Attribute Msg)
unitButtonAttrs msg =
    [ onClick msg
    , Events.onDrop msg

    -- We have to define these two to make this a valid drop target, but
    -- we don't actually want to do anything in response:
    , Events.onDragOver IgnoreMsg
    , Events.onDragEnter IgnoreMsg
    ]


{-| A transparent "button" that we place over a grid cell,
so we can use it as a hook for mouse events.

The first argument is the zoom factor for the button.
The second is a list of attributes to add to the clickable
element.

-}
gridButton : Float -> List (Attribute msg) -> Html msg
gridButton zoom attrs =
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
        ([ href "#"
         , style "height" "100%"
         , style "width" "100%"
         ]
            ++ attrs
        )
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
    -> (Int -> Protocol.Point Int -> Protocol.Point Int)
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
                    grid =
                        m.grid

                    newGrid =
                        { grid | size = updateSize n grid.size }
                in
                ( Ready { m | grid = newGrid }
                , Protocol.send <|
                    Protocol.SetGridSize newGrid.size
                )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( IgnoreMsg, _ ) ->
            ( model, Cmd.none )

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
                        , nextUnit =
                            { id = m.nextUnit.id + 1
                            , name = ""
                            , size = m.nextUnit.size
                            , img = Nothing
                            }
                    }
                , Protocol.send <|
                    Protocol.AddUnit
                        { localId = m.nextUnit.id
                        , name = m.nextUnit.name
                        , size = m.nextUnit.size
                        , loc = loc
                        , imageData =
                            getUnitImgData m.nextUnit
                                |> Maybe.map .bytes
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

        ( SetUnitSize str, Ready m ) ->
            case String.toInt str of
                Nothing ->
                    ( Ready m, Cmd.none )

                Just size ->
                    let
                        unit =
                            m.nextUnit
                    in
                    ( Ready { m | nextUnit = { unit | size = size } }
                    , Cmd.none
                    )

        ( DeleteUnit id, Ready m ) ->
            ( Ready { m | units = Dict.remove id m.units }
            , Protocol.send <| Protocol.DeleteUnit (unitIdToProtocol id)
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

        ( ClearDraw, Ready m ) ->
            -- TODO: send a message through the server to do this.
            ( Ready m
            , Protocol.clearDrawing
            )

        ( StartDraw pos, Ready ({ draw } as m) ) ->
            ( Ready
                { m
                    | draw =
                        { draw
                            | currentLine =
                                Just
                                    { pos = pos
                                    , points = []
                                    }
                        }
                }
            , Cmd.none
            )

        ( MoveDraw pos, Ready ({ draw } as m) ) ->
            case draw.currentLine of
                Nothing ->
                    ( Ready m, Cmd.none )

                Just current ->
                    ( Ready
                        { m
                            | draw =
                                { draw
                                    | currentLine =
                                        Just
                                            { pos = pos
                                            , points = current.pos :: current.points
                                            }
                                }
                        }
                    , Cmd.none
                    )

        ( StopDraw, Ready ({ draw } as m) ) ->
            case draw.currentLine of
                Nothing ->
                    ( Ready m, Cmd.none )

                Just { pos, points } ->
                    ( Ready { m | draw = { draw | currentLine = Nothing } }
                    , Protocol.addLine (pos :: points)
                    )

        ( ClearBg, _ ) ->
            ( model, Protocol.clearBg )

        ( RequestImg purpose, _ ) ->
            ( model
            , File.Select.file [ "image/png" ] (SelectedImg purpose)
            )

        ( SelectedImg UnitSprite file, Ready m ) ->
            let
                nextUnit =
                    m.nextUnit
            in
            ( Ready
                { m
                    | nextUnit =
                        { nextUnit
                            | img =
                                Just
                                    { file = file
                                    , data = Nothing
                                    }
                        }
                }
            , File.toUrl file
                |> Task.andThen
                    (\url ->
                        File.toBytes file
                            |> Task.map
                                (\bytes ->
                                    { file = file
                                    , url = url
                                    , bytes = bytes
                                    }
                                )
                    )
                |> Task.perform GotLocalUnitImgData
            )

        ( GotLocalUnitImgData { file, url, bytes }, Ready m ) ->
            let
                nextUnit =
                    m.nextUnit
            in
            ( Ready
                { m
                    | nextUnit =
                        { nextUnit
                            | img =
                                Just
                                    { file = file
                                    , data =
                                        Just
                                            { url = url
                                            , bytes = bytes
                                            }
                                    }
                        }
                }
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
        ( Protocol.Welcome { yourClientId, unitInfo, grid, lines }, _ ) ->
            ( Ready
                { currentUnit = Nothing
                , tabId = Nothing
                , units =
                    unitInfo
                        |> List.map
                            (\{ loc, name, size, id, image } ->
                                ( ( id.clientId, id.localId )
                                , { loc = loc
                                  , name = name
                                  , size = size
                                  , image = image
                                  }
                                )
                            )
                        |> Dict.fromList
                , nextUnit =
                    { id = 0
                    , name = ""
                    , size = 1
                    , img = Nothing
                    }
                , clientId = yourClientId
                , grid = grid
                , zoom = 1
                , draw =
                    { currentLine = Nothing
                    , oldLines =
                        -- TODO: change the types to avoid empty
                        -- lists statically.
                        lines
                            |> List.filterMap
                                (\line ->
                                    case line of
                                        [] ->
                                            Nothing

                                        p :: ps ->
                                            Just ( p, ps )
                                )
                    }
                }
            , Cmd.none
            )

        ( _, NeedWelcome ) ->
            Debug.log "Unexpected message" ( NeedWelcome, Cmd.none )

        ( Protocol.RefreshBg bg, Ready m ) ->
            let
                grid =
                    m.grid
            in
            ( Ready { m | grid = { grid | bgImg = Just bg } }
            , Cmd.none
            )

        ( Protocol.GridSizeChanged sz, Ready m ) ->
            let
                grid =
                    m.grid
            in
            ( Ready { m | grid = { grid | size = sz } }
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

        ( Protocol.UnitAdded { id, loc, name, size, image }, Ready m ) ->
            ( Ready
                { m
                    | units =
                        Dict.update
                            (unitIdFromProtocol id)
                            (\_ -> Just { loc = loc, name = name, size = size, image = image })
                            m.units
                }
            , Cmd.none
            )

        ( Protocol.UnitDeleted unitId, Ready m ) ->
            ( Ready { m | units = Dict.remove (unitIdFromProtocol unitId) m.units }
            , Cmd.none
            )

        ( Protocol.BgCleared, Ready ({ grid } as m) ) ->
            ( Ready { m | grid = { grid | bgImg = Nothing } }
            , Cmd.none
            )

        ( Protocol.DrawingCleared, Ready ({ draw } as m) ) ->
            ( Ready { m | draw = { draw | oldLines = [] } }
            , Cmd.none
            )

        ( Protocol.LineAdded [], Ready m ) ->
            -- TODO: this should be impossible; rule it out statically.
            ( Ready m, Cmd.none )

        ( Protocol.LineAdded (p :: ps), Ready ({ draw } as m) ) ->
            ( Ready { m | draw = { draw | oldLines = ( p, ps ) :: draw.oldLines } }
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
