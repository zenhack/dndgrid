module Protocol exposing
    ( ClientMsg(..)
    , Error(..)
    , GridInfo
    , Point
    , ServerMsg(..)
    , UnitId
    , UnitInfo
    , addLine
    , clearBg
    , clearDrawing
    , connect
    , disconnect
    , imageUrl
    , recv
    , send
    , uploadBg
    )

{-| Support for the communication protocol between clients and servers.
-}

import Base64
import Bytes exposing (Bytes)
import File exposing (File)
import Http
import Json.Decode as D
import Json.Encode as E
import Ports
import WebSocket



-- TYPES


type alias Point a =
    { x : a
    , y : a
    }


type alias PixelPoint =
    { cell : Point Int
    , px : Point Float
    }


type alias UnitId =
    { clientId : Int
    , localId : Int
    }


type ClientMsg
    = MoveUnit UnitMotion
    | AddUnit
        { localId : Int
        , name : String
        , size : Int
        , loc : Point Int
        , imageData : Maybe Bytes
        }
    | DeleteUnit UnitId
    | SetGridSize (Point Int)
    | ClearBg
    | AddLine (List PixelPoint)
    | ClearDrawing


type alias UnitMotion =
    { unitId : UnitId
    , loc : Point Int
    }


type ServerMsg
    = Welcome WelcomeMsg
    | UnitMoved UnitMotion
    | UnitAdded UnitInfo
    | UnitDeleted UnitId
    | RefreshBg Int
    | BgCleared
    | GridSizeChanged (Point Int)
    | LineAdded (List PixelPoint)
    | DrawingCleared


type alias WelcomeMsg =
    { yourClientId : Int
    , unitInfo : List UnitInfo
    , grid : GridInfo
    , lines : List (List PixelPoint)
    }


type alias GridInfo =
    { bgImg : Maybe Int
    , size : Point Int
    }


type alias UnitInfo =
    { id : UnitId
    , name : String
    , size : Int
    , loc : Point Int
    , image : Maybe Int
    }


type Error
    = JsonDecodeError D.Error
    | WebSocketError String



-- ENCODERS


encodePixelPoint : PixelPoint -> E.Value
encodePixelPoint { cell, px } =
    E.object
        [ ( "cell", encodePoint E.int cell )
        , ( "px", encodePoint E.float px )
        ]


encodePoint : (a -> E.Value) -> Point a -> E.Value
encodePoint encode { x, y } =
    E.object
        [ ( "x", encode x )
        , ( "y", encode y )
        ]


encodeClientMsg : ClientMsg -> E.Value
encodeClientMsg msg =
    case msg of
        MoveUnit motion ->
            E.object
                [ ( "tag", E.string "MoveUnit" )
                , ( "contents", encodeUnitMotion motion )
                ]

        AddUnit { localId, name, size, loc, imageData } ->
            E.object
                [ ( "tag", E.string "AddUnit" )
                , ( "localId", E.int localId )
                , ( "name", E.string name )
                , ( "size", E.int size )
                , ( "loc", encodePoint E.int loc )
                , ( "imageData"
                  , Maybe.map encodeBytes imageData
                        |> Maybe.withDefault E.null
                  )
                ]

        DeleteUnit unitId ->
            E.object
                [ ( "tag", E.string "DeleteUnit" )
                , ( "contents", encodeUnitId unitId )
                ]

        SetGridSize sz ->
            E.object
                [ ( "tag", E.string "SetGridSize" )
                , ( "contents", encodePoint E.int sz )
                ]

        AddLine points ->
            E.object
                [ ( "tag", E.string "AddLine" )
                , ( "contents", E.list encodePixelPoint points )
                ]

        ClearDrawing ->
            E.object [ ( "tag", E.string "ClearDrawing" ) ]

        ClearBg ->
            E.object [ ( "tag", E.string "ClearBg" ) ]


encodeBytes : Bytes -> E.Value
encodeBytes =
    Base64.fromBytes
        -- Base64.fromBytes returns a Maybe, but per the docs it can't
        -- this is due to an implementation detail and it can't actually
        -- return nothing. So just pick an arbitrary default:
        >> Maybe.withDefault "IMPOSSIBLE"
        >> E.string


encodeUnitMotion { unitId, loc } =
    E.object
        [ ( "unitId", encodeUnitId unitId )
        , ( "loc", encodePoint E.int loc )
        ]


encodeUnitId : UnitId -> E.Value
encodeUnitId { clientId, localId } =
    E.object
        [ ( "clientId", E.int clientId )
        , ( "localId", E.int localId )
        ]



-- DECODERS


decodePixelPoint : D.Decoder PixelPoint
decodePixelPoint =
    D.map2 PixelPoint
        (D.field "cell" (decodePoint D.int))
        (D.field "px" (decodePoint D.float))


decodeUnitMotion : D.Decoder UnitMotion
decodeUnitMotion =
    D.map2 UnitMotion
        (D.field "unitId" decodeUnitId)
        (D.field "loc" (decodePoint D.int))


decodeServerMsg : D.Decoder ServerMsg
decodeServerMsg =
    D.field "tag" D.string
        |> D.andThen
            (\tag ->
                case tag of
                    "Welcome" ->
                        D.map Welcome <|
                            D.map4 WelcomeMsg
                                (D.field "yourClientId" D.int)
                                (D.field "unitInfo" (D.list decodeUnitInfo))
                                (D.field "grid" decodeGridInfo)
                                (D.field "lines" (D.list (D.list decodePixelPoint)))

                    "UnitMoved" ->
                        D.map UnitMoved (D.field "contents" decodeUnitMotion)

                    "UnitAdded" ->
                        D.map UnitAdded (D.field "contents" decodeUnitInfo)

                    "UnitDeleted" ->
                        D.map UnitDeleted (D.field "contents" decodeUnitId)

                    "RefreshBg" ->
                        D.map RefreshBg (D.field "contents" D.int)

                    "BgCleared" ->
                        D.succeed BgCleared

                    "GridSizeChanged" ->
                        D.map GridSizeChanged (D.field "contents" (decodePoint D.int))

                    "LineAdded" ->
                        D.map LineAdded (D.field "contents" (D.list decodePixelPoint))

                    "DrawingCleared" ->
                        D.succeed DrawingCleared

                    _ ->
                        D.fail <| "unknown tag: " ++ tag
            )


decodeGridInfo =
    D.map2 GridInfo
        (D.field "bgImg" (D.nullable D.int))
        (D.field "size" (decodePoint D.int))


decodeUnitInfo : D.Decoder UnitInfo
decodeUnitInfo =
    D.map5 UnitInfo
        (D.field "id" decodeUnitId)
        (D.field "name" D.string)
        (D.field "size" D.int)
        (D.field "loc" (decodePoint D.int))
        (D.maybe (D.field "image" D.int))


decodePoint : D.Decoder a -> D.Decoder (Point a)
decodePoint coord =
    D.map2 Point
        (D.field "x" coord)
        (D.field "y" coord)


decodeUnitId : D.Decoder UnitId
decodeUnitId =
    D.map2 UnitId
        (D.field "clientId" D.int)
        (D.field "localId" D.int)



-- COMMUNICATION HELPERS


{-| Receive messages from the server
-}
recv : (Result Error ServerMsg -> msg) -> Sub msg
recv toMsg =
    Ports.receiveSocketMsg <|
        WebSocket.receive
            (Result.mapError JsonDecodeError
                >> Result.andThen
                    (\wsmsg ->
                        case wsmsg of
                            WebSocket.Data { data } ->
                                D.decodeString decodeServerMsg data
                                    |> Result.mapError JsonDecodeError

                            WebSocket.Error { error } ->
                                Err (WebSocketError error)
                    )
                >> toMsg
            )


{-| Upload a file to use as the background.
-}
uploadBg : File -> (Result Http.Error () -> msg) -> Cmd msg
uploadBg file mkMsg =
    Http.post
        { url = "/new-bg"
        , body = Http.fileBody file
        , expect = Http.expectWhatever mkMsg
        }


clearBg : Cmd msg
clearBg =
    send ClearBg


addLine : List PixelPoint -> Cmd msg
addLine points =
    send <| AddLine points


clearDrawing : Cmd msg
clearDrawing =
    send ClearDrawing


{-| Send a message to the server
-}
send : ClientMsg -> Cmd msg
send msg =
    WebSocket.send Ports.sendSocketCommand <|
        WebSocket.Send
            { name = ""
            , content = E.encode 2 <| encodeClientMsg msg
            }


{-| Connect to the server.
-}
connect : Cmd msg
connect =
    WebSocket.send Ports.sendSocketCommand <|
        WebSocket.Connect
            { name = ""
            , address = "/"
            , protocol = "proto"
            }


{-| Disconnect from the server
-}
disconnect : Cmd msg
disconnect =
    WebSocket.send Ports.sendSocketCommand <|
        WebSocket.Close { name = "" }



-- MISC


{-| Get the URL of an image ID.
-}
imageUrl : Int -> String
imageUrl id =
    "/img/" ++ String.fromInt id ++ "/img.png"
