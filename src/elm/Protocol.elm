module Protocol exposing
    ( ClientMsg(..)
    , Error(..)
    , GridInfo
    , Point
    , ServerMsg(..)
    , UnitId
    , UnitInfo
    , connect
    , disconnect
    , recv
    , send
    , uploadBg
    )

{-| Support for the communication protocol between clients and servers.
-}

import File exposing (File)
import Http
import Json.Decode as D
import Json.Encode as E
import Ports
import WebSocket



-- TYPES


type alias Point =
    { x : Int
    , y : Int
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
        , loc : Point
        }
    | SetGridSize Point


type alias UnitMotion =
    { unitId : UnitId
    , loc : Point
    }


type ServerMsg
    = Welcome WelcomeMsg
    | UnitMoved UnitMotion
    | UnitAdded UnitInfo
    | RefreshBg Int
    | GridSizeChanged Point


type alias WelcomeMsg =
    { yourClientId : Int
    , unitInfo : List UnitInfo
    , grid : GridInfo
    }


type alias GridInfo =
    { bgImg : Maybe Int
    , size : Point
    }


type alias UnitInfo =
    { id : UnitId
    , name : String
    , loc : Point
    }


type Error
    = JsonDecodeError D.Error
    | WebSocketError String



-- ENCODERS


encodeClientMsg : ClientMsg -> E.Value
encodeClientMsg msg =
    case msg of
        MoveUnit motion ->
            E.object
                [ ( "tag", E.string "MoveUnit" )
                , ( "contents", encodeUnitMotion motion )
                ]

        AddUnit { localId, name, loc } ->
            E.object
                [ ( "tag", E.string "AddUnit" )
                , ( "localId", E.int localId )
                , ( "name", E.string name )
                , ( "loc", encodePoint loc )
                ]

        SetGridSize sz ->
            E.object
                [ ( "tag", E.string "SetGridSize" )
                , ( "contents", encodePoint sz )
                ]


encodeUnitMotion { unitId, loc } =
    E.object
        [ ( "unitId", encodeUnitId unitId )
        , ( "loc", encodePoint loc )
        ]


encodeUnitId : UnitId -> E.Value
encodeUnitId { clientId, localId } =
    E.object
        [ ( "clientId", E.int clientId )
        , ( "localId", E.int localId )
        ]



-- DECODERS


decodeUnitMotion : D.Decoder UnitMotion
decodeUnitMotion =
    D.map2 UnitMotion
        (D.field "unitId" decodeUnitId)
        (D.field "loc" decodePoint)


decodeServerMsg : D.Decoder ServerMsg
decodeServerMsg =
    D.field "tag" D.string
        |> D.andThen
            (\tag ->
                case tag of
                    "Welcome" ->
                        D.map Welcome <|
                            D.map3 WelcomeMsg
                                (D.field "yourClientId" D.int)
                                (D.field "unitInfo" (D.list decodeUnitInfo))
                                (D.field "grid" decodeGridInfo)

                    "UnitMoved" ->
                        D.map UnitMoved (D.field "contents" decodeUnitMotion)

                    "UnitAdded" ->
                        D.map UnitAdded (D.field "contents" decodeUnitInfo)

                    "RefreshBg" ->
                        D.map RefreshBg (D.field "contents" D.int)

                    "GridSizeChanged" ->
                        D.map GridSizeChanged (D.field "contents" decodePoint)

                    _ ->
                        D.fail <| "unknown tag: " ++ tag
            )


decodeGridInfo =
    D.map2 GridInfo
        (D.field "bgImg" (D.nullable D.int))
        (D.field "size" decodePoint)


decodeUnitInfo : D.Decoder UnitInfo
decodeUnitInfo =
    D.map3 UnitInfo
        (D.field "id" decodeUnitId)
        (D.field "name" D.string)
        (D.field "loc" decodePoint)


decodePoint : D.Decoder Point
decodePoint =
    D.map2 Point
        (D.field "x" D.int)
        (D.field "y" D.int)


encodePoint : Point -> E.Value
encodePoint { x, y } =
    E.object
        [ ( "x", E.int x )
        , ( "y", E.int y )
        ]


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
