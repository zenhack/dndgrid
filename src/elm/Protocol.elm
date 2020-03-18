module Protocol exposing
    ( ClientMsg(..)
    , Error(..)
    , ServerMsg(..)
    , UnitId
    , UnitInfo
    , connect
    , disconnect
    , recv
    , send
    )

import Json.Decode as D
import Json.Encode as E
import Ports
import WebSocket


type alias UnitId =
    { clientId : Int
    , localId : Int
    }


type ClientMsg
    = MoveUnit UnitMotion
    | AddUnit
        { localId : Int
        , name : String
        , x : Int
        , y : Int
        }


type alias UnitMotion =
    { unitId : UnitId
    , x : Int
    , y : Int
    }


type ServerMsg
    = Welcome
        { yourClientId : Int
        , unitInfo : List UnitInfo
        }
    | UnitMoved UnitMotion


type alias UnitInfo =
    { x : Int
    , y : Int
    , id : UnitId
    , name : String
    }


type Error
    = JsonDecodeError D.Error
    | WebSocketError String


encodeClientMsg : ClientMsg -> E.Value
encodeClientMsg msg =
    case msg of
        MoveUnit motion ->
            E.object
                [ ( "tag", E.string "MoveUnit" )
                , ( "contents", encodeUnitMotion motion )
                ]

        AddUnit { localId, name, x, y } ->
            E.object
                [ ( "tag", E.string "AddUnit" )
                , ( "localId", E.int localId )
                , ( "name", E.string name )
                , ( "x", E.int x )
                , ( "y", E.int y )
                ]


encodeUnitMotion { unitId, x, y } =
    E.object
        [ ( "unitId", encodeUnitId unitId )
        , ( "x", E.int x )
        , ( "y", E.int y )
        ]


decodeUnitMotion : D.Decoder UnitMotion
decodeUnitMotion =
    D.map3 UnitMotion
        (D.field "unitId" decodeUnitId)
        (D.field "x" D.int)
        (D.field "y" D.int)


encodeUnitId : UnitId -> E.Value
encodeUnitId { clientId, localId } =
    E.object
        [ ( "clientId", E.int clientId )
        , ( "localId", E.int localId )
        ]


decodeServerMsg : D.Decoder ServerMsg
decodeServerMsg =
    D.field "tag" D.string
        |> D.andThen
            (\tag ->
                case tag of
                    "Welcome" ->
                        D.map2 (\cid units -> Welcome { yourClientId = cid, unitInfo = units })
                            (D.field "yourClientId" D.int)
                            (D.field "unitInfo" (D.list decodeUnitInfo))

                    "UnitMoved" ->
                        D.map UnitMoved (D.field "contents" decodeUnitMotion)

                    _ ->
                        D.fail <| "unknown tag: " ++ tag
            )


decodeUnitInfo : D.Decoder UnitInfo
decodeUnitInfo =
    D.map4 UnitInfo
        (D.field "x" D.int)
        (D.field "y" D.int)
        (D.field "id" decodeUnitId)
        (D.field "name" D.string)


decodeUnitId : D.Decoder UnitId
decodeUnitId =
    D.map2 UnitId
        (D.field "clientId" D.int)
        (D.field "localId" D.int)


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


send : ClientMsg -> Cmd msg
send msg =
    WebSocket.send Ports.sendSocketCommand <|
        WebSocket.Send
            { name = ""
            , content = E.encode 2 <| encodeClientMsg msg
            }


connect : Cmd msg
connect =
    WebSocket.send Ports.sendSocketCommand <|
        WebSocket.Connect
            { name = ""
            , address = "/"
            , protocol = "proto"
            }


disconnect : Cmd msg
disconnect =
    WebSocket.send Ports.sendSocketCommand <|
        WebSocket.Close { name = "" }
