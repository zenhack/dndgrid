module Protocol exposing
    ( ClientMsg(..)
    , Error(..)
    , ServerMsg(..)
    , UnitId
    , UnitInfo
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
    = ClientMsg


type ServerMsg
    = Welcome
        { yourClientId : Int
        , unitInfo : List UnitInfo
        }


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
encodeClientMsg =
    Debug.todo "encodeClientMsg"


decodeServerMsg : D.Decoder ServerMsg
decodeServerMsg =
    D.map2 Welcome
        (D.field "yourClientId" D.int)
        (D.field "unitInfo" (D.list decodeUnitInfo))


decodeUnitInfo : D.Decoder UnitInfo
decodeUnitInfo =
    D.map4
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
recv =
    Ports.receiveSocketMsg <|
        WebSocket.receive
            (Result.andThen
                (\wsmsg ->
                    case wsmsg of
                        WebSocket.Data { data } ->
                            D.decodeString decodeServerMsg data

                        WebSocket.Error { error } ->
                            WebSocketError error
                )
            )


send : ClientMsg -> Cmd msg
send msg =
    WebSocket.send Ports.sendSocketCommand <|
        WebSocket.Send
            { name = ""
            , data = encodeClientMsg msg
            }


connect : Cmd msg
connect =
    WebSocket.send Ports.sendSocketCommand <|
        WebSocket.Connect
            { name = ""
            , address = "/"
            , protocol = ""
            }


disconnect : Cmd msg
disconnect =
    WebSocket.send Ports.sendSocketCommand <|
        WebSocket.Close { name = "" }
