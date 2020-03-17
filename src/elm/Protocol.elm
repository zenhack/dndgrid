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
    D.map2 (\cid units -> Welcome { yourClientId = cid, unitInfo = units })
        (D.field "yourClientId" D.int)
        (D.field "unitInfo" (D.list decodeUnitInfo))


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
