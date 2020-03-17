module Protocol exposing
    ( ClientMsg(..)
    , Error(..)
    , ServerMsg(..)
    , recv
    , send
    )

import Json.Decode as D
import Json.Encode as E
import Ports
import WebSocket


type ClientMsg
    = ClientMsg


type ServerMsg
    = ServerMsg


type Error
    = JsonDecodeError D.Error
    | WebSocketError String


encodeClientMsg : ClientMsg -> E.Value
encodeClientMsg =
    Debug.todo "encodeClientMsg"


decodeServerMsg : D.Decoder ServerMsg
decodeServerMsg =
    D.fail "TODO"


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
