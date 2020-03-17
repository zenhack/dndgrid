port module Ports exposing (receiveSocketMsg, sendSocketCommand)

import Json.Decode as D
import Json.Encode as E


port receiveSocketMsg : (D.Value -> msg) -> Sub msg


port sendSocketCommand : E.Value -> Cmd msg
