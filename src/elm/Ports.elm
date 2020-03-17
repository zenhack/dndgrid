port module Socket exposing (receiveSocketMsg, sendSocketCommand)


port receiveSocketMsg : (JD.Value -> msg) -> Sub msg


port sendSocketCommand : JE.Value -> Cmd msg
