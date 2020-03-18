module Main (main) where

import Zhp

import qualified Data.Text as T

import qualified Data.Map.Strict                as M
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets             as Ws
import qualified Web.Scotty                     as Sc

import Control.Exception.Safe (throwString)
import Network.Wai            (Application)

import Protocol ()
import ServerLogic


staticFiles =
    [ ("/", "static/app.html", "text/html")
    , ("/dndgrid.js", "build/dndgrid.js", "application/javascript")
    , ("/setup.js", "static/setup.js", "application/javascript")
    ]

makeScottyApp :: IO Application
makeScottyApp = Sc.scottyApp $ do
    for_ staticFiles $ \(url, filePath, contentType) ->
        Sc.get url $ do
            Sc.setHeader "Content-Type" contentType
            Sc.file filePath

wsApp :: Server -> Ws.ServerApp
wsApp server pending = do
    conn <- Ws.acceptRequest pending
    Ws.withPingThread conn 30 (pure ()) $
        handleClient server ClientConn
            { sendMsg = Ws.sendTextData conn . Just
            , recvMsg = do
                msg <- Ws.receiveData conn
                case msg of
                    Just msg -> pure msg
                    Nothing  -> throwString "Decoding client message failed"
            }

makeApp :: IO Application
makeApp = do
    server <- newServer
    scottyApp <- makeScottyApp
    pure $ WaiWs.websocketsOr
        Ws.defaultConnectionOptions
        (wsApp server)
        scottyApp

main :: IO ()
main = makeApp >>= Warp.run 3000
