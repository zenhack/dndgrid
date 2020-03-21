module Main (main) where

import Zhp

import qualified Data.Text as T

import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Map.Strict                as M
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets             as Ws
import qualified Web.Scotty                     as Sc

import Control.Exception.Safe (throwString)
import Network.Wai            (Application)
import System.Environment     (getEnv)

import qualified DB

import Protocol ()
import ServerLogic


staticFiles =
    [ ("/", "static/app.html", "text/html")
    , ("/dndgrid.js", "build/dndgrid.js", "application/javascript")
    , ("/setup.js", "static/setup.js", "application/javascript")
    ]

makeScottyApp :: DB.Conn -> Server -> IO Application
makeScottyApp db server = Sc.scottyApp $ do
    for_ staticFiles $ \(url, filePath, contentType) ->
        Sc.get url $ do
            Sc.setHeader "Content-Type" contentType
            Sc.file filePath
    Sc.post "/new-bg" $ do
        bytes <- Sc.body
        liftIO $ do
            DB.setGridBg db bytes
            refreshBg server
    Sc.get "/img/:id/img.png" $ do
        imgId <- Sc.param "id"
        bytes <- liftIO $ DB.getImage db imgId
        Sc.setHeader "Content-Type" "image/png"
        Sc.raw bytes
    Sc.get "/bg/:junk/bg.png" $ do
        result <- liftIO $ DB.getGridBg db
        Sc.setHeader "Content-Type" "image/png"
        case result of
            Just bytes -> Sc.raw bytes
            Nothing    -> Sc.file "default-bg.png"

wsApp :: Server -> Ws.ServerApp
wsApp server pending = do
    conn <- Ws.acceptRequestWith
        pending
        Ws.defaultAcceptRequest { Ws.acceptSubprotocol = Just "dndgrid" }
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
    dbPath <- getEnv "DB_PATH"
    db <- DB.open dbPath
    DB.init db

    server <- newServer db
    scottyApp <- makeScottyApp db server
    pure $ WaiWs.websocketsOr
        Ws.defaultConnectionOptions
        (wsApp server)
        scottyApp

main :: IO ()
main = makeApp >>= Warp.run 3000
