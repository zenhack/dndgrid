module Main (main) where

import Zhp

import qualified Data.Map.Strict                as M
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets             as Ws
import qualified Web.Scotty                     as Sc

import Network.Wai (Application)

import Control.Concurrent.STM


staticFiles =
    [ ("/", "static/app.html", "text/html")
    , ("/dndgrid.js", "build/dndgrid.js", "application/javascript")
    , ("/setup.js", "static/setup.js", "application/javascript")
    ]

newtype ID a = ID Int
    deriving(Show, Read, Eq, Ord, Bounded)

data Client = Client

data Unit = Unit
    { localId  :: ID Unit
    , clientId :: ID Client
    }

data GridState = GridState
    { units :: M.Map (ID Unit) Unit
    }

data ServerState = ServerState
    { grid    :: GridState
    , clients :: M.Map (ID Client) Client
    }

data Message

initialServerState :: ServerState
initialServerState = ServerState
    { grid = GridState { units = M.empty }
    , clients = M.empty
    }

makeScottyApp :: TVar ServerState -> IO Application
makeScottyApp serverState = Sc.scottyApp $ do
    for_ staticFiles $ \(url, filePath, contentType) ->
        Sc.get url $ do
            Sc.setHeader "Content-Type" contentType
            Sc.file filePath

wsApp :: TVar ServerState -> Ws.ServerApp
wsApp _serverState _pending = pure ()

makeApp :: IO Application
makeApp = do
    serverState <- atomically $ newTVar initialServerState
    scottyApp <- makeScottyApp serverState
    pure $ WaiWs.websocketsOr
        Ws.defaultConnectionOptions
        (wsApp serverState)
        scottyApp

main :: IO ()
main = makeApp >>= Warp.run 3000
