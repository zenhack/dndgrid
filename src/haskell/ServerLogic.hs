-- This module implements the high-level "business logic" of the protocol;
-- It does not concern itself with HTTP, serialization, etc, and just specifies
-- communication patterns in terms of STM, channels...
module ServerLogic
    ( Server
    , ClientConn(..)
    , newServer
    , handleClient
    ) where

import Zhp

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Exception.Safe   (bracket)

import qualified Data.Map.Strict as M
import qualified Protocol        as P

handleClient :: Server -> ClientConn -> IO ()
handleClient (Server stateVar) clientConn = do
    ch <- newTChanIO
    race_
        (bracket
            (setupClient ch)
            teardownClient
            (useClient ch))
        (forever $ do
            msg <- atomically $ readTChan ch
            sendMsg clientConn msg)
  where
    useClient clientChan clientId = forever $ do
        msg <- recvMsg clientConn
        atomically $ handleClientMsg stateVar clientId clientChan msg
    teardownClient clientId = atomically $ do
        modifyTVar' stateVar $ \st -> st
            { clients = M.delete clientId (clients st)
            }
    setupClient clientChan = atomically $ do
        st <- readTVar stateVar
        let clientId = nextClientId st
        writeTVar stateVar st
            { nextClientId = clientId + 1
            , clients = M.insert clientId clientChan (clients st)
            }
        writeTChan clientChan P.Welcome
            { P.yourClientId = clientId
            , P.unitInfo = M.elems $ units $ grid st
            }
        pure clientId

handleClientMsg
    :: TVar ServerState
    -> P.ID P.Client
    -> TChan P.ServerMsg
    -> P.ClientMsg
    -> STM ()
handleClientMsg stateVar clientId clientChan msg =
    case msg of
        P.MoveUnit{} -> error "TODO"

newtype Server = Server (TVar ServerState)

data GridState = GridState
    { units :: M.Map P.UnitId P.UnitInfo
    }

data ServerState = ServerState
    { grid         :: GridState
    , nextClientId :: !(P.ID P.Client)
    , clients      :: M.Map (P.ID P.Client) (TChan P.ServerMsg)
    }

data ClientConn = ClientConn
    { sendMsg :: P.ServerMsg -> IO ()
    , recvMsg :: IO P.ClientMsg
    }

newServer :: IO Server
newServer = Server <$> newTVarIO initialServerState

initialServerState :: ServerState
initialServerState = ServerState
    { grid = GridState { units = M.empty }
    , nextClientId = 0
    , clients = M.empty
    }
