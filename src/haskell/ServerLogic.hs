-- This module implements the high-level "business logic" of the protocol;
-- It does not concern itself with HTTP, serialization, etc, and just specifies
-- communication patterns in terms of STM, channels...
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
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
handleClient server@(Server stateVar) clientConn = do
    myChan <- newTChanIO
    pubChan <- atomically $ do
        st <- readTVar stateVar
        dupTChan $ broadcastChan st
    race_
        (bracket
            (setupClient myChan)
            teardownClient
            (useClient myChan))
        (forever $ do
            msg <- atomically $ readTChan myChan `orElse` readTChan pubChan
            sendMsg clientConn msg)
  where
    useClient clientChan clientId = forever $ do
        msg <- recvMsg clientConn
        atomically $ handleClientMsg server clientId clientChan msg
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
    :: Server
    -> P.ID P.Client
    -> TChan P.ServerMsg
    -> P.ClientMsg
    -> STM ()
handleClientMsg server@(Server stateVar) clientId clientChan msg =
    case msg of
        P.MoveUnit motion@P.UnitMotion{unitId, x, y} -> do
            modifyTVar' stateVar $
                alterUnit unitId $ fmap $ \unit -> (unit {P.x = x, P.y = y} :: P.UnitInfo)
            broadcast server (P.UnitMoved motion)
        P.AddUnit{x, y, name, localId} -> do
            let id = P.UnitId {clientId, localId}
            modifyTVar' stateVar $
                alterUnit id $ \_ -> Just P.UnitInfo { id, x, y, name }
            -- TODO: broadcast the addition to clients.

alterUnit :: P.UnitId -> (Maybe P.UnitInfo -> Maybe P.UnitInfo) -> ServerState -> ServerState
alterUnit id f st@ServerState{grid = g@GridState{units}} =
    st { grid = g { units = M.alter f id units } }

broadcast :: Server -> P.ServerMsg -> STM ()
broadcast (Server stateVar) msg = do
    ch <- broadcastChan <$> readTVar stateVar
    writeTChan ch msg

newtype Server = Server (TVar ServerState)

data GridState = GridState
    { units :: M.Map P.UnitId P.UnitInfo
    }

data ServerState = ServerState
    { grid          :: GridState
    , nextClientId  :: !(P.ID P.Client)
    , clients       :: M.Map (P.ID P.Client) (TChan P.ServerMsg)
    , broadcastChan :: TChan P.ServerMsg
    }

data ClientConn = ClientConn
    { sendMsg :: P.ServerMsg -> IO ()
    , recvMsg :: IO P.ClientMsg
    }

newServer :: IO Server
newServer = atomically $ do
    ch <- newBroadcastTChan
    Server <$> newTVar ServerState
        { grid = GridState { units = M.empty }
        , nextClientId = 0
        , clients = M.empty
        , broadcastChan = ch
        }
