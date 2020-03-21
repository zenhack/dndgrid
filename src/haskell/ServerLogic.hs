-- This module implements the high-level "business logic" of the protocol;
-- It does not concern itself with HTTP, serialization, etc, and just specifies
-- communication patterns in terms of STM, channels...
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module ServerLogic
    ( Server
    , ClientConn(..)
    , refreshBg
    , newServer
    , handleClient
    ) where

import Zhp

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Exception.Safe   (bracket)

import qualified Data.Map.Strict as M
import qualified DB
import qualified Protocol        as P

handleClient :: Server -> ClientConn -> IO ()
handleClient server@(Server{stateVar, db}) clientConn = do
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
        handleClientMsg server clientId clientChan msg
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
            { P.bgImg = bgCount st
            , P.gridSize = size (grid st)
            , P.yourClientId = clientId
            , P.unitInfo = M.elems $ units $ grid st
            }
        pure clientId

refreshBg :: Server -> IO ()
refreshBg server@(Server{stateVar}) = atomically $ do
    st <- readTVar stateVar
    let newBgCount = bgCount st + 1
    writeTVar stateVar st { bgCount = newBgCount }
    broadcast server $ P.RefreshBg $! newBgCount

handleClientMsg
    :: Server
    -> P.ID P.Client
    -> TChan P.ServerMsg
    -> P.ClientMsg
    -> IO ()
handleClientMsg server@(Server{stateVar, db}) clientId clientChan msg =
    case msg of
        P.MoveUnit motion@P.UnitMotion{unitId, loc} -> atomically $ do
            modifyTVar' stateVar $
                alterUnit unitId $ fmap $ \unit -> (unit {P.loc = loc} :: P.UnitInfo)
            broadcast server (P.UnitMoved motion)
        P.AddUnit{loc, name, localId} -> atomically $ do
            let id = P.UnitId {clientId, localId}
            let unitInfo = P.UnitInfo { id, loc, name }
            modifyTVar' stateVar $
                alterUnit id $ \_ -> Just unitInfo
            broadcast server (P.UnitAdded unitInfo)
        P.SetGridSize size -> do
            atomically $ do
                modifyTVar' stateVar $
                    \st@ServerState{grid} -> st { grid = grid { size } }
                broadcast server $ P.GridSizeChanged size
            DB.setGridSize db size

alterUnit :: P.UnitId -> (Maybe P.UnitInfo -> Maybe P.UnitInfo) -> ServerState -> ServerState
alterUnit id f st@ServerState{grid = g@GridState{units}} =
    st { grid = g { units = M.alter f id units } }

broadcast :: Server -> P.ServerMsg -> STM ()
broadcast (Server{stateVar}) msg = do
    ch <- broadcastChan <$> readTVar stateVar
    writeTChan ch msg

data Server = Server
    { stateVar :: TVar ServerState
    , db       :: DB.Conn
    }

data GridState = GridState
    { units :: M.Map P.UnitId P.UnitInfo
    , size  :: P.Point
    }

data ServerState = ServerState
    { grid          :: GridState
    , bgCount       :: !Int
    , nextClientId  :: !(P.ID P.Client)
    , clients       :: M.Map (P.ID P.Client) (TChan P.ServerMsg)
    , broadcastChan :: TChan P.ServerMsg
    }

data ClientConn = ClientConn
    { sendMsg :: P.ServerMsg -> IO ()
    , recvMsg :: IO P.ClientMsg
    }

newServer :: DB.Conn -> IO Server
newServer db = do
    size <- DB.getGridSize db
    atomically $ do
        ch <- newBroadcastTChan
        stateVar <- newTVar ServerState
            { grid = GridState
                { units = M.empty
                , size
                }
            , bgCount = 0
            , nextClientId = 0
            , clients = M.empty
            , broadcastChan = ch
            }
        pure Server{stateVar, db}
