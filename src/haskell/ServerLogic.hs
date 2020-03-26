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
    , setBg
    ) where

import Zhp

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Exception.Safe   (bracket)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict      as M
import qualified DB
import qualified Protocol             as P
import qualified Sandstorm

handleClient :: Sandstorm.SessionInfo -> Server -> ClientConn -> IO ()
handleClient sessionInfo server@(Server{stateVar, db}) clientConn = do
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
        handleClientMsg sessionInfo server clientId clientChan msg
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
            { P.grid = settings (grid st)
            , P.yourClientId = clientId
            , P.unitInfo = M.elems $ units $ grid st
            }
        pure clientId

setBg :: LBS.ByteString -> Server -> IO ()
setBg bytes server@(Server{db}) = do
    -- FIXME: there's a race condition here.
    imgId <- DB.setGridBg db bytes
    atomically $ broadcast server $ P.RefreshBg imgId

handleClientMsg
    :: Sandstorm.SessionInfo
    -> Server
    -> P.ID P.Client
    -> TChan P.ServerMsg
    -> P.ClientMsg
    -> IO ()
handleClientMsg sessionInfo server@(Server{stateVar, db}) clientId clientChan msg =
    case msg of
        P.MoveUnit motion@P.UnitMotion{unitId, loc} -> do
            DB.setUnitLoc db unitId loc
            atomically $ do
                modifyTVar' stateVar $
                    alterUnit unitId $ fmap $ \unit -> (unit {P.loc = loc} :: P.UnitInfo)
                broadcast server (P.UnitMoved motion)
        P.AddUnit{loc, name, size, localId, imageData = P.Base64LBS bytes} -> do
            let id = P.UnitId {clientId, localId}
            image <- DB.addUnit
                db
                id
                (Sandstorm.userId sessionInfo)
                bytes
                name
                size
            let unitInfo = P.UnitInfo { id, loc, name, size, image }
            atomically $ do
                modifyTVar' stateVar $
                    alterUnit id $ \_ -> Just unitInfo
                broadcast server (P.UnitAdded unitInfo)
        P.DeleteUnit unitId -> do
            DB.deleteUnit db unitId
            atomically $ broadcast server $ P.UnitDeleted unitId
        P.SetGridSize size -> do
            atomically $ do
                modifyTVar' stateVar $
                    \st@ServerState{grid = grid@GridState{settings}} ->
                        -- Lenses would make this nicer. Maybe at some point we can
                        -- use microlens.
                        st { grid = grid { settings = settings { P.size = size } } }
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
    { units    :: M.Map P.UnitId P.UnitInfo
    , settings :: P.GridInfo
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

newServer :: DB.Conn -> IO Server
newServer db = do
    units <- DB.listUnits db
    nextClientId <- DB.nextClientId db
    gridInfo <- DB.getGrid db
    atomically $ do
        ch <- newBroadcastTChan
        stateVar <- newTVar ServerState
            { grid = GridState
                { settings = gridInfo
                , units = M.fromList [(id, unit) | unit@P.UnitInfo{id} <- units]
                }
            , nextClientId
            , clients = M.empty
            , broadcastChan = ch
            }
        pure Server{stateVar, db}
