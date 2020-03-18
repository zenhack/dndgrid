{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- This module defines the wire protocol spoken between clients
-- and the server. It defines datatypes for messages, including
-- serialization code.
module Protocol
    ( ClientMsg(..)
    , ID(..)
    , ServerMsg(..)
    , Client(..)
    , LocalUnit(..)
    , UnitMotion(..)
    , UnitId(..)
    , UnitInfo(..)
    ) where


import Zhp

import Data.Aeson   (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Network.WebSockets (WebSocketsData(..))

import qualified Data.Aeson     as Aeson
import qualified Data.Text.Lazy as LT


newtype ID a = ID Int
    deriving(Show, Read, Eq, Ord, Bounded, FromJSON, ToJSON, Num)

data Client
data LocalUnit

data Point = Point { x :: !Int, y :: !Int }
    deriving(Show, Read, Eq, Ord, Generic)
instance ToJSON Point
instance FromJSON Point

-- Messages sent from the client to the server.
data ClientMsg
    = MoveUnit UnitMotion
    | AddUnit
        { localId :: !(ID LocalUnit)
        , name    :: LT.Text
        , loc     :: Point
        }
    deriving(Show, Read, Eq, Ord, Generic)
instance ToJSON ClientMsg
instance FromJSON ClientMsg

data UnitMotion = UnitMotion
    { unitId :: !UnitId
    , loc    :: Point
    }
    deriving(Show, Read, Eq, Ord, Generic)
instance ToJSON UnitMotion
instance FromJSON UnitMotion

instance WebSocketsData (Maybe ClientMsg) where
    toLazyByteString (Just msg) = Aeson.encode msg
    toLazyByteString Nothing    = Aeson.encode Aeson.Null
    fromLazyByteString = Aeson.decode
    fromDataMessage = Aeson.decode . fromDataMessage

-- messages sent from the server to the client.
data ServerMsg
    = Welcome
        { yourClientId :: !(ID Client)
        , unitInfo     :: [UnitInfo]
        , bgImg        :: !Int
        }
    | UnitMoved UnitMotion
    | UnitAdded UnitInfo
    | RefreshBg !Int
    deriving(Show, Read, Eq, Ord, Generic)
instance ToJSON ServerMsg
instance FromJSON ServerMsg

instance WebSocketsData (Maybe ServerMsg) where
    toLazyByteString (Just msg) = Aeson.encode msg
    toLazyByteString Nothing    = Aeson.encode Aeson.Null
    fromLazyByteString = Aeson.decode
    fromDataMessage = Aeson.decode . fromDataMessage

data UnitInfo = UnitInfo
    { loc  :: Point
    , id   :: UnitId
    , name :: LT.Text
    }
    deriving(Show, Read, Eq, Ord, Generic)
instance ToJSON UnitInfo
instance FromJSON UnitInfo

data UnitId = UnitId
    { clientId :: !(ID Client)
    , localId  :: !(ID LocalUnit)
    }
    deriving(Show, Read, Eq, Ord, Generic)
instance ToJSON UnitId
instance FromJSON UnitId
