{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- This module defines the wire protocol spoken between clients
-- and the server. It defines datatypes for messages, including
-- serialization code.
module Protocol
    ( ClientMsg(..)
    , Point(..)
    , ID(..)
    , ServerMsg(..)
    , Client(..)
    , GridInfo(..)
    , Image(..)
    , Unit(..)
    , LocalUnit(..)
    , UnitMotion(..)
    , UnitId(..)
    , UnitInfo(..)
    , Base64LBS(..)
    ) where


import Zhp

import Prelude (fail)

import Data.Aeson                     (FromJSON, ToJSON)
import Database.SQLite.Simple.ToField (ToField)
import GHC.Generics                   (Generic)

import Network.WebSockets (WebSocketsData(..))

import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Lazy.Char8  as LBS8
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as LT
import qualified Web.Scotty                  as Sc


newtype ID a = ID Int
    deriving(Show, Read, Eq, Ord, Bounded, FromJSON, ToJSON, Num, Sc.Parsable, ToField)

data Client
data Unit
data LocalUnit
data Image

data Point = Point { x :: !Int, y :: !Int }
    deriving(Show, Read, Eq, Ord, Generic)
instance ToJSON Point
instance FromJSON Point

-- Messages sent from the client to the server.
data ClientMsg
    = MoveUnit UnitMotion
    | AddUnit
        { localId   :: !(ID LocalUnit)
        , name      :: LT.Text
        , loc       :: Point
        , imageData :: Base64LBS
        }
    | SetGridSize Point
    deriving(Show, Read, Eq, Ord, Generic)
instance ToJSON ClientMsg
instance FromJSON ClientMsg

newtype Base64LBS
    = Base64LBS LBS.ByteString
    deriving(Show, Read, Eq, Ord, Generic)

instance ToJSON Base64LBS where
    toJSON (Base64LBS lbs) =
        Base64.encode lbs
        & LBS8.unpack
        & LT.pack
        & Aeson.toJSON

instance FromJSON Base64LBS where
    parseJSON (Aeson.String txt) =
        T.unpack txt
        & LBS8.pack
        & Base64.decodeLenient
        & Base64LBS
        & pure
    parseJSON _ =
        fail "Expected (base64) string"

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

data GridInfo = GridInfo
    { bgImg :: Maybe (ID Image)
    , size  :: Point
    }
    deriving(Show, Read, Eq, Ord, Generic)
instance ToJSON GridInfo
instance FromJSON GridInfo

-- messages sent from the server to the client.
data ServerMsg
    = Welcome
        { yourClientId :: !(ID Client)
        , unitInfo     :: [UnitInfo]
        , grid         :: GridInfo
        }
    | UnitMoved UnitMotion
    | UnitAdded UnitInfo
    | RefreshBg !(ID Image)
    | GridSizeChanged Point
    deriving(Show, Read, Eq, Ord, Generic)
instance ToJSON ServerMsg
instance FromJSON ServerMsg

instance WebSocketsData (Maybe ServerMsg) where
    toLazyByteString (Just msg) = Aeson.encode msg
    toLazyByteString Nothing    = Aeson.encode Aeson.Null
    fromLazyByteString = Aeson.decode
    fromDataMessage = Aeson.decode . fromDataMessage

data UnitInfo = UnitInfo
    { loc   :: Point
    , id    :: UnitId
    , name  :: LT.Text
    , image :: ID Image
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
