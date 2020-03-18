{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Protocol
    ( ClientMsg(..)
    , ID(..)
    , ServerMsg(..)
    , Client(..)
    , LocalUnit(..)
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

data ClientMsg

data ServerMsg
    = Welcome
        { yourClientId :: !(ID Client)
        , unitInfo     :: [UnitInfo]
        }
    deriving(Show, Read, Eq, Ord, Generic)
instance ToJSON ServerMsg
instance FromJSON ServerMsg

instance WebSocketsData (Maybe ServerMsg) where
    toLazyByteString (Just msg) = Aeson.encode msg
    toLazyByteString Nothing    = Aeson.encode Aeson.Null
    fromLazyByteString = Aeson.decode
    fromDataMessage = Aeson.decode . fromDataMessage

data UnitInfo = UnitInfo
    { x    :: !Int
    , y    :: !Int
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
