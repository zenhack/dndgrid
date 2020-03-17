{-# LANGUAGE DeriveGeneric              #-}
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

import qualified Data.Text.Lazy as LT


newtype ID a = ID Int
    deriving(Show, Read, Eq, Ord, Bounded, FromJSON, ToJSON)

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

data UnitInfo = UnitInfo
    { x    :: !Int
    , y    :: !Int
    , id   :: UnitId
    , name :: LT.Text
    }
    deriving(Show, Read, Eq, Ord, Generic)
instance ToJSON UnitInfo

data UnitId = UnitId
    { clientId :: !(ID Client)
    , localId  :: !(ID LocalUnit)
    }
    deriving(Show, Read, Eq, Ord, Generic)
instance ToJSON UnitId
