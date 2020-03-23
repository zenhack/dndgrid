{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Sandstorm
    ( SessionInfo(..)
    , Pronouns(..)
    , getSessionInfo
    , websocketGetSessionInfo
    ) where

import Zhp

import Data.List.Extra          (wordsBy)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding  (decodeUtf8With, encodeUtf8)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict      as M
import qualified Data.Text.Lazy       as LT
import qualified Network.WebSockets   as Ws
import qualified Web.Scotty           as Sc

data Pronouns
    = Neutral
    | Male
    | Female
    | Robot
    deriving(Show, Read, Eq, Ord, Bounded, Enum)

data SessionInfo = SessionInfo
    { userId          :: Maybe LT.Text
    , tabId           :: LT.Text
    , permissions     :: [LT.Text]
    , username        :: LT.Text
    , preferredHandle :: Maybe LT.Text
    , userPicture     :: Maybe LT.Text
    , pronouns        :: Pronouns
    }
    deriving(Show, Read, Eq, Ord)

type HeaderReader a = (LT.Text -> Maybe LT.Text) -> a

websocketGetSessionInfo :: Ws.PendingConnection -> SessionInfo
websocketGetSessionInfo conn =
    let Ws.RequestHead{requestHeaders} = Ws.pendingRequest conn
        hdrMap = M.fromList requestHeaders
        getHeader name =
            M.lookup (CI.mk $ LBS.toStrict $ encodeUtf8 name) hdrMap
            & fmap (LBS.fromStrict >>> decodeUtf8With lenientDecode)
    in
    getSessionInfo getHeader

getSessionInfo :: HeaderReader SessionInfo
getSessionInfo getHeader =
    let
        pronouns = getPronouns getHeader
        userId = getHeader "X-Sandstorm-User-Id"
        permissions = getPermissions getHeader
        username = fromMaybe "" $ getHeader "X-Sandstorm-Username"
        preferredHandle = getHeader "X-Sandstorm-Preferred-Handle"
        userPicture = getHeader "X-Sandstorm-User-Picture"
        tabId = fromMaybe "" $ getHeader "X-Sandstorm-Tab-Id"
    in
    SessionInfo{..}

getPermissions :: HeaderReader [LT.Text]
getPermissions getHeader =
    case getHeader "X-Sandstorm-Permissions" of
        Nothing    -> []
        Just value ->
            LT.unpack value
            & wordsBy (== ',')
            & map LT.pack

getPronouns :: HeaderReader Pronouns
getPronouns getHeader = do
    case getHeader "X-Sandstorm-User-Pronouns" of
        Just "netural" -> Neutral
        Just "male"    -> Male
        Just "female"  -> Female
        Just "robot"   -> Robot
        _              -> Neutral
