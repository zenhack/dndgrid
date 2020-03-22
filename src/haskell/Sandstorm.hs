{-# LANGUAGE RecordWildCards #-}
module Sandstorm
    ( SessionInfo(..)
    , Pronouns(..)
    , getSessionInfo
    ) where

import Zhp

import Data.List.Extra (wordsBy)

import qualified Data.Text.Lazy as LT
import qualified Web.Scotty     as Sc

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

getSessionInfo :: Sc.ActionM SessionInfo
getSessionInfo = do
    pronouns <- getPronouns
    userId <- Sc.header "X-Sandstorm-User-Id"
    permissions <- getPermissions
    username <- fromMaybe "" <$> Sc.header "X-Sandstorm-Username"
    preferredHandle <- Sc.header "X-Sandstorm-Preferred-Handle"
    userPicture <- Sc.header "X-Sandstorm-User-Picture"
    tabId <- fromMaybe "" <$> Sc.header "X-Sandstorm-Tab-Id"
    pure SessionInfo{..}

getPermissions :: Sc.ActionM [LT.Text]
getPermissions = do
    header <- Sc.header "X-Sandstorm-Permissions"
    pure $ case header of
        Nothing    -> []
        Just value ->
            LT.unpack value
            & wordsBy (== ',')
            & map LT.pack

getPronouns :: Sc.ActionM Pronouns
getPronouns = do
    header <- Sc.header "X-Sandstorm-User-Pronouns"
    pure $ case header of
        Just "netural" -> Neutral
        Just "male"    -> Male
        Just "female"  -> Female
        Just "robot"   -> Robot
        _              -> Neutral
