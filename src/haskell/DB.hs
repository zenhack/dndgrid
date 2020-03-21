{-# LANGUAGE QuasiQuotes #-}
module DB
    ( Conn
    , open
    , init
    ) where

import Zhp

import qualified Database.SQLite.Simple as SQL

import Text.Heredoc (here)

newtype Conn = Conn SQL.Connection

open :: FilePath -> IO Conn
open path = Conn <$> SQL.open path

init :: Conn -> IO ()
init (Conn c) =
    SQL.withTransaction c $ do
        SQL.execute_ c
            [here|
                CREATE TABLE IF NOT EXISTS grids
                    ( id INTEGER PRIMARY KEY
                    , height INTEGER NOT NULL
                    , width INTEGER NOT NULL
                    , bg_image BLOB
                    )
            |]
        -- Create a 10x10 grid:
        SQL.execute_ c
            [here|
                INSERT OR IGNORE INTO grids(id, height, width)
                VALUES (0, 10, 10)
            |]
