{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
module DB
    ( Conn
    , open
    , init

    , getGridSize
    , setGridSize
    ) where

import Zhp

import Control.Exception.Safe (throwString)

import           Database.SQLite.Simple (NamedParam((:=)))
import qualified Database.SQLite.Simple as SQL

import Text.Heredoc (here)

import Protocol (Point(..))

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


setGridSize :: Conn -> Point -> IO ()
setGridSize (Conn c) Protocol.Point{x, y} =
    SQL.executeNamed c
        [here|
            UPDATE grids
            SET height = :height, width = :width
            WHERE id = 0
        |]
        [ ":height" := y
        , ":width" := x
        ]

getGridSize :: Conn -> IO Point
getGridSize (Conn c) = do
    rs <- SQL.query_ c
        [here|
            SELECT width, height
            FROM grids
            WHERE id = 0
        |]
    case rs of
        [(x, y)] -> pure Point{x, y}
        _ -> throwString "Grid not found. Has the DB been initialized?"
