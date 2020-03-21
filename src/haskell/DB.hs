{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
module DB
    ( Conn
    , open
    , init

    , getGridBg
    , setGridBg
    , getGridSize
    , setGridSize
    ) where

import Zhp

import Control.Exception.Safe (throwString)

import           Database.SQLite.Simple (NamedParam((:=)))
import qualified Database.SQLite.Simple as SQL

import qualified Data.ByteString.Lazy as LBS

import Text.Heredoc (here, there)

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


oneResult :: IO [a] -> IO a
oneResult m = m >>= \rs -> case rs of
    [r] -> pure r
    _ -> throwString $ "Error: expected exactly one result but got " <> show (length rs)


setGridBg :: Conn -> LBS.ByteString -> IO ()
setGridBg (Conn c) bytes =
    SQL.executeNamed c
        [here|
            UPDATE grids
            SET bg_image = :bg_image
            WHERE id = 0
        |]
        [ ":bg_image" := bytes ]


getGridBg :: Conn -> IO (Maybe LBS.ByteString)
getGridBg (Conn c) = do
    SQL.Only bytes <- oneResult $ SQL.query_ c
        [here|
            SELECT bg_image
            FROM grids
            WHERE id = 0
        |]
    pure bytes


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
    (x, y) <- oneResult $ SQL.query_ c
        [here|
            SELECT width, height
            FROM grids
            WHERE id = 0
        |]
    pure Point{x, y}
