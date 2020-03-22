{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
module DB
    ( Conn
    , open
    , init

    , getImage
    , saveImage

    , getGrid
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

import qualified Protocol as P

newtype Conn = Conn SQL.Connection

open :: FilePath -> IO Conn
open path = Conn <$> SQL.open path

init :: Conn -> IO ()
init (Conn c) =
    SQL.withTransaction c $ do
        SQL.execute_ c
            [here|
                CREATE TABLE IF NOT EXISTS images
                    ( id INTEGER PRIMARY KEY
                    , img_data BLOB NOT NULL
                    )
            |]
        SQL.execute_ c
            [here|
                CREATE TABLE IF NOT EXISTS grids
                    ( id INTEGER PRIMARY KEY
                    , height INTEGER NOT NULL
                    , width INTEGER NOT NULL
                    , bg_image INTEGER REFERENCES images(id)
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

saveImage :: Conn -> LBS.ByteString -> IO (P.ID P.Image)
saveImage (Conn c) bytes =
    SQL.withTransaction c $ saveImageNoTx (Conn c) bytes

-- Non transactional version of saveImage, because sqlite doesn't do nested
-- transactions, and we need to call this from other functions here.
saveImageNoTx :: Conn -> LBS.ByteString -> IO (P.ID P.Image)
saveImageNoTx (Conn c) bytes = do
    SQL.executeNamed c
        [here|
            INSERT INTO images(img_data)
            VALUES (:bytes)
        |]
        [ ":bytes" := bytes ]
    P.ID . fromIntegral <$> SQL.lastInsertRowId c



getImage :: Conn -> P.ID P.Image -> IO LBS.ByteString
getImage (Conn c) (P.ID imgId) = do
    SQL.Only bytes <- oneResult $ SQL.queryNamed c
        [here|
            SELECT img_data
            FROM images
            WHERE id = :id
        |]
        [ ":id" := imgId ]
    pure bytes


setGridBg :: Conn -> LBS.ByteString -> IO (P.ID P.Image)
setGridBg conn@(Conn c) bytes =
    SQL.withTransaction c $ do
        ret@(P.ID imgId) <- saveImageNoTx conn bytes
        SQL.executeNamed c
            [here|
                UPDATE grids
                SET bg_image = :bg_image
                WHERE id = 0
            |]
            [ ":bg_image" := imgId ]
        pure ret


getGrid :: Conn -> IO P.GridInfo
getGrid (Conn c) = do
    (bgImg, w, h) <- oneResult $ SQL.query_ c
        [here|
            SELECT bg_image, width, height
            FROM grids
            WHERE id = 0
        |]
    pure P.GridInfo
        { P.bgImg = fmap P.ID bgImg
        , P.size = P.Point { P.x = w, P.y = h }
        }

getGridBg :: Conn -> IO (Maybe LBS.ByteString)
getGridBg (Conn c) = do
    SQL.Only bytes <- oneResult $ SQL.query_ c
        [here|
            SELECT bg_image
            FROM grids
            WHERE id = 0
        |]
    pure bytes


setGridSize :: Conn -> P.Point -> IO ()
setGridSize (Conn c) P.Point{P.x, P.y} =
    SQL.executeNamed c
        [here|
            UPDATE grids
            SET height = :height, width = :width
            WHERE id = 0
        |]
        [ ":height" := y
        , ":width" := x
        ]


getGridSize :: Conn -> IO P.Point
getGridSize (Conn c) = do
    (x, y) <- oneResult $ SQL.query_ c
        [here|
            SELECT width, height
            FROM grids
            WHERE id = 0
        |]
    pure P.Point{P.x = x, P.y = y}
