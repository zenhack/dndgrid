{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
module DB
    ( Conn
    , open
    , init

    , addUnit
    , setUnitLoc
    , listUnits

    , nextClientId

    , getImage
    , saveImage

    , getGrid
    , setGridBg
    , setGridSize
    ) where

import Zhp

import Control.Exception.Safe (throwString)

import           Database.SQLite.Simple (NamedParam((:=)))
import qualified Database.SQLite.Simple as SQL

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy       as LT

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
                CREATE TABLE IF NOT EXISTS units
                    ( client_id INTEGER NOT NULL
                    , local_id INTEGER NOT NULL
                    , name VARCHAR NOT NULL
                      -- We allow this to be null for now, but maybe we should
                      -- instead tell anonymous users they have to sign in
                      -- to add units?
                    , owner VARCHAR
                    , img_id INTEGER REFERENCES images(id)
                    , UNIQUE(client_id, local_id)
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
        SQL.execute_ c
            [here|
                CREATE TABLE IF NOT EXISTS unit_locations
                    ( grid_id INTEGER NOT NULL REFERENCES grids(id)
                    , client_id INTEGER NOT NULL REFERENCES units(client_id)
                    , local_id INTEGER NOT NULL REFERENCES units(local_id)
                    , x INTEGER NOT NULL
                    , y INTEGER NOT NULL
                    , UNIQUE(client_id, local_id, grid_id)
                    )
            |]
        -- Create a 10x10 grid:
        SQL.execute_ c
            [here|
                INSERT OR IGNORE INTO grids(id, height, width)
                VALUES (0, 10, 10)
            |]

nextClientId :: Conn -> IO (P.ID P.Client)
nextClientId (Conn c) = do
    rs <- SQL.query_ c
        [here|
            SELECT client_id + 1
            FROM units
            ORDER BY client_id DESC
            LIMIT 1
        |]
    pure $ case rs of
        []             -> 0
        (SQL.Only r:_) -> r

listUnits :: Conn -> IO [P.UnitInfo]
listUnits (Conn c) = do
    rs <- SQL.query_ c
        [here|
            SELECT client_id, local_id, name, img_id
            FROM units
        |]
    pure $
        [ P.UnitInfo
            { id = P.UnitId {clientId, localId}
            , loc = P.Point 1 1
            , name
            , image
            }
        | ( clientId, localId, name, image) <- rs
        ]


setUnitLoc :: Conn -> P.UnitId -> P.Point -> IO ()
setUnitLoc (Conn c) P.UnitId{clientId, localId} P.Point{x, y} =
    SQL.executeNamed c
        [here|
            INSERT OR REPLACE
            INTO unit_locations(grid_id, client_id, local_id, x, y)
            VALUES(0, :client_id, :local_id, :x, :y)
        |]
        [ ":x" := x
        , ":y" := y
        , ":client_id" := clientId
        , ":local_id" := localId
        ]

addUnit
    :: Conn
    -> P.UnitId
    -> Maybe LT.Text
    -> LBS.ByteString
    -> LT.Text
    -> IO (P.ID P.Image)
addUnit conn@(Conn c) P.UnitId{clientId, localId} owner img name =
    SQL.withTransaction c $ do
        imgId <- saveImageNoTx conn img
        SQL.executeNamed c
            [here|
                INSERT INTO units(owner, name, img_id, client_id, local_Id)
                VALUES (:owner, :name, :img_id, :client_id, :local_id)
            |]
            [ ":owner" := owner
            , ":name" := name
            , ":img_id" := imgId
            , ":client_id" := clientId
            , ":local_id" := localId
            ]
        pure imgId


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
