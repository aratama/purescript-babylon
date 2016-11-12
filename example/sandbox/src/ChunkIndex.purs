module Graphics.Babylon.Example.ChunkIndex where

import Control.Alternative (pure)
import Control.Bind (bind)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class AsForeign, class IsForeign, readProp)
import Data.Generic (class Generic, gCompare, gEq)
import Data.Ord (class Ord)
import Prelude (class Eq, class Show, show, (<>))

data ChunkIndex = ChunkIndex { x :: Int, y :: Int, z :: Int }

runChunkIndex :: ChunkIndex -> { x :: Int, y :: Int, z :: Int }
runChunkIndex (ChunkIndex xyz) = xyz

derive instance generic_ChunkIndex :: Generic ChunkIndex

instance eq_ChunkIndex :: Eq ChunkIndex where
    eq = gEq

instance ord_ChunkIndex :: Ord ChunkIndex where
    compare = gCompare

instance show_Show :: Show ChunkIndex where
    show (ChunkIndex i) = show i.x <> "," <> show i.y <> "," <> show i.z

instance isForeign_ChunkIndex :: IsForeign ChunkIndex where
    read value = do
        x <- readProp "x" value
        y <- readProp "y" value
        z <- readProp "z" value
        pure (ChunkIndex { x, y, z })

instance asForeign_ChunkIndex :: AsForeign ChunkIndex where
    write = toForeign


