module Graphics.Babylon.Example.ChunkIndex (ChunkIndex, chunkIndex, runChunkIndex, addChunkIndex, chunkIndexRange) where

import Control.Alt (alt)
import Control.Alternative (pure)
import Control.Bind (bind)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class AsForeign, class IsForeign, read, readProp, write)
import Data.Generic (class Generic, gCompare, gEq)
import Data.Ord (class Ord, abs)
import Graphics.Babylon.Example.BlockIndex (BlockIndex, blockIndex, runBlockIndex)
import Prelude (class Eq, class Show, show, (<>), (<$>), (+), (-))

newtype ChunkIndex = ChunkIndex BlockIndex

chunkIndex :: Int -> Int -> Int -> ChunkIndex
chunkIndex x y z = ChunkIndex (blockIndex x y z)

runChunkIndex :: ChunkIndex -> { x :: Int, y :: Int, z :: Int }
runChunkIndex (ChunkIndex xyz) = runBlockIndex xyz

chunkIndexRange :: ChunkIndex -> ChunkIndex -> Int
chunkIndexRange (ChunkIndex a) (ChunkIndex b) = abs (i.x - k.x) + abs (i.y - k.y) + abs (i.z - k.z)
  where
    i = runBlockIndex a
    k = runBlockIndex b

addChunkIndex :: ChunkIndex -> ChunkIndex -> ChunkIndex
addChunkIndex (ChunkIndex a) (ChunkIndex b) = chunkIndex (i.x + k.x) (i.y + k.y) (i.z + k.z)
  where
    i = runBlockIndex a
    k = runBlockIndex b


derive instance generic_ChunkIndex :: Generic ChunkIndex

instance eq_ChunkIndex :: Eq ChunkIndex where
    eq = gEq

instance ord_ChunkIndex :: Ord ChunkIndex where
    compare = gCompare

instance show_Show :: Show ChunkIndex where
    show (ChunkIndex i) = show i

instance isForeign_ChunkIndex :: IsForeign ChunkIndex where
    read value = ChunkIndex <$> read value

instance asForeign_ChunkIndex :: AsForeign ChunkIndex where
    write (ChunkIndex i) = write i


