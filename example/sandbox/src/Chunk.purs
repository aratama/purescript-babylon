module Graphics.Babylon.Example.Chunk where

import Control.Bind (bind)
import Data.Array (fromFoldable) as Array
import Data.Foreign (F, Foreign, readArray, toForeign)
import Data.Foreign.Class (readProp, write, class AsForeign, class IsForeign)
import Data.Map (Map, fromFoldable, mapWithKey, values)
import Data.Traversable (for)
import Data.Tuple (Tuple(Tuple))
import Graphics.Babylon.Example.ChunkIndex (ChunkIndex(..))
import Prelude (class Eq, pure, ($), (&&), (==), (>>=))

import Graphics.Babylon.Example.BlockIndex (BlockIndex(..))
import Graphics.Babylon.Example.BlockType (BlockType)

newtype Chunk = Chunk { index :: ChunkIndex, map :: Map BlockIndex BlockType }

instance eq_TerrainMap :: Eq Chunk where
    eq (Chunk a) (Chunk b) = a.index == b.index && a.map == b.map

instance isForeign_TerrainMap :: IsForeign Chunk where
    read value = do
        index <- readProp "index" value
        map <- readProp "map" value >>= foreignToBoxelMap
        pure $ Chunk { index, map }

instance asForeign_TerrainMap :: AsForeign Chunk where
    write (Chunk terrain) = toForeign {
        index: write terrain.index,
        map: boxelMapToForeign terrain.map
    }

boxelMapToForeign :: Map BlockIndex BlockType -> Foreign
boxelMapToForeign map = toForeign $ Array.fromFoldable (values (mapWithKey (\(BlockIndex x y z) v -> { x, y, z, blockType: write v }) map))

foreignToBoxelMap :: Foreign -> F (Map BlockIndex BlockType)
foreignToBoxelMap value = do
    blocksFn <- readArray value
    blocks <- for blocksFn \block -> do
        x <- readProp "x" block
        y <- readProp "y" block
        z <- readProp "z" block
        blockType <- readProp "blockType" block
        pure (Tuple (BlockIndex x y z) blockType)
    pure (fromFoldable blocks)
