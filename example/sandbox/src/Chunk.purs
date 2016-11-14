module Graphics.Babylon.Example.Sandbox.Chunk where

import Data.Foreign (toForeign, unsafeFromForeign)
import Data.Foreign.Class (class AsForeign, class IsForeign)
import Data.ShowMap (ShowMap)
import Graphics.Babylon.Example.Sandbox.Block (Block)
import Graphics.Babylon.Example.Sandbox.BlockIndex (BlockIndex)
import Graphics.Babylon.Example.Sandbox.ChunkIndex (ChunkIndex)
import Prelude (pure)

newtype Chunk = Chunk {
    index :: ChunkIndex,
    map :: ShowMap BlockIndex Block
}

instance isForeign_TerrainMap :: IsForeign Chunk where
    read value = pure (unsafeFromForeign value)

instance asForeign_TerrainMap :: AsForeign Chunk where
    write = toForeign
