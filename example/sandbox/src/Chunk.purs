module Graphics.Babylon.Example.Chunk where

import Data.Foreign (toForeign, unsafeFromForeign)
import Data.Foreign.Class (class AsForeign, class IsForeign)
import Data.StrMap (StrMap)
import Graphics.Babylon.Example.ChunkIndex (ChunkIndex)
import Prelude (pure)
import Graphics.Babylon.Example.Block (Block)

newtype Chunk = Chunk { index :: ChunkIndex, map :: StrMap Block }

instance isForeign_TerrainMap :: IsForeign Chunk where
    read value = pure (unsafeFromForeign value)

instance asForeign_TerrainMap :: AsForeign Chunk where
    write = toForeign
