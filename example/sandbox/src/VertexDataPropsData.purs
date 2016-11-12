module Graphics.Babylon.Example.VertexDataPropsData (VertexDataPropsData(VertexDataPropsData)) where

import Control.Bind (bind)
import Data.Foreign (toForeign)
import Data.Foreign.Class (readProp, write, class AsForeign, class IsForeign)
import Prelude (pure, ($))

import Graphics.Babylon.VertexData (VertexDataProps)
import Graphics.Babylon.Example.Chunk (Chunk)

newtype VertexDataPropsData = VertexDataPropsData {
    terrain :: Chunk,
    grassBlocks :: VertexDataProps,
    waterBlocks :: VertexDataProps
}

instance isForeign_VertexDataPropsData :: IsForeign VertexDataPropsData where
    read value = do
        terrain <- readProp "terrain" value
        grassBlocks <- readProp "grassBlocks" value
        waterBlocks <- readProp "waterBlocks" value
        pure $ VertexDataPropsData { terrain, grassBlocks, waterBlocks }

instance asForeign_VertexDataPropsData :: AsForeign VertexDataPropsData where
    write (VertexDataPropsData value) = toForeign {
        terrain: write value.terrain,
        grassBlocks: write value.grassBlocks,
        waterBlocks: write value.waterBlocks
    }

