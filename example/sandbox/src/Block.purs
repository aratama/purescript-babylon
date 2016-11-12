module Graphics.Babylon.Example.Block where

import Control.Alternative (pure)
import Control.Bind (bind)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class AsForeign, class IsForeign, readProp, write)
import Graphics.Babylon.Example.BlockIndex (BlockIndex)
import Graphics.Babylon.Example.BlockType (BlockType)

newtype Block = Block {
    index :: BlockIndex,
    blockType :: BlockType
}

