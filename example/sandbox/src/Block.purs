module Graphics.Babylon.Example.Sandbox.Block where

import Graphics.Babylon.Example.Sandbox.BlockIndex (BlockIndex)
import Graphics.Babylon.Example.Sandbox.BlockType (BlockType)

newtype Block = Block {
    index :: BlockIndex,
    blockType :: BlockType
}


