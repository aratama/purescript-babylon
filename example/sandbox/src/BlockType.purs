module Graphics.Babylon.Example.Sandbox.BlockType where

import Data.Generic (class Generic, gEq, gShow)
import Prelude (class Eq, class Show)

newtype BlockType = BlockType Int

grassBlock :: BlockType
grassBlock = BlockType 0

waterBlock :: BlockType
waterBlock = BlockType 1

derive instance generic_BlockType :: Generic BlockType

instance eq_BlockType :: Eq BlockType where
    eq = gEq

instance show_BlockType :: Show BlockType where
    show = gShow

