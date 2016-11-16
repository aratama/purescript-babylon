module Graphics.Babylon.Example.Sandbox.BlockType where

import Data.Generic (class Generic, gEq, gShow)
import Data.Newtype (class Newtype)
import Prelude (class Eq, class Show)

newtype BlockType = BlockType Int

airBlock :: BlockType
airBlock = BlockType 0

grassBlock :: BlockType
grassBlock = BlockType 1

waterBlock :: BlockType
waterBlock = BlockType 2

type BlockTypes = {
    airBlock :: BlockType,
    grassBlock :: BlockType,
    waterBlock :: BlockType
}

blockTypes :: BlockTypes
blockTypes = {
    airBlock: airBlock,
    grassBlock: grassBlock,
    waterBlock: waterBlock
}

derive instance generic_BlockType :: Generic BlockType

derive instance newtype_BlockType :: Newtype BlockType _

instance eq_BlockType :: Eq BlockType where
    eq = gEq

instance show_BlockType :: Show BlockType where
    show = gShow

