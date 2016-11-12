module Graphics.Babylon.Example.BlockIndex (BlockIndex, blockIndex, runIndex3D) where

import Control.Alternative (pure)
import Data.Foreign (toForeign, unsafeFromForeign)
import Data.Foreign.Class (class AsForeign, class IsForeign)
import Data.Generic (class Generic, gCompare, gEq)
import Data.Ord (class Ord)
import Prelude (class Eq, class Show, show, (<>), (+), (*))


newtype BlockIndex = BlockIndex Int

foreign import blockIndex :: Int -> Int -> Int -> BlockIndex

foreign import runIndex3D :: BlockIndex -> { x :: Int, y :: Int, z :: Int }

derive instance generic_Index3D :: Generic BlockIndex

instance eq_Index3D :: Eq BlockIndex where
    eq = gEq

instance ord_Index3D :: Ord BlockIndex where
    compare = gCompare

instance show_Show :: Show BlockIndex where
    show (BlockIndex i) = show i

instance isForeign_Index3D :: IsForeign BlockIndex where
    read value = pure (unsafeFromForeign value)

instance asForeign_Index3D :: AsForeign BlockIndex where
    write = toForeign


