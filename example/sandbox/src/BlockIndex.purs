module Graphics.Babylon.Example.BlockIndex where

import Control.Alternative (pure)
import Control.Bind (bind)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class AsForeign, class IsForeign, readProp)
import Data.Generic (class Generic, gCompare, gEq)
import Data.Ord (class Ord)
import Prelude (class Eq, class Show, show, (<>))

data BlockIndex = BlockIndex Int Int Int

runIndex3D :: BlockIndex -> { x :: Int, y :: Int, z :: Int }
runIndex3D (BlockIndex x y z) = { x, y, z }

derive instance generic_Index3D :: Generic BlockIndex

instance eq_Index3D :: Eq BlockIndex where
    eq = gEq

instance ord_Index3D :: Ord BlockIndex where
    compare = gCompare

instance show_Show :: Show BlockIndex where
    show (BlockIndex x y z) = show x <> "," <> show y <> "," <> show z

instance isForeign_Index3D :: IsForeign BlockIndex where
    read value = do
        x <- readProp "x" value
        y <- readProp "y" value
        z <- readProp "z" value
        pure (BlockIndex x y z)

instance asForeign_Index3D :: AsForeign BlockIndex where
    write (BlockIndex x y z) = toForeign { x, y, z }


