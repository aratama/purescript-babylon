module Graphics.Babylon.Example.Index3D where

import Data.Ord (compare, class Ord)
import Data.Ordering (Ordering(EQ))

import Prelude (class Eq, class Show, show, (&&), (<>), (==))

data Index3D = Index3D Int Int Int

runIndex3D :: Index3D -> { x :: Int, y :: Int, z :: Int }
runIndex3D (Index3D x y z) = { x, y, z }

instance eq_Index3D :: Eq Index3D where
    eq (Index3D ax ay az) (Index3D bx by bz) = (ax == bx) && (ay == by) && (az == bz)

instance ord_Index3D :: Ord Index3D where
    compare (Index3D ax ay az) (Index3D bx by bz) = case compare ax bx of
        EQ -> case compare ay by of
            EQ -> compare az bz
            m -> m
        n -> n

instance show_Show :: Show Index3D where
    show (Index3D x y z) = show x <> "," <> show y <> "," <> show z


