module Graphics.Babylon.Example.Index3D where

import Control.Alt (void)
import Control.Bind (join, bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Except (except)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM)
import Control.Monad.ST (writeSTRef, readSTRef, newSTRef, pureST)
import Data.Array (fromFoldable) as Array
import Data.Array.ST (freeze, pushAllSTArray, emptySTArray)
import Data.Either (Either(Left))
import Data.Foreign (ForeignError(ForeignError), toForeign, readArray, readInt)
import Data.Foreign.Class (readProp, write, class AsForeign, class IsForeign)
import Data.Generic (gShow, class Generic)
import Data.Int (toNumber, floor)
import Data.List (List(Cons, Nil), (..))
import Data.Map (Map, member, toList, fromFoldable, mapWithKey, values)
import Data.Ord (compare, class Ord)
import Data.Ordering (Ordering(EQ))
import Data.Ring (negate)
import Data.Traversable (for)
import Data.Tuple (Tuple(Tuple))
import Data.Unit (unit)
import Graphics.Babylon.VertexData (VertexDataProps(VertexDataProps))
import PerlinNoise (createNoise, simplex2)
import Prelude (class Show, class Eq, pure, show, (<*>), (<$>), (+), (-), (*), (<>), (==), (&&), ($), (#), (>>=))

data Index3D = Index3D Int Int Int

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


