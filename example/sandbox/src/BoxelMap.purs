module Graphics.Babylon.Example.Sandbox.BoxelMap (BoxelMap, empty, lookup, insert, delete) where

import Data.Foldable (class Foldable, foldl)
import Data.Functor (class Functor)
import Data.List (List) as List
import Data.Map (lookup)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Tuple (Tuple(..))
import Graphics.Babylon.Example.Sandbox.LocalIndex (LocalIndex)

foreign import data BoxelMap :: * -> *

instance functor_ShowMap :: Functor BoxelMap where
    map = mapBoxelMap

foreign import mapBoxelMap :: forall a b. (a -> b) -> BoxelMap a -> BoxelMap b

foreign import _lookup :: forall a. LocalIndex -> BoxelMap a -> Nullable a

lookup :: forall a. LocalIndex -> BoxelMap a -> Maybe a
lookup key map = toMaybe (_lookup key map)

foreign import member :: forall a. LocalIndex -> BoxelMap a -> Boolean

foreign import insert :: forall a. LocalIndex -> a -> BoxelMap a -> BoxelMap a

foreign import delete :: forall a. LocalIndex -> BoxelMap a -> BoxelMap a

foreign import empty :: forall a. BoxelMap a

fromFoldable :: forall f a. (Foldable f) => f (Tuple LocalIndex a) -> BoxelMap a
fromFoldable f = foldl (\m (Tuple k v) -> insert k v m) empty f

fromList :: forall a. List.List (Tuple LocalIndex a) -> BoxelMap a
fromList f = foldl (\m (Tuple k v) -> insert k v m) empty f

foreign import toArray :: forall a. BoxelMap a -> Array { key :: LocalIndex, value :: a }

foreign import size :: forall a. BoxelMap a -> Int

--
