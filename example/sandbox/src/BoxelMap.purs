module Data.BoxelMap (BoxelMap, lookup, member, insert, delete, fromFoldable, toList, empty, size, fromStrMap, isEmpty) where

import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Functor (class Functor, (<$>))
import Data.Int (floor)
import Data.List (List, fromFoldable) as List
import Data.Maybe (Maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Semigroup (class Semigroup, append)
import Data.Show (class Show, show)
import Data.StrMap (StrMap, delete, empty, insert, lookup, member, toList, size, isEmpty) as StrMap
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (Tuple(..))

foreign import data BoxelMap :: * -> * -> *

instance functor_ShowMap :: (Show k) => Functor (BoxelMap k) where
    map = mapBoxelMap

{-

instance semigroup_ShowMap :: (Semigroup a) => Semigroup (BoxelMap k a) where
    append (BoxelMap a) (BoxelMap b) = BoxelMap (append a b)

instance monoid_ShowMap :: (Monoid a) => Monoid (BoxelMap k a) where
    mempty = BoxelMap mempty

instance foldable_ShowMap :: Foldable (BoxelMap k) where
    foldMap f (BoxelMap m) = foldMap f m
    foldl f a (BoxelMap m) = foldl f a m
    foldr f a (BoxelMap m) = foldr f a m



instance traversable_ShowMap :: Traversable (BoxelMap k) where
    traverse f (BoxelMap m) = map BoxelMap (traverse f m)
    sequence (BoxelMap m) = map BoxelMap (sequence m)
-}

foreign import mapBoxelMap :: forall k a b. (Show k) => (a -> b) -> BoxelMap k a -> BoxelMap k b

foreign import _lookup :: forall k a. (Show k) => k -> BoxelMap k a -> Nullable a

lookup :: forall k a. (Show k) => k -> BoxelMap k a -> Maybe a
lookup key map = toMaybe (_lookup key map)

foreign import member :: forall k a. (Show k) => k -> BoxelMap k a -> Boolean

foreign import insert :: forall k a. (Show k) => k -> a -> BoxelMap k a -> BoxelMap k a

foreign import delete :: forall k a. (Show k) => k -> BoxelMap k a -> BoxelMap k a

foreign import empty :: forall k a. BoxelMap k a

fromFoldable :: forall f k a. (Show k, Foldable f) => f (Tuple k a) -> BoxelMap k a
fromFoldable f = foldl (\m (Tuple k v) -> insert k v m) empty f

fromList :: forall k a. (Show k) => List.List (Tuple k a) -> BoxelMap k a
fromList f = foldl (\m (Tuple k v) -> insert k v m) empty f

toList :: forall k a. (Show k) => BoxelMap k a -> List.List (Tuple String a)
toList map = (\entry -> Tuple entry.key entry.value) <$> List.fromFoldable (toArray map)

foreign import toArray :: forall k a. (Show k) => BoxelMap k a -> Array { key :: String, value :: a }

foreign import size :: forall k a. BoxelMap k a -> Int

foreign import fromStrMap :: forall k a. StrMap.StrMap a -> BoxelMap k a

foreign import isEmpty :: forall k a. BoxelMap k a -> Boolean
