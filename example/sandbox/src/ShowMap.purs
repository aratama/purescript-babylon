module Data.ShowMap (ShowMap, lookup, member, insert, delete, fromFoldable, toList, empty) where

import Control.Alt (alt)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.List (List)
import Data.Map (lookup)
import Data.Maybe (Maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Semigroup (class Semigroup, append)
import Data.Semiring (class Semiring)
import Data.Show (class Show, show)
import Data.StrMap (StrMap, lookup, member, insert, delete, fromFoldable, toList, empty) as StrMap
import Data.Tuple (Tuple(..))

newtype ShowMap k a = ShowMap (StrMap.StrMap a)

instance semigroup_ShowMap :: (Semigroup a) => Semigroup (ShowMap k a) where
    append (ShowMap a) (ShowMap b) = ShowMap (append a b)

instance monoid_ShowMap :: (Monoid a) => Monoid (ShowMap k a) where
    mempty = ShowMap mempty

instance foldable_ShowMap :: Foldable (ShowMap k) where
    foldMap f (ShowMap m) = foldMap f m
    foldl f b (ShowMap m) = foldl f b m
    foldr f b (ShowMap m) = foldr f b m

lookup :: forall k a. (Show k) => k -> ShowMap k a -> Maybe a
lookup k (ShowMap m) = StrMap.lookup (show k) m

member :: forall k a. (Show k) => k -> ShowMap k a -> Boolean
member k (ShowMap m) = StrMap.member (show k) m

insert :: forall k a. (Show k) => k -> a -> ShowMap k a -> ShowMap k a
insert k v (ShowMap m) = ShowMap (StrMap.insert (show k) v m)

delete :: forall k a. (Show k) => k -> ShowMap k a -> ShowMap k a
delete k (ShowMap m) = ShowMap (StrMap.delete (show k) m)

empty :: forall k a. ShowMap k a
empty = ShowMap StrMap.empty

fromFoldable :: forall f k a. (Show k, Foldable f) => f (Tuple k a) -> ShowMap k a
fromFoldable f = foldl (\m (Tuple k v) -> insert k v m) empty f

toList :: forall k a. ShowMap k a -> List (Tuple String a)
toList (ShowMap m) = StrMap.toList m
