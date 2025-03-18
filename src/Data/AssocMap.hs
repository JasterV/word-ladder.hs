module Data.AssocMap
  ( AssocMap,
    empty,
    insert,
    delete,
    alter,
    member,
    lookup,
    findWithDefault,
  )
where

import qualified Data.List as List
import Data.Maybe (fromMaybe, isJust)
import Prelude hiding (lookup)

newtype AssocMap k v = AssocMap [(k, v)]
  deriving (Show)

empty :: AssocMap k v
empty = AssocMap []

delete :: (Eq k) => k -> AssocMap k v -> AssocMap k v
delete = alter (const Nothing)

insert :: (Eq k) => k -> v -> AssocMap k v -> AssocMap k v
insert key value = alter (const (Just value)) key

member :: (Eq a) => a -> AssocMap a b -> Bool
member x (AssocMap xs) = isJust (List.lookup x xs)

lookup :: (Eq k) => k -> AssocMap k v -> Maybe v
lookup key (AssocMap xs) = List.lookup key xs

findWithDefault :: (Eq k) => v -> k -> AssocMap k v -> v
findWithDefault defaultValue key assocMap = fromMaybe defaultValue (lookup key assocMap)

alter :: (Eq k) => (Maybe v -> Maybe v) -> k -> AssocMap k v -> AssocMap k v
alter f key (AssocMap xs) = AssocMap (alter' xs)
  where
    {-
      If list is empty, it means the key is missing, so we evaluate the provided function with Nothing.
      If the function evaluates to Nothing, we keep the list empty.
      If the function evaluates to a value, we add a mapping between key and value.
    -}
    alter' [] = maybe [] (\value -> [(key, value)]) (f Nothing)
    {-
    If the list is not empty, we recursively search for a mapping that has the provided key.
    If the there is a mapping with the provided key, we call the function passing the found value.
    If the function evaluates to Nothing, we delete the mapping.
    If the function evaluates to a value, we update the mapping.
    -}
    alter' ((key', value') : xs')
      | key == key' = maybe xs' (\value -> (key, value) : xs') (f (Just value'))
      | otherwise = (key', value') : alter' xs'
