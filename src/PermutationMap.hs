module PermutationMap
  ( PermutationMap,
    empty,
    member,
    alter,
    delete,
    insert,
    lookup,
    findWithDefault,
    createPermutationMap,
  )
where

import qualified Data.AssocMap as M
import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

type PermutationMap = M.AssocMap String [String]

empty :: PermutationMap
empty = M.empty

member :: String -> PermutationMap -> Bool
member key = M.member (sort key)

alter :: (Maybe [String] -> Maybe [String]) -> String -> PermutationMap -> PermutationMap
alter f key = M.alter f (sort key)

delete :: String -> PermutationMap -> PermutationMap
delete key = M.delete (sort key)

insert :: String -> [String] -> PermutationMap -> PermutationMap
insert key = M.insert (sort key)

lookup :: String -> PermutationMap -> Maybe [String]
lookup key = M.lookup (sort key)

findWithDefault :: [String] -> String -> PermutationMap -> [String]
findWithDefault defaultValue key pmap = fromMaybe defaultValue (PermutationMap.lookup key pmap)

createPermutationMap :: [String] -> PermutationMap
createPermutationMap = aux empty
  where
    aux permMap [] = permMap
    aux permMap (x : xs) = aux (insertPermutation x permMap) xs

    insertPermutation word = alter (insertList word) word

    insertList word Nothing = Just [word]
    insertList word (Just xs) = Just $ nub (word : xs)
