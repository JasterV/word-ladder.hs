module Ladder (readDictionary, mkLadderGraph) where

import Data.Char (isLower)
import qualified Data.List as L
import qualified Graph as G
import qualified PermutationMap as PM
import Prelude hiding (lines, words)

type Dictionary = [String]

readDictionary :: String -> IO Dictionary
readDictionary filename = do
  text <- readFile filename
  let lines = L.lines text
  let lowercaseWords = map (filter isLower) lines
  return (L.nub lowercaseWords)

mkLadderGraph :: Dictionary -> G.DiGraph String
mkLadderGraph dict = G.buildDiGraph [(word, computeCandidates permMap word) | word <- dict]
  where
    permMap = PM.createPermutationMap dict

computeCandidates :: PM.PermutationMap -> String -> [String]
computeCandidates permMap word =
  -- Delete the original word from the permutations list
  L.delete word permutations
  where
    removed = [L.delete c word | c <- word]
    added = [c : word | c <- ['a' .. 'z']]
    modified = [x : L.delete y word | x <- ['a' .. 'z'], y <- word, x /= y]
    -- Sort and deduplicate all the candidates
    candidates = L.nub $ map L.sort (added ++ removed ++ modified ++ [word])
    -- For each candidate, lookup all its permutations
    permutations = L.concatMap (\w -> PM.findWithDefault [] w permMap) candidates
