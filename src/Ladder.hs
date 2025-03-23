module Ladder (readDictionary, ladderSolve) where

import Data.Char (isLower)
import qualified Data.List as L
import qualified Graph as G
import qualified PermutationMap as PM
import Prelude hiding (lines, words)

type Dictionary = [String]

ladderSolve :: Dictionary -> String -> String -> Maybe [String]
ladderSolve dict start end =
  let graph = mkLadderGraph dict
   in G.bfsSearch graph start end

readDictionary :: String -> IO Dictionary
readDictionary filename = do
  text <- readFile filename
  let lines = L.lines text
  let lowercaseWords = map (filter isLower) lines
  return (L.nub lowercaseWords)

mkLadderGraph :: Dictionary -> G.DiGraph String
mkLadderGraph dict = G.buildDiGraph [(word, computeCandidates word) | word <- dict]
  where
    permMap = PM.createPermutationMap dict

    computeCandidates :: String -> [String]
    computeCandidates word =
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
