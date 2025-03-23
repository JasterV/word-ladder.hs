module Ladder (readDictionary, ladderSolve) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Char (isLower)
import qualified Data.List as L
import qualified Graph as G
import qualified PermutationMap as PM
import Prelude hiding (lines, words)

type Dictionary = [BS.ByteString]

ladderSolve :: Dictionary -> String -> String -> Maybe [BS.ByteString]
ladderSolve dict start end =
  let graph = mkLadderGraph dict
   in G.bfsSearch graph (C.pack start) (C.pack end)

readDictionary :: String -> IO Dictionary
readDictionary filepath = do
  text <- C.readFile filepath
  let lines = C.lines text
  let lowercaseWords = map (C.filter isLower) lines
  return lowercaseWords

mkLadderGraph :: Dictionary -> G.DiGraph BS.ByteString
mkLadderGraph dict = G.buildDiGraph [(word, computeCandidates word) | word <- dict]
  where
    permMap = PM.createPermutationMap dict

    computeCandidates :: BS.ByteString -> [BS.ByteString]
    computeCandidates word =
      -- Delete the original word from the permutations list
      L.delete word permutations
      where
        added = [C.cons c word | c <- ['a' .. 'z']]
        removed = [delete c word | c <- C.unpack word]
        modified =
          [C.cons x (delete y word) | x <- ['a' .. 'z'], y <- C.unpack word, x /= y]
        -- Sort and deduplicate all the candidates
        candidates = added ++ removed ++ modified ++ [word]
        -- For each candidate, lookup all its permutations
        permutations = L.concatMap (\w -> PM.findWithDefault [] w permMap) candidates

    delete :: Char -> BS.ByteString -> BS.ByteString
    delete ch string = case C.uncons string of
      Just (x, xs) -> if ch == x then xs else C.cons x (delete ch xs)
      Nothing -> C.empty
