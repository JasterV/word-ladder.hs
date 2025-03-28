{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map once" #-}

module Graph
  ( DiGraph,
    hasNode,
    addNode,
    empty,
    addEdge,
    addEdges,
    buildDiGraph,
    children,
    deleteEdge,
    deleteEdges,
    deleteNode,
    deleteNodes,
    bfsSearch,
    dfsSearch,
  )
where

import Data.Bifunctor (second)
import Data.Function ((&))
import qualified Data.HashMap.Lazy as M
import Data.Hashable (Hashable)
import qualified Data.List as L

type DiGraph a = M.HashMap a [a]

empty :: DiGraph a
empty = M.empty

hasNode :: (Hashable a) => DiGraph a -> a -> Bool
hasNode = flip M.member

addNode :: (Hashable a) => DiGraph a -> a -> DiGraph a
addNode graph node = M.insert node [] graph

addEdge :: (Hashable a) => (a, a) -> DiGraph a -> DiGraph a
addEdge (node, child) = M.alter insertEdge node
  where
    insertEdge Nothing = Just [child]
    insertEdge (Just nodes) = Just $ L.nub (child : nodes)

addEdges :: (Hashable a) => [(a, a)] -> DiGraph a -> DiGraph a
addEdges edges graph = foldr addEdge graph edges

buildDiGraph :: (Hashable a) => [(a, [a])] -> DiGraph a
buildDiGraph = foldr (\(node, childs) -> M.insert node (L.nub childs)) M.empty

children :: (Hashable a) => a -> DiGraph a -> [a]
children = M.findWithDefault []

deleteNode :: (Hashable a) => a -> DiGraph a -> DiGraph a
deleteNode = M.delete

deleteNodes :: (Hashable a) => [a] -> DiGraph a -> DiGraph a
deleteNodes nodes graph = foldr deleteNode graph nodes

deleteEdge :: (Hashable a) => (a, a) -> DiGraph a -> DiGraph a
deleteEdge (node, child) = M.alter aux node
  where
    aux Nothing = Nothing
    aux (Just nodes) = Just (L.delete child nodes)

deleteEdges :: (Hashable a) => [(a, a)] -> DiGraph a -> DiGraph a
deleteEdges edges graph = foldr deleteEdge graph edges

type SearchState a = ([a], DiGraph a, DiGraph a)

type SearchResult a = Maybe (DiGraph a)

bfsSearch :: forall a. (Hashable a) => DiGraph a -> a -> a -> Maybe [a]
bfsSearch initialGraph start end
  | start == end = Just [start]
  | otherwise = findSolution <$> bfsSearch' ([start], initialGraph, empty)
  where
    findSolution :: DiGraph a -> [a]
    findSolution predecessors = L.reverse (aux end)
      where
        aux node = case children node predecessors of
          [] -> [node]
          (x : _) -> node : aux x

    addMultiplePredecessors :: [(a, [a])] -> DiGraph a -> DiGraph a
    addMultiplePredecessors [] graph = graph
    addMultiplePredecessors ((node, childs) : xs) graph =
      let edges = L.map (,node) childs
       in addMultiplePredecessors xs (addEdges edges graph)

    bfsSearch' :: SearchState a -> SearchResult a
    bfsSearch' ([], _, _) = Nothing
    bfsSearch' (frontier, graph, predecessors) =
      let graph' =
            -- Create a new graph with the frontier nodes removed
            deleteNodes frontier graph
          neighboursMap =
            -- Associate each node to its neighbours
            L.map (\node -> (node, children node graph)) frontier
              -- Filter the neighbours that have already been visited
              & L.map (second $ L.filter (`M.member` graph'))

          -- Add all the neighbours of each node as predecessors of the node
          predecessors' = addMultiplePredecessors neighboursMap predecessors

          -- Put together all the neighbours into the next frontier
          frontier' = L.concatMap snd neighboursMap
       in if end `L.elem` frontier'
            then
              Just predecessors'
            else
              bfsSearch' (frontier', graph', predecessors')

dfsSearch :: forall a. (Hashable a) => DiGraph a -> a -> a -> Maybe [a]
dfsSearch initialGraph start end =
  case dfsSearch' initialGraph start of
    Right path -> Just path
    Left _ -> Nothing
  where
    dfsSearch' :: DiGraph a -> a -> Either (DiGraph a) [a]
    dfsSearch' graph node
      | node == end = Right [node]
      | otherwise =
          let graph' = deleteNode node graph -- Mark node as visited
              neighbours = children node graph -- Get neighbouring nodes
           in case searchNeighbours neighbours graph' of
                Right path -> Right (node : path)
                Left graph'' -> Left graph''

    searchNeighbours :: [a] -> DiGraph a -> Either (DiGraph a) [a]
    searchNeighbours [] graph = Left graph
    searchNeighbours (x : xs) graph = case dfsSearch' graph x of
      -- If a path was found, just return it
      Right path -> Right path
      -- If no path was found, keep searching on the updated graph
      Left graph' -> searchNeighbours xs graph'
