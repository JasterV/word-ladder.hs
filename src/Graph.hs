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
  )
where

import qualified Data.AssocMap as M
import qualified Data.List as L

type DiGraph a = M.AssocMap a [a]

empty :: DiGraph a
empty = M.empty

hasNode :: (Eq a) => DiGraph a -> a -> Bool
hasNode = flip M.member

addNode :: (Eq a) => DiGraph a -> a -> DiGraph a
addNode graph node = M.insert node [] graph

addEdge :: (Eq a) => (a, a) -> DiGraph a -> DiGraph a
addEdge (node, child) = M.alter insertEdge node
  where
    insertEdge Nothing = Just [child]
    insertEdge (Just nodes) = Just $ L.nub (child : nodes)

addEdges :: (Eq a) => [(a, a)] -> DiGraph a -> DiGraph a
addEdges edges graph = foldr addEdge graph edges

buildDiGraph :: (Eq a) => [(a, [a])] -> DiGraph a
buildDiGraph = foldr (\(node, childs) -> M.insert node (L.nub childs)) M.empty

children :: (Eq a) => a -> DiGraph a -> [a]
children = M.findWithDefault []

deleteNode :: (Eq a) => a -> DiGraph a -> DiGraph a
deleteNode = M.delete

deleteNodes :: (Eq a) => [a] -> DiGraph a -> DiGraph a
deleteNodes nodes graph = foldr deleteNode graph nodes

deleteEdge :: (Eq a) => (a, a) -> DiGraph a -> DiGraph a
deleteEdge (node, child) = M.alter aux node
  where
    aux Nothing = Nothing
    aux (Just nodes) = Just (L.delete child nodes)

deleteEdges :: (Eq a) => [(a, a)] -> DiGraph a -> DiGraph a
deleteEdges edges graph = foldr deleteEdge graph edges
