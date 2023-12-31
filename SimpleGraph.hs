module SimpleGraph (SGraph, Graph(..)) where

import Graph

import qualified Data.Map as Map
import Data.Maybe

newtype SGraph v = SGraph (Map.Map v [v])

instance Ord v => Graph (SGraph v) v v where
  empty = SGraph Map.empty

  addVertex v g@(SGraph hm)
    | Map.member v hm = g
    | otherwise = SGraph $ Map.insert v [] hm

  addEdge o d g@(SGraph hm) = SGraph $ Map.insertWith f d [o] $
                              Map.insertWith f o [d] hm
    where f new [] = new
          f new@[v] l
            | v `elem` l = l
            | otherwise = v : l

  removeVertex v (SGraph hm) = SGraph $
    filter (/= v) <$> Map.delete v hm

  removeEdge o d (SGraph hm) = SGraph $
    Map.adjust (filter (/= d)) o hm

  hasVertex v (SGraph hm) = Map.member v hm

  vertices (SGraph hm) = Map.keys hm

  edge o d g = do
    adjes <- adjList o g
    if d `elem` adjes
      then Just d
      else Nothing

  adjList v (SGraph hm) = hm Map.!? v

  adjVertices = adjList

instance Ord v => Semigroup (SGraph v) where
  (<>) = gconcat

instance Ord v => Monoid (SGraph v) where
  mempty = empty
