module SimpleGraph (SGraph, module Graph) where

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
                              Map.insertWith f o [d] hm'
    where f new [] = new
          f new@[v] (h : t)
            | v == h = h:t
            | otherwise = h : f new t

          hm' = (\(SGraph hm) -> hm) $ addVertex d g

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
