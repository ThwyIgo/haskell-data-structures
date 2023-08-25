module WDigraph (module WDigraph, module Graph) where

import Graph
import qualified Data.Map.Strict as Map
import Data.Maybe

newtype WDigraph v w = WDigraph (Map.Map v [(v,w)])

instance Ord v => Graph (WDigraph v w) v (v,w) where
  empty = WDigraph Map.empty

  addVertex v g@(WDigraph hm)
    | isNothing (hm Map.!? v) = WDigraph $ Map.insert v [] hm
    | otherwise = g

  addEdge o (d,w) g@(WDigraph hm) = WDigraph $ Map.insertWith f o [(d,w)] hm'
    where f new [] = new
          -- Se o vértice já for adjacente, atualizar seu peso.
          f new@[(v,w)] (h@(hv,_):t)
            | v == hv = (v, w) : t
            | otherwise = h : f new t

          -- Criar vértice de destino caso ele não exista.
          hm' = (\(WDigraph hm) -> hm) $ addVertex d g

  removeVertex v (WDigraph hm) = WDigraph $
    filter (\(a,_) -> a /= v) <$> Map.delete v hm

  removeEdge o d (WDigraph hm) = WDigraph $
    Map.adjust (filter (\(v,_) -> v /= d)) o hm

  hasVertex v (WDigraph hm) = Map.member v hm

  vertices (WDigraph hm) = Map.keys hm

  edge o d (WDigraph hm) = case hm Map.!? o of
    Just adjList -> (d,) <$> lookup d adjList
    Nothing -> Nothing

  adjList v (WDigraph hm) = hm Map.!? v

  adjVertices v (WDigraph hm) = map fst <$> hm Map.!? v

----------

edgeWeight :: Ord v
           => v -- Origin
           -> v -- Destination
           -> WDigraph v w
           -> Maybe w
edgeWeight o d g@(WDigraph hm) = adjList o g >>= lookup d

toString :: (Ord v, Show v, Show w)
         => WDigraph v w -> String
toString (WDigraph hm) = show $ Map.toList hm
