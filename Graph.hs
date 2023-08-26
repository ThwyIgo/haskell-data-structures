{-# LANGUAGE FunctionalDependencies #-}
{- Adjacency list Graph class -}

module Graph (Graph(..), gconcat) where

import Data.Bifunctor
import Data.Maybe
import Data.List

-- g = graph
-- v = vertex
-- e = edge without origin vertex (Usually a tuple of (vertex, weight))
class Monoid g => Graph g v e | g -> v e where
  ----- Construction -----

  empty    :: g
  empty = fromList []

  fromList :: [(v,[e])] -> g
  fromList l = let addVertices g = foldr (\(v,_) g -> addVertex v g) g l
    in addVertices $
       foldr (\(v,el) g -> foldr (addEdge v) g el) empty l

  ----- Modify -----

  addVertex :: v -> g -> g

  -- If the edge already exists for a v, update it
  addEdge   :: v -> e -> g -> g

  removeVertex :: v -> g -> g
  removeEdge   :: v -> v -> g -> g

  ----- Predicates -----

  hasVertex :: v -> g -> Bool
  hasEdge   :: v -> v -> g -> Bool
  hasEdge o d g = case edge o d g of
    Nothing -> False
    Just e -> True

  ----- Query -----

  vertices    :: g -> [v]
  edge        :: v -> v -> g -> Maybe e
  adjList     :: v -> g -> Maybe [e]
  adjVertices :: v -> g -> Maybe [v]
  toList      :: g -> [(v,[e])]
  toList g = do
    v <- vertices g
    return (v, fromJust $ adjList v g)

  ----- Algorithms -----

  -- Returns the in degrees of all vertices in the graph
  inDegrees :: Eq v => g -> [(v,Int)]
  inDegrees g = let zeroMap = map (,0) (vertices g)
    in foldr f zeroMap (vertices g)
    where
      f v dict = foldr (\v1 d -> insertWith (\_ old -> old+1) v1 1 d) dict (fromJust $ adjVertices v g)

  topologicalOrder :: (Ord v,Eq v) => g -> Maybe [v]
  -- Kahn's algorithm
  topologicalOrder g = let (zeros, notZeros) = first (fmap fst) $
                             partition (\(_,d) -> d == 0) (inDegrees g)
    in helper g zeros notZeros
    where
      helper g [] degrees
        | (not . null) degrees = Nothing
        | otherwise = Just []
      helper g (zeroDegreeVert : t) degrees =
        let adjVerts = fromJust $ adjVertices zeroDegreeVert g
            (newg, newDegrees) = foldr f (g,degrees) adjVerts
            f v (g1, degrees1) = (removeEdge zeroDegreeVert v g
                                 , insertWith (\_ old -> old - 1) v (-1) degrees1)
            (zs, notZs) = first (fmap fst) $
              partition (\(_,d) -> d == 0) newDegrees
        in (zeroDegreeVert :) <$>
           helper newg (t ++ zs) notZs

----- Not exported -----

insertWith :: Eq k => (a -> a -> a) -> k -> a -> [(k,a)] -> [(k,a)]
insertWith f k new dict =
  case span (\(k1, _) -> k1 /= k) dict of
    (pre, (_,old) : t) -> pre ++ (k, f new old) : t
    _ -> (k, new) : dict

update :: Eq k => k -> a -> [(k,a)] -> [(k,a)]
update = insertWith const

----- Semigroup -----
gconcat :: Graph g v e => g -> g -> g
gconcat a b = let bVerts = vertices b
                  bEdges = map (\v -> (v, fromJust $ adjList v b)) bVerts
  in flip combineVerts bVerts $
     combineEdges a bEdges
  where
    combineVerts = foldr addVertex

    combineEdges acc [] = acc
    combineEdges acc ((v,es) : t) = flip combineEdges t $
      foldr (addEdge v) acc es
