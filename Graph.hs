{-# LANGUAGE FunctionalDependencies #-}
{- Adjacency list Graph class -}

module Graph (Graph(..)) where

import Data.Maybe
import Data.List
import qualified Control.Applicative as CA
import Control.Monad.State.Strict
import Debug.Trace

-- g = graph
-- v = vertex
-- e = edge without origin vertex (Usually a tuple of (vertex, adjList))
class Graph g v e | g -> v e where
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

  topologicalOrder :: (Show v, Eq v) => g -> Maybe [v]
  topologicalOrder g = evalState state (map (,Unvisited) (vertices g))
    where
      firstUnvisited :: [(v, SearchStatus)] -> Maybe v
      firstUnvisited dict = fst <$> find (\(_,ss) -> ss == Unvisited) dict
      state = do
        d <- get
        case firstUnvisited d of
          Nothing -> return $ Just []
          Just u -> do
            let (ml, fd) = visit u d
            put fd
            return $ CA.liftA2 (++) (evalState state fd) ml

      -- Lista tem que ser global de algum modo
      visit :: v -> [(v,SearchStatus)] -> (Maybe [v], [(v,SearchStatus)])
      visit v dict =  case lookup v dict of
        Just Visited -> (Just [], dict)
        Just Visiting -> trace "ERRO" (Nothing, dict)
        Just Unvisited -> let
          updatedDict = update v Visiting dict
          vchildren _ (Nothing, d) = (Nothing, d)
          vchildren v1 (Just l, d) = let (ml, fd) = visit v1 d
                                     in (fmap (++ l) ml, fd)
          (mfl, fd) = foldr vchildren (Just [],updatedDict) (fromJust $ adjVertices v g)
          in  ((v :) <$> mfl, update v Visited fd)

----- Not exported -----

insertWith :: Eq k => (a -> a -> a) -> k -> a -> [(k,a)] -> [(k,a)]
insertWith f k new dict =
  case span (\(k1, _) -> k1 /= k) dict of
    (pre, (_,old) : t) -> pre ++ (k, f new old) : t
    _ -> (k, new) : dict

update :: Eq k => k -> a -> [(k,a)] -> [(k,a)]
update = insertWith const

data SearchStatus = Unvisited | Visiting | Visited deriving Eq
