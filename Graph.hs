{-# LANGUAGE FunctionalDependencies, RecordWildCards, DuplicateRecordFields #-}
{- Adjacency list Graph class -}

module Graph (Graph(..), gconcat) where

import Data.Bifunctor
import Data.Maybe
import Data.List

-- g = graph
-- v = vertex
-- e = edge without origin vertex (Usually a tuple of (vertex, weight))
class (Eq v, Monoid g) => Graph g v e | g -> v e where
  ----- Construction -----

  empty    :: g
  empty = fromList []

  fromList :: [(v,[e])] -> g
  fromList l = let addVertices g = foldr (\(v,_) g -> addVertex v g) g l
    in addVertices $
       foldr (\(v,el) g -> foldr (addEdge v) g el) empty l

  fromEdgeList :: [(v,e)] -> g
  fromEdgeList = foldr (\(v,e) g -> addEdge v e g) empty

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

  findBridges :: g -> [(v,v)]
  findBridges g = let istate = BS 0 [] []
                      vs = vertices g
                      loop v s@BS{..} = if isNothing $ lookup v inlow
                                        then dfs v Nothing s
                                        else s -- Vertex already visited
    in bridges $ foldr loop istate vs
    where
      dfs v parent BS{..} =
        let inlowv   = update v (counter, counter) inlow
            newState = BS (counter + 1) inlowv bridges
            adjvs = case parent of
              Just p -> delete p . fromJust $ adjVertices v g
              Nothing -> fromJust $ adjVertices v g
        in foldr (f v) newState adjvs

      f v adjv state@BS{..}
        | isNothing $ lookup adjv inlow =
            let dfsState@BS{inlow=il,bridges=bs} = dfs adjv (Just v) state

                Just (_,lowadjv) = lookup adjv il
                Just (inv,lowv)  = lookup v il
                newInLow         = update v (inv, min lowv lowadjv) il
                updBridges = if lowadjv > inv
                             then (v, adjv) : bs
                             else bs
            in dfsState { inlow   = newInLow
                        , bridges = updBridges }

        | otherwise =
            let Just (inadjv,_) = lookup adjv inlow
                Just (inv,lowv) = lookup v inlow
                newInlow        = update v (inv, min lowv inadjv) inlow
            in state {inlow=newInlow, bridges}

  scc :: g -> [[v]]
  -- Tarjan's algorithm
  scc g = let loop v s@SCCS{inlow} = if isNothing $ lookup v inlow
                                     then dfs v s
                                     else s
    in sccs $ foldr loop (SCCS 0 [] [] []) (vertices g)
    where
      dfs v SCCS{..} =
        let newInlow = update v (counter, counter) inlow
            state    = SCCS (counter + 1) newInlow (v : stack) sccs
            r@SCCS{inlow=ril, stack=rs, sccs=rsccs} =
              foldr (f v) state (fromJust $ adjVertices v g)
            Just (inv, lowv) = lookup v ril

        in if inv == lowv -- v is the beginning of a component
        -- Remove from the stack the vertices that are part of the component
           then let (component, fs) = splitAt ((+1) . fromJust $ elemIndex v rs) rs
                in r {stack=fs, sccs=component : rsccs}
           else r

      f v adjv state@SCCS{..}
        | isNothing $ lookup adjv inlow = -- adjv unvisited
            let r@SCCS{..}       = dfs adjv state
                Just (_,lowadjv) = lookup adjv inlow
                Just (inv,lowv)  = lookup v inlow
                newInlow         = update v (inv, min lowv lowadjv) inlow
            in r {inlow=newInlow, sccs}

        | adjv `elem` stack = -- adjv belongs to current component
            let Just (inv, lowv) = lookup v inlow
                Just (inadjv,_)  = lookup adjv inlow
                newInlow         = update v (inv, min lowv inadjv) inlow
            in state {inlow=newInlow, sccs}

        | otherwise = state -- adjv belongs to another component

----- Semigroup -----
gconcat :: Graph g v e => g -> g -> g
gconcat a b = let bVerts = vertices b
                  bEdges = map (\v -> (v, fromJust $ adjList v b)) bVerts
  in combineEdges a bEdges
  where
    combineEdges acc [] = acc
    combineEdges acc ((v,es) : t) = flip combineEdges t $
      foldr (addEdge v) (addVertex v acc) es

----- Not exported -----

type ListMap k a = [(k,a)]
data BridgeState v = BS { counter :: Int
                        , inlow   :: ListMap v (Int,Int)
                        , bridges :: [(v,v)]
                        }
data SCCState v = SCCS { counter :: Int
                       , inlow :: ListMap v (Int,Int)
                       , stack :: [v]
                       , sccs :: [[v]]
                       }

insertWith :: Eq k => (a -> a -> a) -> k -> a -> ListMap k a -> ListMap k a
insertWith f k new dict =
  case span (\(k1, _) -> k1 /= k) dict of
    (pre, (_,old) : t) -> pre ++ (k, f new old) : t
    _ -> (k, new) : dict

update :: Eq k => k -> a -> ListMap k a -> ListMap k a
update = insertWith const
