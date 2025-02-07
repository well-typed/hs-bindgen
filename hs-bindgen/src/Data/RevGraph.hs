module Data.RevGraph (
    -- * Type
    RevGraph
    -- * Construction
  , empty
    -- * Insertion
  , insertVertex
  , insertEdge
    -- * Query
  , reaches
  , reachedBy
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

{-------------------------------------------------------------------------------
  Type
-------------------------------------------------------------------------------}

-- | Directed graph that supports efficient queries of the reversed (transposed)
-- graph
--
-- Type variable @a@ represents the type of a vertex in the graph.
data RevGraph a
  = RevGraph
      { vertexMap    :: Map a Int
      , idxMap       :: Map Int a
      , forwardEdges :: IntMap IntSet
      , reverseEdges :: IntMap IntSet
      }

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | The empty graph
empty :: RevGraph a
empty = RevGraph
    { vertexMap    = Map.empty
    , idxMap       = Map.empty
    , forwardEdges = IntMap.empty
    , reverseEdges = IntMap.empty
    }

{-------------------------------------------------------------------------------
  Insertion
-------------------------------------------------------------------------------}

-- | Insert a vertex
--
-- The graph is not changed if the vertex already exists.
insertVertex :: Ord a => a -> RevGraph a -> RevGraph a
insertVertex v = snd . insertVertex' v

-- | Insert an edge
--
-- This function inserts vertices automatically.
--
-- The graph is not changed if the edge already exists.
insertEdge :: forall a. Ord a => a -> a -> RevGraph a -> RevGraph a
insertEdge vFrom vTo revGraph0 =
    let (vFromId, revGraph1) = insertVertex' vFrom revGraph0
        (vToId,   revGraph2) = insertVertex' vTo   revGraph1
    in  revGraph2 {
            forwardEdges =
              IntMap.insertWith (<>) vFromId (IntSet.singleton vToId) $
                forwardEdges revGraph2
          , reverseEdges =
              IntMap.insertWith (<>) vToId (IntSet.singleton vFromId) $
                reverseEdges revGraph2
          }

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Gets the set of vertices that are reachable from the specified vertex
--
-- The specified vertex is included in the set.
reaches :: Ord a => RevGraph a -> a -> Set a
reaches RevGraph{..} v = case Map.lookup v vertexMap of
    Just i  -> Set.fromList $ (idxMap Map.!) <$> edgeMapReaches forwardEdges i
    Nothing -> mempty

-- | Gets the set of vertices that are reachable from the specified vertex in
-- the reverse (transposed) graph
--
-- The specified vertex is included in the set.
reachedBy :: Ord a => RevGraph a -> a -> Set a
reachedBy RevGraph{..} v = case Map.lookup v vertexMap of
    Just i  -> Set.fromList $ (idxMap Map.!) <$> edgeMapReaches reverseEdges i
    Nothing -> mempty

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

-- | Insert a vertex along with the internal index.
--
-- The graph is not changed if the vertex already exists.
insertVertex' :: forall a. Ord a => a -> RevGraph a -> (Int, RevGraph a)
insertVertex' v revGraph@RevGraph{..} =
    case Map.insertLookupWithKey (\_key _new old -> old) v i' vertexMap of
      (Just i, _)      -> (i, revGraph)
      (Nothing, vMap') -> (i', revGraph { vertexMap = vMap', idxMap = iMap' })
  where
    i' :: Int
    i' = Map.size vertexMap

    iMap' :: Map Int a
    iMap' = Map.insert i' v idxMap

-- | Get a list of vertex indexes reachable from the specified vertex index in
-- the specified edge map
--
-- The specified vertex index is included in the list.
--
-- Note that 'Map' is used so that lookup and insertion can be performed at the
-- same time.
edgeMapReaches :: IntMap IntSet -> Int -> [Int]
edgeMapReaches edgeMap = aux Map.empty . pure
  where
    aux :: Map Int () -> [Int] -> [Int]
    aux acc [] = Map.keys acc
    aux acc (x:xs) =
      case Map.insertLookupWithKey (\_key _new old -> old) x () acc of
        (Just (), _)    -> aux acc xs
        (Nothing, acc') -> aux acc' $
          IntSet.toList (IntMap.findWithDefault mempty x edgeMap) ++ xs
