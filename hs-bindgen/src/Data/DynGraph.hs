module Data.DynGraph (
    -- * Type
    DynGraph
    -- * Construction
  , empty
    -- * Insertion
  , insertVertex
  , insertEdge
    -- * Query
  , vertices
  , reaches
    -- * Debugging
  , dumpMermaid
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

-- | Directed graph that supports dynamic insertion and a 'reaches' query
--
-- Type variable @a@ represents the type of a vertex in the graph.  Internally,
-- each value is mapped to an 'Int' index that is used in the representation of
-- the graph.
data DynGraph a
  = DynGraph
      { vtxMap :: Map a Int
      , idxMap :: Map Int a
      , edges  :: IntMap IntSet
      }

deriving instance Show a => Show (DynGraph a)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | The empty graph
empty :: DynGraph a
empty = DynGraph
    { vtxMap = Map.empty
    , idxMap = Map.empty
    , edges  = IntMap.empty
    }

{-------------------------------------------------------------------------------
  Insertion
-------------------------------------------------------------------------------}

-- | Insert a vertex
--
-- The graph is not changed if the vertex already exists.
insertVertex :: Ord a => a -> DynGraph a -> DynGraph a
insertVertex v = snd . insertVertex' v

-- | Insert an edge
--
-- This function inserts vertices automatically.
--
-- The graph is not changed if the edge already exists.
insertEdge :: forall a. Ord a => a -> a -> DynGraph a -> DynGraph a
insertEdge vFrom vTo dynGraph0 =
    let (vFromIdx, dynGraph1) = insertVertex' vFrom dynGraph0
        (vToIdx,   dynGraph2) = insertVertex' vTo   dynGraph1
    in  dynGraph2 {
            edges =
              IntMap.insertWith (<>) vFromIdx (IntSet.singleton vToIdx) $
                edges dynGraph2
          }

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Gets the vertices in the graph
vertices :: DynGraph a -> [a]
vertices DynGraph{..} = Map.keys vtxMap

-- | Gets the set of vertices that are reachable from the specified vertex
--
-- The specified vertex is included in the set.
reaches :: Ord a => DynGraph a -> a -> Set a
reaches DynGraph{..} v = case Map.lookup v vtxMap of
    Just idx -> Set.fromList $ (idxMap Map.!) <$> reaches' edges idx
    Nothing  -> mempty

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

-- | Render a Mermaid diagram
dumpMermaid ::
     (a -> String) -- ^ Function to render a vertex
  -> Bool          -- ^ 'False' if forward, 'True' if reverse
  -> DynGraph a
  -> String
dumpMermaid renderVertex isReverse DynGraph{..} =
    unlines $ header : nodes ++ links
  where
    header :: String
    header = "graph TD;"

    nodes, links :: [String]
    nodes = [
        -- TODO escape quotes?
        "  v" ++ show idx ++ "[\"" ++ renderVertex v ++ "\"]"
      | (v, idx) <- Map.toAscList vtxMap
      ]
    links
      | isReverse = [
            "  v" ++ show idxR ++ "-->v" ++ show idxL
          | (idxL, rSet) <- IntMap.toAscList edges
          , idxR <- IntSet.toAscList rSet
          ]
      | otherwise = [
            "  v" ++ show idxL ++ "-->v" ++ show idxR
          | (idxL, rSet) <- IntMap.toAscList edges
          , idxR <- IntSet.toAscList rSet
          ]

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

-- | Insert a vertex
--
-- The graph is not changed if the vertex already exists.
insertVertex' :: forall a. Ord a => a -> DynGraph a -> (Int, DynGraph a)
insertVertex' v dynGraph@DynGraph{..} =
    case Map.insertLookupWithKey (\_key _new old -> old) v i' vtxMap of
      (Just i, _)        -> (i,  dynGraph)
      (Nothing, vtxMap') -> (i', dynGraph{ vtxMap = vtxMap', idxMap = idxMap' })
  where
    i' :: Int
    i' = Map.size vtxMap

    idxMap' :: Map Int a
    idxMap' = Map.insert i' v idxMap

-- | Get a list of vertex indexes reachable from the specified vertex index in
-- the specified edge map
--
-- The specified vertex index is included in the list.
--
-- Note that 'Map' is used so that lookup and insertion can be performed at the
-- same time.
reaches' :: IntMap IntSet -> Int -> [Int]
reaches' edgeMap = aux Map.empty . pure
  where
    aux :: Map Int () -> [Int] -> [Int]
    aux acc [] = Map.keys acc
    aux acc (x:xs) =
      case Map.insertLookupWithKey (\_key _new old -> old) x () acc of
        (Just (), _)    -> aux acc xs
        (Nothing, acc') -> aux acc' $
          IntSet.toList (IntMap.findWithDefault mempty x edgeMap) ++ xs
