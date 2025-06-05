module Data.DynGraph.Labelled (
    -- * Type
    DynGraph
    -- * Construction
  , empty
  , reverse
    -- * Insertion
  , insertVertex
  , insertEdge
    -- * Query
  , vertices
  , reaches
  , topSort
  , dff
  , findTrailFrom
    -- * Debugging
  , dumpMermaid
    -- * Auxiliary: tree traversals
  , postorderForest
  , postorderTree
  ) where

import Prelude hiding (reverse)

import Control.Monad.ST (ST)
import Control.Monad.ST qualified as ST
import Data.Array.ST.Safe qualified as Array
import Data.Bifunctor
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree (Tree)
import Data.Tree qualified as Tree

{-------------------------------------------------------------------------------
  Type

  TODO: We should make sure that DynGraph is (spine) strict
-------------------------------------------------------------------------------}

-- | Directed graph that supports dynamic insertion
--
-- Type variable @a@ represents the type of a vertex in the graph.  Internally,
-- each value is mapped to an 'Int' index that is used in the representation of
-- the graph.
--
-- NOTE: We use @(Int, l)@ rather than @(l, Int)@ for the 'edges', so that
-- we preserve vertex-based ordering as much as possible.
data DynGraph l a = DynGraph {
      vtxMap :: Map a Int
    , idxMap :: IntMap a
    , edges  :: IntMap (Set (Int, l))
    }

deriving instance (Show a, Show l) => Show (DynGraph l a)
deriving instance (Eq   a, Eq   l) => Eq   (DynGraph l a)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | The empty graph
empty :: DynGraph l a
empty = DynGraph
    { vtxMap = Map.empty
    , idxMap = IntMap.empty
    , edges  = IntMap.empty
    }

reverse :: forall l a. Ord l => DynGraph l a -> DynGraph l a
reverse DynGraph{vtxMap, idxMap, edges} = DynGraph{
      vtxMap
    , idxMap
    , edges = go IntMap.empty (IntMap.toList edges)
    }
  where
    go :: IntMap (Set (Int, l)) -> [(Int, Set (Int, l))] -> IntMap (Set (Int, l))
    go !acc []            = acc
    go !acc ((fr, to):es) = go (foldr (addEdge fr) acc to) es

    addEdge :: Int -> (Int, l) -> IntMap (Set (Int, l)) -> IntMap (Set (Int, l))
    addEdge fr (to, l) = IntMap.insertWith (<>) to $ Set.singleton (fr, l)

{-------------------------------------------------------------------------------
  Insertion
-------------------------------------------------------------------------------}

-- | Insert a vertex
--
-- The graph is not changed if the vertex already exists.
insertVertex :: Ord a => a -> DynGraph l a -> DynGraph l a
insertVertex v = snd . insertVertex' v

-- | Insert an edge
--
-- This function inserts vertices automatically.
--
-- The graph is not changed if the edge already exists.
insertEdge :: (Ord a, Ord l) => a -> l -> a -> DynGraph l a -> DynGraph l a
insertEdge vFrom l vTo dynGraph0 =
    let (vFromIdx, dynGraph1) = insertVertex' vFrom dynGraph0
        (vToIdx,   dynGraph2) = insertVertex' vTo   dynGraph1
    in  dynGraph2 {
            edges =
              IntMap.insertWith (<>) vFromIdx (Set.singleton (vToIdx, l)) $
                edges dynGraph2
          }

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Gets the vertices in the graph
vertices :: DynGraph l a -> [a]
vertices DynGraph{..} = Map.keys vtxMap

-- | Gets the set of vertices that are reachable from the specified vertex
--
-- The specified vertex is included in the set.
--
-- NOTE: We /could/ generalize this to
--
-- > reaches :: (Ord a, Ord l) => DynGraph l a -> a -> ([l], a)
--
-- where we additionally record paths also; we don't need this currently, so
-- not yet done.
reaches :: (Ord a, Ord l) => DynGraph l a -> a -> Set a
reaches DynGraph{..} v = case Map.lookup v vtxMap of
    Just idx -> Set.fromList $ (idxMap IntMap.!) <$> reaches' edges idx
    Nothing  -> mempty

-- | Gets a topological sort of the graph
topSort :: DynGraph l a -> [a]
topSort dynGraph@DynGraph{..} = (idxMap IntMap.!) <$> topSort' dynGraph

-- | Gets the spanning forest of the graph obtained from a depth-first search of
-- the graph starting from each vertex in insertion order
dff :: DynGraph l a -> [Tree a]
dff dynGraph@DynGraph{..} = fmap (idxMap IntMap.!) <$> dff' dynGraph

-- | Find trail through the graph
findTrailFrom :: forall m l a r.
     (Monad m, Ord a)
  => DynGraph l a
  -> ([(a, l)] -> m (Either a r)) -- ^ Choose next step
  -> a                            -- ^ Starting point
  -> m r
findTrailFrom DynGraph{..} f = go
  where
    go :: a -> m r
    go curr = do
        mResult <- f successors
        case mResult of
          Left  next -> go next
          Right res  -> return res
      where
        successors :: [(a, l)]
        successors = fromMaybe [] $ do
            currIx <- Map.lookup curr vtxMap
            next   <- Set.toList <$> IntMap.lookup currIx edges
            return $ map (first (idxMap IntMap.!)) next

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

-- | Insert a vertex
--
-- The graph is not changed if the vertex already exists.
insertVertex' :: forall l a. Ord a => a -> DynGraph l a -> (Int, DynGraph l a)
insertVertex' v dynGraph@DynGraph{..} =
    case Map.insertLookupWithKey (\_key _new old -> old) v i' vtxMap of
      (Just i, _)        -> (i,  dynGraph)
      (Nothing, vtxMap') -> (i', dynGraph{ vtxMap = vtxMap', idxMap = idxMap' })
  where
    i' :: Int
    i' = Map.size vtxMap

    idxMap' :: IntMap a
    idxMap' = IntMap.insert i' v idxMap

-- | Get a list of vertex indexes reachable from the specified vertex index in
-- the specified edge map
--
-- The specified vertex index is included in the list.
--
-- Note that 'Map' is used so that lookup and insertion can be performed at the
-- same time.
reaches' :: Ord l => IntMap (Set (Int, l)) -> Int -> [Int]
reaches' edgeMap = aux Map.empty . pure
  where
    aux :: Map Int () -> [Int] -> [Int]
    aux acc [] = Map.keys acc
    aux acc (x:xs) =
      case Map.insertLookupWithKey (\_key _new old -> old) x () acc of
        (Just (), _)    -> aux acc xs
        (Nothing, acc') -> aux acc' $
          map fst (Set.toList (IntMap.findWithDefault mempty x edgeMap)) ++ xs

-- | Gets a topological sort of the graph
topSort' :: DynGraph l a -> [Int]
topSort' = List.reverse . postorderForest . dff'

-- | Gets the spanning forest of the graph obtained from a depth-first search of
-- the graph starting from each vertex index in insertion order
dff' :: DynGraph l a -> [Tree Int]
dff' dynGraph@DynGraph{..} = dfs' dynGraph (IntMap.keys idxMap)

-- | Gets a spanning forest of the part of the graph reachable from the listed
-- vertext indexes, obtained from a depth-first search of the graph starting at
-- each of the listed vertex indexes in order
dfs' :: forall l a. DynGraph l a -> [Int] -> [Tree Int]
dfs' DynGraph{..} idxs0 = case Map.size vtxMap of
    0 -> []
    n -> run (0, n - 1) $ \(contains  :: Int -> ST s Bool)
                           (include   :: Int -> ST s ()) ->
      let aux :: [Int] -> ST s [Tree Int]
          aux [] = pure []
          aux (idx:idxs) = do
            visited <- contains idx
            if visited
              then aux idxs
              else do
                include idx
                children <-
                  aux . map fst $ maybe [] Set.toList (IntMap.lookup idx edges)
                trees <- aux idxs
                return $ Tree.Node idx children : trees
      in aux idxs0
  where
    run :: forall x.
         (Int, Int)
      -> (forall s. (Int -> ST s Bool) -> (Int -> ST s ()) -> ST s x)
      -> x
    run bnds f = ST.runST $ do
      m <- Array.newArray bnds False :: ST s (Array.STUArray s Int Bool)
      f (Array.readArray m) (\idx -> Array.writeArray m idx True)

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

-- | Render a Mermaid diagram
--
-- See https://mermaid.js.org/>
dumpMermaid ::
     (l -> Maybe String) -- ^ Function to optionally render an edge
  -> (a -> String)       -- ^ Function to render a vertex
  -> DynGraph l a
  -> String
dumpMermaid renderEdge renderVertex DynGraph{..} =
    unlines $ header : nodes ++ links
  where
    header :: String
    header = "graph TD;"

    nodes, links :: [String]
    nodes = [
        -- TODO escape quotes?
        "  v" ++ show idx ++ "[\"" ++ escapeString (renderVertex v) ++ "\"]"
      | (v, idx) <- Map.toAscList vtxMap
      ]
    links = [
         concat [
             "  v"
           , show fr
           , "-->"
           , maybe "" (\e -> "|\"" ++ escapeString e ++ "\"|") (renderEdge l)
           , "v"
           , show to
           ]
       | (fr, rSet) <- IntMap.toAscList edges
       , (to, l) <- Set.toAscList rSet
       ]

    escapeString :: String -> [Char]
    escapeString = concatMap $ \case
        '"' -> "#quot;"
        c   -> [c]

{-------------------------------------------------------------------------------
  Auxiliary: tree traversals
-------------------------------------------------------------------------------}

postorderTree :: Tree a -> [a]
postorderTree Tree.Node{rootLabel, subForest} =
       postorderForest subForest
    ++ [rootLabel]

postorderForest :: [Tree a] -> [a]
postorderForest = concatMap postorderTree
