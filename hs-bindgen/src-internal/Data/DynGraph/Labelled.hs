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
  , neighbors
  , topSort
  , dff
  , dfFindMember
  , findTrailFrom
  , findEdges
  , FindEdgesResult(..)
    -- * Deletion
  , deleteEdgesTo
  , filterEdges
    -- * Debugging
  , MermaidOptions(..)
  , dumpMermaid
    -- * Auxiliary: tree traversals
  , postorderForest
  , postorderTree
  ) where

import Prelude hiding (reverse)

import Control.Monad (unless, (<=<))
import Control.Monad.ST (ST)
import Control.Monad.ST qualified as ST
import Data.Array.ST.Safe qualified as Array
import Data.Bifunctor
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
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
reverse graph = DynGraph{
      vtxMap = graph.vtxMap
    , idxMap = graph.idxMap
    , edges  = go IntMap.empty (IntMap.toList graph.edges)
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
insertEdge vFrom l vTo graph0 =
    let (vFromIdx, graph1) = insertVertex' vFrom graph0
        (vToIdx,   graph2) = insertVertex' vTo   graph1
    in  graph2 {
            edges =
              IntMap.insertWith (<>) vFromIdx (Set.singleton (vToIdx, l)) $
                graph2.edges
          }

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Gets the vertices in the graph
vertices :: DynGraph l a -> [a]
vertices graph = Map.keys graph.vtxMap

-- | Gets the set of vertices that are reachable from any of the specified
-- vertices
--
-- The specified vertices are included in the set (assuming that they are in
-- the graph).
reaches :: Ord a => DynGraph l a -> [a] -> Set a
reaches graph =
      Set.map (graph.idxMap IntMap.!)
    . reaches' graph.edges
    . mapMaybe (graph.vtxMap Map.!?)

-- | Gets the set of vertices that are immediate neighbors of the specified
-- vertex
neighbors :: (Ord a, Ord l) => DynGraph l a -> a -> Set (a, l)
neighbors graph =
      maybe mempty (Set.map (first (graph.idxMap IntMap.!)))
    . ((`IntMap.lookup` graph.edges) <=< (`Map.lookup` graph.vtxMap))

-- | Gets a topological sort of the graph
topSort :: DynGraph l a -> [a]
topSort graph = (graph.idxMap IntMap.!) <$> topSort' graph

-- | Gets the spanning forest of the graph obtained from a depth-first search of
-- the graph starting from each vertex in insertion order
dff :: DynGraph l a -> [Tree a]
dff graph = fmap (graph.idxMap IntMap.!) <$> dff' graph

-- | Find the first vertex in the specified set in a depth-first traversal of
-- the graph starting from the specified vertex
--
-- This function is specific to equality so that more can be done in the index
-- domain, for performance.
dfFindMember :: Ord a => Set a -> DynGraph l a -> a -> Maybe a
dfFindMember targets graph v = do
    ix <- Map.lookup v graph.vtxMap
    let targetIxs = IntSet.fromList $
          mapMaybe (`Map.lookup` graph.vtxMap) (Set.toList targets)
    targetIx <- List.find (`IntSet.member` targetIxs) $
      List.reverse (postorderForest (dfs' graph [ix]))
    IntMap.lookup targetIx graph.idxMap

-- | Find trail through the graph
findTrailFrom :: forall m l a r.
     (Monad m, Ord a)
  => DynGraph l a
  -> ([(a, l)] -> m (Either a r)) -- ^ Choose next step
  -> a                            -- ^ Starting point
  -> m r
findTrailFrom graph f = go
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
            currIx <- Map.lookup curr graph.vtxMap
            next   <- Set.toList <$> IntMap.lookup currIx graph.edges
            return $ map (first (graph.idxMap IntMap.!)) next

-- | Find edges from the specified starting vertex and edges to terminal
-- vertices in paths from that starting vertex
findEdges :: forall l a.
     Ord a
  => DynGraph l a
  -> a
  -> FindEdgesResult l
findEdges graph startV =
    run (0, Map.size graph.vtxMap - 1) $ \(check :: Int -> ST s Bool) ->
      let step ::
               NonEmpty l  -- edges from starting vertex
            -> IntSet      -- terminal vertices
            -> [l]         -- accumulated list of edges to terminal vertices
            -> [(Int, l)]  -- frontier
            -> ST s (FindEdgesResult l)
          step startEdges termIxs acc ((ix, l) : rest)
            | IntSet.member ix termIxs = step startEdges termIxs (l : acc) rest
            | otherwise = check ix >>= \case
                True -> step startEdges termIxs acc rest
                False -> case Set.toList <$> IntMap.lookup ix graph.edges of
                  Just ps | not (null ps) ->
                    step startEdges termIxs acc (ps ++ rest)
                  _otherwise ->
                    step startEdges (IntSet.insert ix termIxs) (l : acc) rest
          step startEdges _termIxs acc [] = return $
            FindEdgesFound startEdges (NonEmpty.fromList (List.reverse acc))
      in  case Map.lookup startV graph.vtxMap of
            Nothing -> return FindEdgesInvalid
            Just ix -> case Set.toList <$> IntMap.lookup ix graph.edges of
              Just ps -> case fmap snd <$> NonEmpty.nonEmpty ps of
                Just startEdges -> step startEdges IntSet.empty [] ps
                Nothing -> return FindEdgesNone
              Nothing -> return FindEdgesNone
  where
    run :: forall x.
         (Int, Int)
      -> (forall s. (Int -> ST s Bool) -> ST s x)
      -> x
    run bnds f = ST.runST $ do
      m <- Array.newArray bnds False :: ST s (Array.STUArray s Int Bool)
      f $ \idx -> do
            visited <- Array.readArray m idx
            unless visited $ Array.writeArray m idx True
            return visited

-- | 'findEdges' result
data FindEdgesResult l =
    -- | Invalid graph: one or more vertex not found
    FindEdgesInvalid

    -- | Starting vertex has no edges
  | FindEdgesNone

    -- | Edges from starting vertex and edges to terminal vertices
  | FindEdgesFound (NonEmpty l) (NonEmpty l)

{-------------------------------------------------------------------------------
  Deletion
-------------------------------------------------------------------------------}

-- | Delete edges to any of the specified vertices
--
-- This function never deletes vertices, even if removing edges results in a
-- disconnected graph.
deleteEdgesTo :: Ord a => [a] -> DynGraph l a -> DynGraph l a
deleteEdgesTo vs graph =
    let ixs = IntSet.fromList $ mapMaybe (graph.vtxMap Map.!?) vs
        f   = mne . Set.filter ((`IntSet.notMember` ixs) . fst)
    in graph { edges = IntMap.mapMaybe f graph.edges }
  where
    mne :: Set a -> Maybe (Set a)
    mne s
      | Set.null s = Nothing
      | otherwise  = Just s

filterEdges :: forall l a. (l -> Bool) -> DynGraph l a -> DynGraph l a
filterEdges f graph = graph {
      edges = IntMap.mapMaybe aux graph.edges
    }
  where
    aux :: Set (Int, l) -> Maybe (Set (Int, l))
    aux = dropIfEmpty . Set.filter (f . snd)

    dropIfEmpty :: Set (Int, l) -> Maybe (Set (Int, l))
    dropIfEmpty x = if Set.null x then Nothing else Just x

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

-- | Insert a vertex
--
-- The graph is not changed if the vertex already exists.
insertVertex' :: forall l a. Ord a => a -> DynGraph l a -> (Int, DynGraph l a)
insertVertex' v graph =
    case Map.insertLookupWithKey (\_key _new old -> old) v i' graph.vtxMap of
      (Just i, _)        -> (i,  graph)
      (Nothing, vtxMap') -> (i', graph{ vtxMap = vtxMap', idxMap = idxMap' })
  where
    i' :: Int
    i' = Map.size graph.vtxMap

    idxMap' :: IntMap a
    idxMap' = IntMap.insert i' v graph.idxMap

-- | Get the set of vertices that are reachable from any of the specified
-- vertices
--
-- The specified vertices are included in the set (assuming that they are in
-- the graph).
--
-- Note that 'Map' is used so that lookup and insertion can be performed at the
-- same time.
reaches' :: IntMap (Set (Int, l)) -> [Int] -> Set Int
reaches' edgeMap = aux Map.empty
  where
    aux :: Map Int () -> [Int] -> Set Int
    aux acc []         = Map.keysSet acc
    aux acc (idx:idxs) =
      case Map.insertLookupWithKey (\_key _new old -> old) idx () acc of
        (Just (), _)    -> aux acc idxs
        (Nothing, acc') -> aux acc' $
          maybe [] (map fst . Set.toList) (IntMap.lookup idx edgeMap) ++ idxs

-- | Gets a topological sort of the graph
topSort' :: DynGraph l a -> [Int]
topSort' = List.reverse . postorderForest . dff'

-- | Gets the spanning forest of the graph obtained from a depth-first search of
-- the graph starting from each vertex index in insertion order
dff' :: DynGraph l a -> [Tree Int]
dff' graph = dfs' graph (IntMap.keys graph.idxMap)

-- | Gets a spanning forest of the part of the graph reachable from the listed
-- vertex indexes, obtained from a depth-first search of the graph starting at
-- each of the listed vertex indexes in order
dfs' :: forall l a. DynGraph l a -> [Int] -> [Tree Int]
dfs' graph idxs0 = case Map.size graph.vtxMap of
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
                  aux . map fst $
                    maybe [] Set.toList (IntMap.lookup idx graph.edges)
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

data MermaidOptions l a = MermaidOptions{
      reverseEdges :: Bool
    , renderVertex :: a -> Maybe String
    , renderEdge   :: l -> Maybe String
    }

-- | Render a Mermaid diagram
--
-- See https://mermaid.js.org/>
dumpMermaid :: MermaidOptions l a -> DynGraph l a -> String
dumpMermaid opts graph =
    unlines $ header : nodes ++ links
  where
    header :: String
    header = "graph TD;"

    pSet :: IntMap String
    pSet = IntMap.fromAscList $
        mapMaybe
          (\(idx, v) -> (idx,) <$> opts.renderVertex v)
          (IntMap.toAscList graph.idxMap)

    nodes, links :: [String]
    nodes = [
        "  v" ++ show idx ++ "[\"" ++ escapeString rendered ++ "\"]"
      | (_v, idx) <- Map.toAscList graph.vtxMap
      , Just rendered <- [IntMap.lookup idx pSet]
      ]
    links = [
         concat [
             "  v"
           , show (if opts.reverseEdges then to else fr)
           , "-->"
           , maybe "" (\e -> "|\"" ++ escapeString e ++ "\"|") (opts.renderEdge l)
           , "v"
           , show (if opts.reverseEdges then fr else to)
           ]
       | (fr, rSet) <- IntMap.toAscList graph.edges
       , fr `IntMap.member` pSet
       , (to, l) <- Set.toAscList rSet
       , to `IntMap.member` pSet
       ]

    escapeString :: String -> [Char]
    escapeString = concatMap $ \case
        '"' -> "&quot;"
        '<' -> "&lt;"
        '>' -> "&gt;"
        c   -> [c]

{-------------------------------------------------------------------------------
  Auxiliary: tree traversals
-------------------------------------------------------------------------------}

postorderTree :: Tree a -> [a]
postorderTree tree =
       postorderForest tree.subForest
    ++ [tree.rootLabel]

postorderForest :: [Tree a] -> [a]
postorderForest = concatMap postorderTree
