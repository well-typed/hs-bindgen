-- | Directed graph
--
-- Intended for qualified import.
--
-- > import Data.Digraph (Digraph)
-- > import Data.Digraph qualified as Digraph
module Data.Digraph (
    -- * Type
    Digraph -- opaque
    -- * Construction
  , empty
  , reIndex
  , transpose
    -- * Insertion
  , insertVertex
  , insertEdge
  , insertEdgeIfVerticesExist
  , InsertEdgeIfVerticesExistResult(..)
    -- * Deletion/Update
  , deleteEdgesFrom
  , deleteEdgesTo
  , combineParallelEdges
  , filterEdges
  , filterVerticesCombineEdges
    -- * Query
  , hasVertex
  , vertices
  , neighbors
  , reaches
  , sort
  , sortBy
  , dfs
  , dff
  , dfFindMember
  , traversePathFrom
  , findEdges
  , FindEdgesResult(..)
    -- * Traversal
  , mapEdges
  , mapVerticesOutgoingEdges
    -- * Visualization
  , VisOptions(..)
  , VisVertex(..)
  , VisEdge(..)
  , VisEdgeStyle(..)
  , testVisOptions
  , renderMermaid
  ) where

import Control.Monad (unless, (<=<))
import Control.Monad.ST (ST)
import Control.Monad.ST qualified as ST
import Data.Array.ST.Safe qualified as Array
import Data.Bifunctor (bimap, first)
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

{-------------------------------------------------------------------------------
  Type
-------------------------------------------------------------------------------}

-- | Internal vertex index
type Idx = Int

-- | Directed graph with vertices of type @v@ and edges of type @e@
--
-- Functions in this module implement objective ordering of vertices, using
-- vertex insertion order by default.  Use the 'reIndex' function to transform
-- the internal representation of a graph so that vertices are ordered by an
-- explicit ordering function.
data Digraph e v = Digraph {
      -- | Next internal vertex index to allocate
      nextIdx :: !Idx

      -- | Map from vertex to internal vertex index
      --
      -- Invariant: All entries have corresponding entries in @idxMap@.  For any
      -- graph @g@:
      --
      -- @
      -- and [
      --   IntMap.lookup idx g.idxMap == Just v
      --   | (v, idx) <- Map.toList g.vMap
      --   ]
      -- @
    , vMap :: !(Map v Idx)

      -- | Map from internal vertex index to vertex
      --
      -- Invariant: All entries have corresponding entries in @vMap@.  For any
      -- graph @g@:
      --
      -- @
      -- and [
      --   Map.lookup v g.vMap == Just idx
      --   | (idx, v) <- IntMap.toList g.idxMap
      --   ]
      -- @
    , idxMap :: !(IntMap v)

      -- | Map of directed edges between vertices
      --
      -- The outside 'IntMap' maps from source vertices to an 'IntMap' from
      -- target vertices to a 'Set' of edges.
      --
      -- Invariant: Every entry has at least one edge, so no inside 'IntMap' or
      -- 'Set' may be empty.  For any graph @g@:
      --
      -- @
      -- not $ or [
      --     any IntMap.null graph.edgeMap
      --   , any (any Set.null) graph.edgeMap
      --   ]
      -- @
    , edgeMap :: !(IntMap (IntMap (Set e)))
    }
  deriving stock (Generic)

deriving instance (Eq   e, Eq   v) => Eq   (Digraph e v)
deriving instance (Show e, Show v) => Show (Digraph e v)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | The empty graph
empty :: Digraph e v
empty = Digraph{
      nextIdx = 0
    , vMap    = Map.empty
    , idxMap  = IntMap.empty
    , edgeMap = IntMap.empty
    }

-- | Re-index the graph using the specified ordering function
--
-- The new graph has the same vertices and edges.  Only the internal ordering is
-- updated.
reIndex :: forall e v.
     Ord v
  => (v -> v -> Ordering)
  -> Digraph e v
  -> Digraph e v
reIndex cmp graph = Digraph{
      nextIdx = Map.size vMap'
    , vMap    = vMap'
    , idxMap  = idxMap'
    , edgeMap = edgeMap'
    }
  where
    vIdxs :: [(Idx, (v, Idx))]
    vIdxs = zip [0..] $
      List.sortBy (\l r -> cmp (fst l) (fst r)) (Map.toList graph.vMap)

    reIdxMap :: IntMap Idx
    reIdxMap = IntMap.fromList [
        (oldIdx, newIdx)
      | (newIdx, (_v, oldIdx)) <- vIdxs
      ]

    vMap' :: Map v Idx
    vMap'   = Map.fromList    [(v, newIdx) | (newIdx, (v, _oldIdx)) <- vIdxs]

    idxMap' :: IntMap v
    idxMap' = IntMap.fromList [(newIdx, v) | (newIdx, (v, _oldIdx)) <- vIdxs]

    edgeMap' :: IntMap (IntMap (Set e))
    edgeMap' = IntMap.fromList [
        let toMap'   = IntMap.fromList [
                (reIdxMap IntMap.! toIdx, edges)
              | (toIdx, edges) <- IntMap.toList toMap
              ]
        in  (reIdxMap IntMap.! fromIdx, toMap')
      | (fromIdx, toMap) <- IntMap.toList graph.edgeMap
      ]

-- | Transpose an existing graph
--
-- The new graph has the same vertices, and edges are reversed.
--
-- Property: The internal representation is maintained.  For any graph @g@:
--
-- @
-- Digraph.transpose (Digraph.transpose g) == g
-- @
transpose :: Digraph e v -> Digraph e v
transpose graph = graph{ edgeMap = aux graph.edgeMap }
  where
    aux :: IntMap (IntMap es) -> IntMap (IntMap es)
    aux = IntMap.foldrWithKey' auxF IntMap.empty

    auxF :: Idx -> IntMap es -> IntMap (IntMap es) -> IntMap (IntMap es)
    auxF fromIdx = flip (IntMap.foldrWithKey' (auxT fromIdx))

    auxT :: Idx -> Idx -> es -> IntMap (IntMap es) -> IntMap (IntMap es)
    auxT fromIdx toIdx edges = flip IntMap.alter toIdx $
        Just
      . maybe (IntMap.singleton fromIdx edges) (IntMap.insert fromIdx edges)

{-------------------------------------------------------------------------------
  Insertion
-------------------------------------------------------------------------------}

-- | Insert a vertex
--
-- Property: This function is idempotent.  The graph is not changed if the
-- vertex already exists.  For any graph @g@:
--
-- @
-- let g' = Dyngraph.insertVertex v g
-- in  Dyngraph.insertVertex v g' == g'
-- @
insertVertex :: Ord v => v -> Digraph e v -> Digraph e v
insertVertex v = snd . insertVertex' v

-- | Insert an edge
--
-- The vertices do not have to exist, as this function inserts vertices
-- automatically.
--
-- Property: This function is idempotent.  The graph is not changed if the edge
-- already exists.  For any graph @g@:
--
-- @
-- let g' = Dyngraph.insertEdge v1 e v2 g
-- in  Dyngraph.insertEdge v1 e v2 g' == g'
-- @
insertEdge :: (Ord e, Ord v) => v -> e -> v -> Digraph e v -> Digraph e v
insertEdge fromV edge toV graph =
    let (fromIdx, graph1) = insertVertex' fromV graph
        (toIdx,   graph2) = insertVertex' toV   graph1
    in  graph2{ edgeMap = insertEdge' fromIdx edge toIdx graph2.edgeMap }

-- | Insert an edge when both vertices are already in the graph
--
-- Property: This function is idempotent.  The graph is not changed if the edge
-- already exists.  For any graph @g@ that has edges @v1@ and @v2@:
--
-- @
-- let result@(InsertEdgeSuccess g') =
--       Dyngraph.insertEdgeIfVerticesExist v1 e v2 g
-- in  Dyngraph.insertEdgeIfVerticesExist v1 e v2 g' == result
-- @
insertEdgeIfVerticesExist ::
     (Ord e, Ord v)
  => v
  -> e
  -> v
  -> Digraph e v
  -> InsertEdgeIfVerticesExistResult e v
insertEdgeIfVerticesExist fromV edge toV graph =
    either id InsertEdgeSuccess $ do
      fromIdx <- maybe (Left (InsertEdgeSourceVertexNotFound fromV)) Right $
        Map.lookup fromV graph.vMap
      toIdx   <- maybe (Left (InsertEdgeTargetVertexNotFound toV))   Right $
        Map.lookup toV   graph.vMap
      return $ graph{ edgeMap = insertEdge' fromIdx edge toIdx graph.edgeMap }

-- | 'insertEdgeIfVerticesExist' result
data InsertEdgeIfVerticesExistResult e v =
    InsertEdgeSuccess (Digraph e v)
  | InsertEdgeSourceVertexNotFound v
  | InsertEdgeTargetVertexNotFound v
  deriving stock (Eq, Show)

{-------------------------------------------------------------------------------
  Deletion/Update
-------------------------------------------------------------------------------}

-- | Delete edges from any of the specified vertices
--
-- This function never deletes vertices, even if removing edges results in a
-- disconnected graph.
deleteEdgesFrom :: Ord v => Set v -> Digraph e v -> Digraph e v
deleteEdgesFrom fromVs graph =
    let fromIdxs = Maybe.mapMaybe (graph.vMap Map.!?) (Set.elems fromVs)
    in  graph{
            edgeMap =
              Foldable.foldl' (flip IntMap.delete) graph.edgeMap fromIdxs
          }

-- | Delete edges to any of the specified vertices
--
-- This function never deletes vertices, even if removing edges results in a
-- disconnected graph.
deleteEdgesTo :: Ord v => Set v -> Digraph e v -> Digraph e v
deleteEdgesTo toVs graph =
    let toIdxs = Maybe.mapMaybe (graph.vMap Map.!?) (Set.elems toVs)
    in  graph{ edgeMap = aux toIdxs graph.edgeMap }
  where
    aux :: [Idx] -> IntMap (IntMap (Set e)) -> IntMap (IntMap (Set e))
    aux idxs = IntMap.mapMaybe $
        maybeEmpty IntMap.null
      . IntMap.filterWithKey (\idx _ -> idx `notElem` idxs)

-- | Combine parallel edges using the provided function
--
-- The 'Ord' instance determines the order that edges are passed to the provided
-- function.
--
-- Example graph @g@, with two edges @s@ and @t@, both from vertex @A@ to vertex
-- @B@:
--
-- @
--   +-----s-----+
--   |           |
--   |           v
--   A           B
--   |           ^
--   |           |
--   +-----t-----+
-- @
--
-- Assuming @s < t@, @combineParallelEdges f g@ replaces edges @s@ and @t@ with a
-- single edge @f s t@:
--
-- @
--   A-----(f s t)---->B
-- @
combineParallelEdges :: forall e v. (e -> e -> e) -> Digraph e v -> Digraph e v
combineParallelEdges combine graph =
    graph{ edgeMap = IntMap.map (IntMap.map aux) graph.edgeMap }
  where
    aux :: Set e -> Set e
    aux = Set.singleton . Foldable.foldl1 combine

-- | Filter edges of a graph
--
-- Only edges for which the predicate returns 'True' are kept.
filterEdges :: forall e v. (e -> Bool) -> Digraph e v -> Digraph e v
filterEdges p graph = graph { edgeMap = IntMap.mapMaybe aux graph.edgeMap }
  where
    aux :: IntMap (Set e) -> Maybe (IntMap (Set e))
    aux =
        maybeEmpty IntMap.null
      . IntMap.mapMaybe (maybeEmpty Set.null . Set.filter p)

-- | Filter vertices of a graph, combining edges that traverse removed vertices
--
-- __WARNING__ Vertices for which the predicate returns 'True' are removed,
-- unlike normal Haskell filters.
--
-- The 'Ord' instance determines the order that edges are passed to the provided
-- function.
--
-- Example graph @g@, with four vertices (uppercase) and four edges (lowercase):
--
-- @
--   +------ac-------+
--   |               |
--   |               v
--   A--ab-->B--bc-->C
--           |
--           +--bd-->D
-- @
--
-- When using this function to remove vertex @B@ with
-- @filterVerticesCombineEdges (== 'B') f g@:
--
-- * Vertex @B@ and edges @ab@, @bc@, @bd@ are removed.
-- * Assuming @ab < bc@, a new edge @f ab bc@ from vertex @A@ to vertex @C@ is
--   created /if/ @f ab bc /= ac@.
-- * Assuming @ab < bd@, a new edge @f ab bd@ from vertex @A@ to vertex @D@ is
--   created.
--
-- This results in the following graph when @f ab bc /= ac@:
--
-- @
--   +------ac-------+
--   |               |
--   |               v
--   A---(f ab bc)-->C
--   |
--   +---(f ab bd)-->D
-- @
--
-- Alternatively, this results in the following graph when @f ab bc == ac@:
--
-- @
--   A------ac------>C
--   |
--   +---(f ab bd)-->D
-- @
filterVerticesCombineEdges ::
     (Ord e, Ord v)
  => (v -> Bool)
  -> (e -> e -> e)
  -> Digraph e v
  -> Digraph e v
filterVerticesCombineEdges p combine graph =
    let delIdxVs = IntMap.toList $ IntMap.filter (not . p) graph.idxMap
    in  Foldable.foldl' (deleteVertexCombineEdges combine) graph delIdxVs

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Check whether a vertex exists in the graph
hasVertex :: Ord v => v -> Digraph e v -> Bool
hasVertex v graph = Map.member v graph.vMap

-- | Get the vertices of the graph
vertices :: Digraph e v -> [v]
vertices graph = IntMap.elems graph.idxMap

-- | Get the immediate neighbors of the specified vertex
neighbors :: Ord v => v -> Digraph e v -> Map v (Set e)
neighbors fromV graph = maybe Map.empty Map.fromList $ do
    fromIdx <- Map.lookup fromV graph.vMap
    toMap   <- IntMap.lookup fromIdx graph.edgeMap
    return $ map (first (graph.idxMap IntMap.!)) (IntMap.toList toMap)

-- | Get the set of vertices that are reachable from any of the specified
-- vertices
--
-- The specified vertices are included in the set (assuming that they are in
-- the graph).
reaches :: Ord v => Set v -> Digraph e v -> Set v
reaches fromVs graph =
      Set.map (graph.idxMap IntMap.!)
    . reaches' graph.edgeMap
    . Maybe.mapMaybe (graph.vMap Map.!?)
    $ Set.toList fromVs

-- | Sort the vertices of a graph using a topological sort of groups of strongly
-- connected components
sort :: forall e v. Digraph e v -> [v]
sort graph = vs
  where
    -- Get the strongly connected components for the graph.
    sccs :: [Tree Idx]
    sccs = scc' graph

    -- Associate each group of strongly connected components with the least
    -- vertex in that group.  @sccToMap@ is a map from each index in the graph
    -- to the representative index for the group of strongly connected
    -- components that it is in.  @sccFromMap@ is a map from each representative
    -- index to an /ordered/ list of strongly connected components in that
    -- group.
    sccToMap   :: IntMap Idx
    sccFromMap :: IntMap [Idx]
    (sccToMap, sccFromMap) = foldr aux (IntMap.empty, IntMap.empty) sccs
      where
        aux ::
             Tree Idx
          -> (IntMap Idx, IntMap [Idx])
          -> (IntMap Idx, IntMap [Idx])
        aux tree (toMap, fromMap) =
          let idxs = List.sort (Tree.flatten tree)
              idx' = unsafeHead idxs  -- safe because Tree is non-empty
          in  ( foldr (flip IntMap.insert idx') toMap idxs
              , IntMap.insert idx' idxs fromMap
              )

    -- Construct an internal graph where vertices are the representative
    -- indices of the groups of strongly connected components.
    sccVs :: IntSet
    sccVs = IntMap.keysSet sccFromMap

    -- The forward and reverse edges of this graph are non-distict.
    sccFEdgeMap, sccREdgeMap :: IntMap IntSet
    (sccFEdgeMap, sccREdgeMap) =
        IntMap.foldrWithKey aux (IntMap.empty, IntMap.empty) graph.edgeMap
      where
        aux ::
             Idx
          -> IntMap (Set e)
          -> (IntMap IntSet, IntMap IntSet)
          -> (IntMap IntSet, IntMap IntSet)
        aux fromIdx =
          let fromIdx' = sccToMap IntMap.! fromIdx
          in  flip . IntMap.foldrWithKey $ \toIdx _ ->
                case sccToMap IntMap.! toIdx of
                  toIdx'
                    | toIdx' /= fromIdx' ->
                        bimap (ins fromIdx' toIdx') (ins toIdx' fromIdx')
                    | otherwise ->
                        id

        ins :: Idx -> Idx -> IntMap IntSet -> IntMap IntSet
        ins lIdx rIdx = IntMap.insertWith (<>) lIdx (IntSet.singleton rIdx)

    -- Compute the topological sort of the internal graph.  The topological sort
    -- always exists because there are no cycles between groups of strongly
    -- connected components.
    idxs' :: [Idx]
    idxs' = Maybe.fromJust $
        let startIdxs = sccVs IntSet.\\ IntMap.keysSet sccREdgeMap
        in  auxF [] startIdxs sccFEdgeMap sccREdgeMap
      where
        auxF ::
             [Idx]
          -> IntSet
          -> IntMap IntSet
          -> IntMap IntSet
          -> Maybe [Idx]
        auxF acc startIdxs fEdgeMap rEdgeMap = case IntSet.minView startIdxs of
          Just (fromIdx, startIdxs') ->
            let (toIdxs, fEdgeMap') = first (maybe [] IntSet.elems) $
                  IntMap.updateLookupWithKey (\_ _ -> Nothing) fromIdx fEdgeMap
                (startIdxs'', rEdgeMap') =
                  auxR startIdxs' rEdgeMap fromIdx toIdxs
            in  auxF (fromIdx : acc) startIdxs'' fEdgeMap' rEdgeMap'
          Nothing
            | IntMap.null fEdgeMap -> Just (List.reverse acc)
            | otherwise            -> Nothing

        auxR ::
             IntSet
          -> IntMap IntSet
          -> Idx
          -> [Idx]
          -> (IntSet, IntMap IntSet)
        auxR startIdxs rEdgeMap fromIdx = \case
          toIdx : toIdxs ->
            let (mFromIdxs, rEdgeMap') =
                  IntMap.updateLookupWithKey
                    (\_ -> maybeEmpty IntSet.null . IntSet.delete fromIdx)
                    toIdx
                    rEdgeMap
                startIdxs' = case mFromIdxs of
                  Just fromIdxs
                    | fromIdxs /= IntSet.singleton fromIdx -> startIdxs
                  _otherwise -> IntSet.insert toIdx startIdxs
            in  auxR startIdxs' rEdgeMap' fromIdx toIdxs
          [] -> (startIdxs, rEdgeMap)

    -- Transform the topological sort of the internal graph to a list of
    -- vertices in the actual graph.  Each representative index is expanded to
    -- the (already sorted) list of graph indices, and the actual vertices are
    -- queried.
    vs :: [v]
    vs = flip Foldable.concatMap idxs' $ \idx' -> [
        graph.idxMap IntMap.! idx
      | idx <- sccFromMap IntMap.! idx'
      ]

-- | Sort the vertices of a graph using a topological sort of groups of strongly
-- connected components, ordering with the specified ordering function
sortBy :: Ord v => (v -> v -> Ordering) -> Digraph e v -> [v]
sortBy cmp = sort . reIndex cmp

-- | Depth-first traversal of the graph from the specified vertices
--
-- The specified vertices are traversed in the given order.  Vertices that have
-- already been processed are omitted from subsequent trees.
dfs :: Ord v => [v] -> Digraph e v -> [Tree v]
dfs fromVs graph =
      map (fmap (graph.idxMap IntMap.!))
    . dfs' graph
    $ Maybe.mapMaybe (graph.vMap Map.!?) fromVs

-- | Depth-first traversal of the graph
--
-- Vertices that have already been processed are omitted from subsequent trees.
dff :: Digraph e v -> [Tree v]
dff graph =
      map (fmap (graph.idxMap IntMap.!))
    $ dfs' graph (IntMap.keys graph.idxMap)

-- | Find the first vertex in a set of target vertices using a depth-first
-- traversal from the specified vertex
dfFindMember ::
     Ord v
  => v      -- ^ Vertex to search from
  -> Set v  -- ^ Target vertices
  -> Digraph e v
  -> Maybe v
dfFindMember fromV targetVs graph = fmap (graph.idxMap IntMap.!) $ do
    fromIdx <- Map.lookup fromV graph.vMap
    aux $ dfs' graph [fromIdx]
  where
    targetIdxs :: IntSet
    targetIdxs = IntSet.fromList $
      Maybe.mapMaybe (`Map.lookup` graph.vMap) (Set.toList targetVs)

    aux :: [Tree Idx] -> Maybe Idx
    aux = \case
      Tree.Node idx children : trees
        | IntSet.member idx targetIdxs -> Just idx
        | otherwise                    -> aux $ children ++ trees
      []                               -> Nothing

-- | Traverse a path from a vertex using the specified function
traversePathFrom :: forall e v r m.
     (Monad m, Ord v)
  => v                                 -- ^ Starting vertex
  -> ([(v, Set e)] -> m (Either v r))  -- ^ Choose next step
  -> Digraph e v
  -> m r
traversePathFrom fromV f graph = step fromV
  where
    step :: v -> m r
    step = either step return <=< f . getSuccessors

    getSuccessors :: v -> [(v, Set e)]
    getSuccessors v = Maybe.fromMaybe [] $ do
      idx   <- Map.lookup v graph.vMap
      toMap <- IntMap.lookup idx graph.edgeMap
      return $ map (first (graph.idxMap IntMap.!)) (IntMap.toAscList toMap)

-- | Find edges from a vertex and edges to all terminal vertices in paths from
-- that vertex
findEdges :: forall e v.
     (Ord e, Ord v)
  => v  -- ^ Starting vertex
  -> Digraph e v
  -> FindEdgesResult e
findEdges fromV graph = either id id $ do
    fromIdx <- maybe (Left FindEdgesNone) Right $
      Map.lookup fromV graph.vMap
    startIdxs <- maybe (Left FindEdgesNone) (Right . IntMap.toAscList) $
      IntMap.lookup fromIdx graph.edgeMap
    startEdges <-
      maybe (Left FindEdgesNone) Right . NonEmpty.nonEmpty . Set.elems $
        Set.unions (map snd startIdxs)
    terminalEdges <-
      maybe (Left FindEdgesInvalid) Right . NonEmpty.nonEmpty . Set.elems $
        getTerminalEdges startIdxs
    return $ FindEdgesFound startEdges terminalEdges
  where
    getTerminalEdges :: [(Idx, Set e)] -> Set e
    getTerminalEdges startIdxs = run (0, graph.nextIdx - 1) $
      \(check :: Int -> ST s Bool) ->
        let aux ::
                 IntSet          -- ^ Known terminal vertex indices
              -> Set e           -- ^ Accumulator
              -> [(Idx, Set e)]  -- ^ Frontier
              -> ST s (Set e)
            aux termIdxs acc = \case
              (idx, edges) : rest
                | IntSet.member idx termIdxs -> aux termIdxs (edges <> acc) rest
                | otherwise                  -> check idx >>= \case
                    True  -> aux termIdxs acc rest
                    False -> case IntMap.lookup idx graph.edgeMap of
                      Just toIdxs | not (IntMap.null toIdxs) ->
                        aux termIdxs acc $ IntMap.toAscList toIdxs ++ rest
                      _otherwise ->
                        aux (IntSet.insert idx termIdxs) (edges <> acc) rest
              [] -> return acc
        in  aux IntSet.empty Set.empty startIdxs

    run :: (Idx, Idx) -> (forall s. (Idx -> ST s Bool) -> ST s x) -> x
    run bounds f = ST.runST $ do
      m <- Array.newArray bounds False :: ST s (Array.STUArray s Idx Bool)
      f $ \idx -> do
        visited <- Array.readArray m idx
        unless visited $ Array.writeArray m idx True
        return visited

-- | 'findEdges' result
data FindEdgesResult e =
    -- | Invalid internal representation (should never happen)
    FindEdgesInvalid

    -- | Starting vertex is not in the graph or has no edges
  | FindEdgesNone

    -- | Edges from starting vertex and edges to terminal vertices
  | FindEdgesFound (NonEmpty e) (NonEmpty e)
  deriving stock (Eq, Show)

{-------------------------------------------------------------------------------
  Traversal
-------------------------------------------------------------------------------}

-- | Transform edges of a graph
--
-- This function may decrease the number of edges between two vertices.
mapEdges :: Ord e2 => (e1 -> e2) -> Digraph e1 v -> Digraph e2 v
mapEdges f graph =
    graph{ edgeMap = IntMap.map (IntMap.map (Set.map f)) graph.edgeMap }

-- | Transform vertices of a graph
--
-- The transformation function is passed the vertex and the outgoing edges from
-- that vertex.
mapVerticesOutgoingEdges :: forall e v1 v2.
     (Ord e, Ord v1, Ord v2)
  => (v1 -> Set e -> v2)
  -> Digraph e v1
  -> Digraph e v2
mapVerticesOutgoingEdges f graph = graph{
      vMap   = Map.mapKeys aux graph.vMap
    , idxMap = IntMap.map  aux graph.idxMap
    }
  where
    aux :: v1 -> v2
    aux fromV = f fromV (outgoingEdges fromV)

    outgoingEdges :: v1 -> Set e
    outgoingEdges fromV = Maybe.fromMaybe Set.empty $ do
      fromIdx <- Map.lookup fromV graph.vMap
      toMap   <- IntMap.lookup fromIdx graph.edgeMap
      return $ IntMap.foldl' (<>) Set.empty toMap

{-------------------------------------------------------------------------------
  Visualization
-------------------------------------------------------------------------------}

-- | Graph visualization options
data VisOptions e v = VisOptions {
      visVertex    :: v -> VisVertex  -- ^ Determine how to visualize a vertex
    , visEdge      :: e -> VisEdge    -- ^ Determine how to visualize an edge
    , reverseEdges :: Bool            -- ^ Visualize edges in reverse?
    }

-- | Vertex visualization
data VisVertex = VisVertex {
      label :: Maybe String  -- ^ Vertex label
    }

-- | Edge visualization
data VisEdge = VisEdge {
      label :: Maybe String  -- ^ Edge label
    , style :: VisEdgeStyle  -- ^ Edge style
    }

-- | Edge visualization style
data VisEdgeStyle =
    Solid   -- ^ Solid line
  | Dotted  -- ^ Dotted line

-- | Test graph visualization options, for debugging
testVisOptions :: (Show e, Show v) => VisOptions e v
testVisOptions = VisOptions{
      visVertex = \v -> VisVertex{
          label = Just (show v)
        }
    , visEdge   = \e -> VisEdge{
          label = Just (show e)
        , style = Solid
        }
    , reverseEdges = False
    }

-- | Render a Mermaid diagram
--
-- On GitHub, put Mermaid diagram code in a @mermaid@ clode block to render it.
--
-- Reference:
--
-- * <https://mermaid.js.org/>
renderMermaid :: VisOptions e v -> Digraph e v -> String
renderMermaid opts graph = unlines $ header : nodes ++ links
  where
    header :: String
    header = "graph TD"

    nodes :: [String]
    nodes = [
        let nodeId  = getNodeId idx
            nodeVis = opts.visVertex v
            label   = case nodeVis.label of
              Just l  -> "(\"" ++ escapeString l ++ "\")"
              Nothing -> ""
        in  indent ++ nodeId ++ label
      | (idx, v) <- IntMap.toAscList graph.idxMap
      ]

    links :: [String]
    links = [
        let fromNodeId =
              getNodeId $ if opts.reverseEdges then toIdx else fromIdx
            toNodeId   =
              getNodeId $ if opts.reverseEdges then fromIdx else toIdx
            edgeVis    = opts.visEdge edge
            edgeSyntax = getEdgeSyntax edgeVis.style
            label      = case edgeVis.label of
              Just l  -> "|\"" ++ escapeString l ++ "\"|"
              Nothing -> ""
        in  indent ++ fromNodeId ++ edgeSyntax ++ label ++ toNodeId
      | (fromIdx, toMap) <- IntMap.toAscList graph.edgeMap
      , (toIdx, edges) <- IntMap.toAscList toMap
      , edge <- Set.elems edges
      ]

    indent :: String
    indent = "  "

    escapeString :: String -> [Char]
    escapeString = concatMap $ \case
        '"' -> "&quot;"
        '<' -> "&lt;"
        '>' -> "&gt;"
        c   -> [c]

    getNodeId :: Idx -> String
    getNodeId idx = 'v' : show idx

    getEdgeSyntax :: VisEdgeStyle -> String
    getEdgeSyntax = \case
      Solid  -> "-->"
      Dotted -> "-.->"

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- GHC warns when using some partial functions, but there are still cases when
-- such functions are correct.  In this module, 'Tree' must have at least one
-- member, but 'Tree.flatten' returns a list.  This is an implementation of
-- 'head' that uses a partial function that GHC does not warn about (yet).
unsafeHead :: HasCallStack => [a] -> a
unsafeHead = Maybe.fromJust . Maybe.listToMaybe
{-# INLINE unsafeHead #-}

maybeEmpty :: (a -> Bool) -> a -> Maybe a
maybeEmpty p x
    | p x       = Nothing
    | otherwise = Just x

insertVertex' :: Ord v => v -> Digraph e v -> (Idx, Digraph e v)
insertVertex' v graph =
    case Map.insertLookupWithKey keepOld v graph.nextIdx graph.vMap of
      (Just idx, _)     -> (idx, graph)
      (Nothing,  vMap') ->
        let graph' = graph{
                nextIdx = graph.nextIdx + 1
              , vMap    = vMap'
              , idxMap  = IntMap.insert graph.nextIdx v graph.idxMap
              }
        in  (graph.nextIdx, graph')
  where
    keepOld :: k -> v -> v -> v
    keepOld _key _new old = old

insertEdge' :: forall e.
     Ord e
  => Idx
  -> e
  -> Idx
  -> IntMap (IntMap (Set e))
  -> IntMap (IntMap (Set e))
insertEdge' fromIdx edge toIdx = flip IntMap.alter fromIdx $
      Just
    . maybe (IntMap.singleton toIdx edges) (IntMap.insertWith (<>) toIdx edges)
  where
    edges :: Set e
    edges = Set.singleton edge

deleteVertexCombineEdges :: forall e v.
     (Ord v, Ord e)
  => (e -> e -> e)
  -> Digraph e v
  -> (Idx, v)
  -> Digraph e v
deleteVertexCombineEdges combine graph (delIdx, delV) = graph{
      vMap    = Map.delete delV graph.vMap
    , idxMap  = IntMap.delete delIdx graph.idxMap
    , edgeMap = IntMap.mapMaybeWithKey auxF graph.edgeMap
    }
  where
    auxF :: Idx -> IntMap (Set e) -> Maybe (IntMap (Set e))
    auxF fromIdx toMap
      | fromIdx == delIdx = Nothing  -- Remove all edges from vertex to remove
      | otherwise         = Just $
          case IntMap.updateLookupWithKey (\_ _ -> Nothing) delIdx toMap of
            (Nothing,    _)      -> toMap
            -- Redirect edges to vertex to remove to targets of that vertex
            (Just edges, toMap') ->
              Foldable.foldl' auxE toMap' (Set.elems edges)

    delToIdxEdges :: [(Idx, [e])]
    delToIdxEdges = maybe [] (map (fmap Set.elems) . IntMap.toList) $
      IntMap.lookup delIdx graph.edgeMap

    auxE :: IntMap (Set e) -> e -> IntMap (Set e)
    auxE toMap edge = Foldable.foldl' (auxC edge) toMap delToIdxEdges

    auxC :: e -> IntMap (Set e) -> (Idx, [e]) -> IntMap (Set e)
    auxC edge toMap (delToIdx, delEdges) =
      let edges = Set.fromList $ map (combine edge) delEdges
      in  IntMap.insertWith (<>) delToIdx edges toMap

reaches' :: IntMap (IntMap (Set e)) -> [Idx] -> Set Idx
reaches' edgeMap = aux Map.empty
  where
    aux :: Map Idx () -> [Idx] -> Set Idx
    aux acc = \case
      []         -> Map.keysSet acc
      idx : idxs ->
        case Map.insertLookupWithKey nop idx () acc of
          (Just (), _)    -> aux acc idxs
          (Nothing, acc') -> aux acc' $
            maybe [] IntMap.keys (IntMap.lookup idx edgeMap) ++ idxs

    nop :: k -> () -> () -> ()
    nop _key () () = ()

dfs' :: Digraph e v -> [Idx] -> [Tree Idx]
dfs' graph fromIdxs
    | graph.nextIdx == 0 = []
    | otherwise          = run (0, graph.nextIdx - 1) $
        \(contains :: Idx -> ST s Bool) (include  :: Idx -> ST s ()) ->
          let aux :: [Idx] -> ST s [Tree Idx]
              aux = \case
                []         -> return []
                idx : idxs -> contains idx >>= \case
                  True  -> aux idxs
                  False -> do
                    include idx
                    children <- aux $
                      maybe [] IntMap.keys (IntMap.lookup idx graph.edgeMap)
                    trees <- aux idxs
                    return $ Tree.Node idx children : trees
          in  aux fromIdxs
  where
    run ::
         (Idx, Idx)
      -> (forall s. (Idx -> ST s Bool) -> (Idx -> ST s ()) -> ST s x)
      -> x
    run bounds f = ST.runST $ do
      m <- Array.newArray bounds False :: ST s (Array.STUArray s Idx Bool)
      f (Array.readArray m) (\idx -> Array.writeArray m idx True)

postorderTree :: Tree a -> [a]
postorderTree tree = postorderForest tree.subForest ++ [tree.rootLabel]

postorderForest :: [Tree a] -> [a]
postorderForest = concatMap postorderTree

scc' :: Digraph e v -> [Tree Idx]
scc' graph =
      dfs' graph
    . reverse
    . postorderForest
    $ dfs' (transpose graph) (IntMap.keys graph.idxMap)
