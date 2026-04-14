module Test.HsBindgen.Unit.Digraph (tests) where

import Control.Monad (forM_, unless)
import Data.Bifunctor (bimap, first)
import Data.Char qualified as Char
import Data.Digraph (Digraph)
import Data.Digraph qualified as Digraph
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree qualified as Tree
import GHC.Stack (HasCallStack)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@=?))

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Unit.Digraph" [
      -- Construction
      testTranspose
      -- Deletion/Update
    , testDeleteEdgesFrom
    , testDeleteEdgesTo
    , testCombineParallelEdges
    , testFilterEdges
    , testFilterVerticesCombineEdges
      -- Query
    , testVertices
    , testNeighbors
    , testReaches
    , testTopSort
    , testTopSortBy
    , testDfs
    , testDff
    , testDfFindMember
    , testTraversePathFrom
    , testFindEdges
      -- Traversal
    , testMapEdges
    , testMapVerticesOutgoingEdges
    ]

--------------------------------------------------------------------------------

type E = String
type V = String
type G = Digraph E V

-- Empty graph
graph0 :: G
graph0 = Digraph.empty

-- Test graph 1: vertices, ordered
graph1Vs :: [V]
graph1Vs = mkVs [16, 15, 2, 14, 12, 3, 6, 10, 8, 5, 4, 1, 11, 7, 13, 9]
-- Indexes:       0   1  2   3   4  5  6   7  8  9 10 11  12 13  14 15

-- Test graph 1: edges
graph1Es :: [(E, (V, V))]
graph1Es = map (fmap (bimap mkV mkV)) [
      ("a", (16, 15))
    , ("b", (16, 2))
    , ("c", (2,  14))
    , ("d", (14, 12))
    , ("e", (14, 3))
    , ("f", (10, 14))
    , ("g", (8,  5))
    , ("h", (1,  11))
    , ("i", (1,  7))
    , ("j", (1,  13))
    , ("k", (7,  9))
    ]

-- Test graph 1, with vertices but no edges
graph1V :: G
graph1V = mkGraph graph1Vs []

-- Test graph 1, with edges
--
-- @
-- flowchart TD
--     v16
--     v15
--     v2
--     v14
--     v12
--     v3
--     v6
--     v10
--     v8
--     v5
--     v4
--     v1
--     v11
--     v7
--     v13
--     v9
--
--     v16-->|a|v15
--     v16-->|b|v2
--     v2-->|c|v14
--     v14-->|d|v12
--     v14-->|e|v3
--     v10-->|f|v14
--     v8-->|g|v5
--     v1-->|h|v11
--     v1-->|i|v7
--     v1-->|j|v13
--     v7-->|k|v9
-- @
graph1E :: G
graph1E = mkGraph graph1Vs graph1Es

--------------------------------------------------------------------------------

testTranspose :: TestTree
testTranspose = testGroup "transpose" [
      testCase "empty" $
        graph0 @=? Digraph.transpose graph0
    , testCase "no edges" $
        graph1V @=? Digraph.transpose (Digraph.transpose graph1V)
    , testCase "with edges" $
        graph1E @=? Digraph.transpose (Digraph.transpose graph1E)
    ]

testDeleteEdgesFrom :: TestTree
testDeleteEdgesFrom = testGroup "deleteEdgesFrom" [
      testCase "none" $
        graph1E @=? Digraph.deleteEdgesFrom Set.empty graph1E
    , testCase "unknown" $
        graph1E @=? Digraph.deleteEdgesFrom (vSet [42]) graph1E
    , testCase "orphan" $
        graph1E @=? Digraph.deleteEdgesFrom (vSet [6]) graph1E
    , testCase "leaf" $
        graph1E @=? Digraph.deleteEdgesFrom (vSet [9]) graph1E
    , testCase "one" $ do
        let expected = mkGraph graph1Vs $ filter (\ p -> fst p /= "c") graph1Es
        expected @=? Digraph.deleteEdgesFrom (vSet [2]) graph1E
    , testCase "two" $ do
        let expected = mkGraph graph1Vs $ filter (\ p -> fst p > "b") graph1Es
        expected @=? Digraph.deleteEdgesFrom (vSet [16]) graph1E
    ]

testDeleteEdgesTo :: TestTree
testDeleteEdgesTo = testGroup "deleteEdgesTo" [
      testCase "none" $
        graph1E @=? Digraph.deleteEdgesTo Set.empty graph1E
    , testCase "unknown" $
        graph1E @=? Digraph.deleteEdgesTo (vSet [42]) graph1E
    , testCase "orphan" $
        graph1E @=? Digraph.deleteEdgesTo (vSet [4]) graph1E
    , testCase "root" $
        graph1E @=? Digraph.deleteEdgesTo (vSet [8]) graph1E
    , testCase "one" $ do
        let expected = mkGraph graph1Vs $ filter (\ p -> fst p /= "i") graph1Es
        expected @=? Digraph.deleteEdgesTo (vSet [7]) graph1E
    , testCase "two" $ do
        let expected = mkGraph graph1Vs $
              filter (\ p -> fst p `notElem` ["c", "f"]) graph1Es
        expected @=? Digraph.deleteEdgesTo (vSet [14]) graph1E
    ]

testCombineParallelEdges :: TestTree
testCombineParallelEdges = testGroup "combineParallelEdges" [
      testCase "empty" $
        graph0 @=? Digraph.combineParallelEdges combine graph0
    , testCase "single" $
        graph1E @=? Digraph.combineParallelEdges combine graph1E
    , testCase "multi" $
        expected @=? Digraph.combineParallelEdges combine graph
    ]
  where
    graphVs :: [V]
    graphVs = mkVs [1, 2, 3]

    graph :: G
    graph = mkGraph graphVs $ map (fmap (bimap mkV mkV)) [
        ("a", (2, 3))
      , ("b", (2, 3))
      , ("c", (2, 3))
      , ("d", (3, 4))
      , ("e", (2, 4))
      , ("f", (2, 4))
      ]

    combine :: String -> String -> String
    combine l r = l ++ ':' : r

    expected :: G
    expected = mkGraph graphVs $ map (fmap (bimap mkV mkV)) [
        ("a:b:c", (2, 3))
      , ("d",     (3, 4))
      , ("e:f",   (2, 4))
      ]

testFilterEdges :: TestTree
testFilterEdges = testGroup "filterEdges" [
      testCase "empty" $
        graph0 @=? Digraph.filterEdges (const True) graph0
    , testCase "none" $
        graph1E @=? Digraph.filterEdges (const True) graph1E
    , testCase "all" $
        graph1V @=? Digraph.filterEdges (const False) graph1E
    , testCase "single" $ do
        let p        = (<= "c")
            expected = mkGraph graph1Vs $ filter (p . fst) graph1Es
        expected @=? Digraph.filterEdges p graph1E
    , testCase "multi" $ do
        let p        = (`notElem` ["b", "e"])
            expected = mkGraph graphVs $ filter (p . fst) graphEs
        expected @=? Digraph.filterEdges p graph
    ]
  where
    graphVs :: [V]
    graphVs = mkVs [1, 2, 3]

    graphEs :: [(E, (V, V))]
    graphEs = map (fmap (bimap mkV mkV)) [
        ("a", (2, 3))
      , ("b", (2, 3))
      , ("c", (2, 3))
      , ("d", (3, 4))
      , ("e", (2, 4))
      , ("f", (2, 4))
      ]

    graph :: G
    graph = mkGraph graphVs graphEs

testFilterVerticesCombineEdges :: TestTree
testFilterVerticesCombineEdges = testGroup "filterVerticesCombineEdges" [
      testCase "empty" $
        graph0 @=? Digraph.filterVerticesCombineEdges p (++) graph0
    , testCase "no edges" $ do
        let graph = Digraph.filterVerticesCombineEdges p (++) graph1V
        filter p graph1Vs @=? Digraph.vertices graph
    , testCase "with edges" $ do
        let graph = Digraph.filterVerticesCombineEdges p (++) graph1E
            vs    = Digraph.vertices graph
        filter p graph1Vs @=? vs
        forM_ vs $ \ v -> do
          let ns = Digraph.neighbors v graph
          case v of
            "v1"  -> expect ns [(9, ["ik"]), (11, ["h"]), (13, ["j"])]
            "v2"  -> expect ns []
            "v3"  -> expect ns []
            "v4"  -> expect ns []
            "v5"  -> expect ns []
            "v6"  -> expect ns []
            "v7"  -> expect ns []
            "v8"  -> expect ns [(5, ["g"])]
            "v9"  -> expect ns []
            "v10" -> expect ns [(3, ["fe"]), (12, ["fd"])]
            "v11" -> expect ns []
            "v12" -> expect ns []
            "v13" -> expect ns []
            "v14" -> expect ns []
            "v15" -> expect ns []
            "v16" -> expect ns [(3, ["bce"]), (12, ["bcd"]), (15, ["a"])]
            _     -> assertFailure $ "unexpected vertex: " ++ v
    ]
  where
    p :: String -> Bool
    p = (`notElem` (mkVs [2, 7, 14]))

    expect :: Map V (Set E) -> [(Int, [E])] -> Assertion
    expect ns = (@=? ns) . Map.fromList . map (bimap mkV Set.fromList)

testVertices :: TestTree
testVertices = testGroup "vertices" [
      testCase "empty"      $ []       @=? Digraph.vertices graph0
    , testCase "no edges"   $ graph1Vs @=? Digraph.vertices graph1V
    , testCase "with edges" $ graph1Vs @=? Digraph.vertices graph1E
    ]

testNeighbors :: TestTree
testNeighbors = testGroup "neighbors" [
      testCase "unknown" $
        expect [] @=? Digraph.neighbors "v42" graph1E
    , testCase "orphan" $
        expect [] @=? Digraph.neighbors "v6" graph1E
    , testCase "root" $
        expect [(7, "i"), (11, "h"), (13, "j")]
          @=? Digraph.neighbors "v1" graph1E
    , testCase "mid" $
        expect [(3, "e"), (12, "d")] @=? Digraph.neighbors "v14" graph1E
    , testCase "leaf" $
        expect [] @=? Digraph.neighbors "v5" graph1E
    ]
  where
    expect :: [(Int, E)] -> Map V (Set E)
    expect = Map.fromList . map (bimap mkV Set.singleton)

testReaches :: TestTree
testReaches = testGroup "reaches" [
      testCase "empty" $
        vSet [] @=? Digraph.reaches (vSet []) graph1E
    , testCase "unknown" $
        vSet [] @=? Digraph.reaches (vSet [42]) graph1E
    , testCase "orphan" $
        vSet [6] @=? Digraph.reaches (vSet [6]) graph1E
    , testCase "root" $
        vSet [1, 7, 9, 11, 13] @=? Digraph.reaches (vSet [1]) graph1E
    , testCase "mid" $
        vSet [3, 12, 14] @=? Digraph.reaches (vSet [14]) graph1E
    , testCase "leaf" $
        vSet [5] @=? Digraph.reaches (vSet [5]) graph1E
    , testCase "multi" $
        vSet [3, 4, 7, 9, 12, 14]
          @=? Digraph.reaches (vSet [4, 7, 9, 14]) graph1E
    ]

testTopSort :: TestTree
testTopSort = testGroup "topSort" [
      testCase "empty" $
        (Set.empty, []) @=? Digraph.topSort graph0
    , testCase "no edges" $
        (Set.empty, graph1Vs) @=? Digraph.topSort graph1V
    , testCase "with edges" $ do
        let expected =
              mkVs [16, 15, 2, 6, 10, 14, 12, 3, 8, 5, 4, 1, 11, 7, 13, 9]
        (Set.empty, expected) @=? Digraph.topSort graph1E
        assertTopSortDeps expected graph1Es
    ]

assertTopSortDeps :: [V] -> [(E, (V, V))] -> Assertion
assertTopSortDeps vs es = forM_ es $ \ (_edge, (fromV, toV)) -> do
    fromIdx <- elemIndex fromV vs
    toIdx   <- elemIndex toV   vs
    unless (fromIdx <= toIdx) $
      assertFailure $ show fromV ++ " not before " ++ show toV

testTopSortBy :: TestTree
testTopSortBy = testGroup "topSortBy" [
      testCase "empty" $
        (Set.empty, []) @=? Digraph.topSortBy cmp graph0
    , testCase "no edges" $ do
        let expected = List.sortBy cmp graph1Vs
        (Set.empty, expected) @=? Digraph.topSortBy cmp graph1V
    , testCase "with edges" $ do
        let expected =
              mkVs [1, 4, 6, 7, 8, 5, 9, 10, 11, 13, 16, 2, 14, 3, 12, 15]
        (Set.empty, expected) @=? Digraph.topSortBy cmp graph1E
        assertTopSortDeps expected graph1Es
    ]
  where
    cmp :: String -> String -> Ordering
    cmp (_ : l) (_ : r) = compare (read @Int l) (read @Int r)
    cmp l       r       = compare l r

testDfs :: TestTree
testDfs = testGroup "dfs" [
      testCase "empty" $
        [] @=? Digraph.dfs [] graph1E
    , testCase "unknown" $
        [] @=? Digraph.dfs ["v42"] graph1E
    , testCase "order" $ do
        let expected = map mkVs [
                Tree.Node 4 []
              , Tree.Node 10
                  [ Tree.Node 14
                      [ Tree.Node 12 []
                      , Tree.Node 3 []
                      ]
                  ]
              , Tree.Node 16
                  [ Tree.Node 15 []
                  , Tree.Node 2 []
                  ]
              ]
        expected @=? Digraph.dfs (mkVs [4, 10, 16]) graph1E
    , testCase "done" $ do
        let expected = map mkVs [
                Tree.Node 8
                  [ Tree.Node 5 []
                  ]
              ]
        expected @=? Digraph.dfs (mkVs [8, 5]) graph1E
    ]

testDff :: TestTree
testDff = testGroup "dff" [
      testCase "empty" $
        [] @=? Digraph.dff graph0
    , testCase "no edges" $
        [Tree.Node v [] | v <- graph1Vs] @=? Digraph.dff graph1V
    , testCase "with edges" $ do
        let expected = map mkVs [
                Tree.Node 16
                  [ Tree.Node 15 []
                  , Tree.Node 2
                      [ Tree.Node 14
                          [ Tree.Node 12 []
                          , Tree.Node 3 []
                          ]
                      ]
                  ]
              , Tree.Node 6 []
              , Tree.Node 10 []
              , Tree.Node 8
                  [ Tree.Node 5 []
                  ]
              , Tree.Node 4 []
              , Tree.Node 1
                  [ Tree.Node 11 []
                  , Tree.Node 7
                      [ Tree.Node 9 []
                      ]
                  , Tree.Node 13 []
                  ]
              ]
        expected @=? Digraph.dff graph1E
    ]

testDfFindMember :: TestTree
testDfFindMember = testGroup "dfFindMember" [
      testCase "unknown" $
        Nothing @=? Digraph.dfFindMember "v42" (vSet [42]) graph1E
    , testCase "self" $
        Just "v2" @=? Digraph.dfFindMember "v2" (vSet [1, 2]) graph1E
    , testCase "child" $
        Just "v14" @=? Digraph.dfFindMember "v2" (vSet [3, 12, 14]) graph1E
    , testCase "descendent" $
        Just "v9" @=? Digraph.dfFindMember "v1" (vSet [9, 13]) graph1E
    , testCase "none" $
        Nothing @=? Digraph.dfFindMember "v16" (vSet [4, 5, 10]) graph1E
    ]

testTraversePathFrom :: TestTree
testTraversePathFrom = testGroup "traversePathFrom" [
      testCase "orphan" $
        [Nothing] @=? Digraph.traversePathFrom "v6" (step [1, 6]) graph1E
    , testCase "all branches, some targets" $
        [Just "v15:a", Just "v14:c"]
          @=? Digraph.traversePathFrom "v16" (step [3, 6, 14, 15]) graph1E
    , testCase "some branches, some targets" $
        [Nothing, Just "v12:d", Nothing]
          @=? Digraph.traversePathFrom "v16" (step [6, 12]) graph1E
    , testCase "no branches, no targets" $
        [Nothing, Nothing, Nothing]
          @=? Digraph.traversePathFrom "v1" (step [3, 6]) graph1E
    ]
  where
    step :: [Int] -> [(V, Set E)] -> [Either V (Maybe String)]
    step targets = \case
      [] -> [Right Nothing]
      cs -> case List.partition ((`Set.member` vSet targets) . fst) cs of
        (hits, misses) ->
          map (Right . Just . renderHit) hits ++ map (Left . fst) misses

    renderHit :: (V, Set E) -> String
    renderHit (v, edges) = v ++ ':' : List.intercalate "," (Set.elems edges)

testFindEdges :: TestTree
testFindEdges = testGroup "findEdges" [
      testCase "empty" $
        Digraph.FindEdgesNone @=? Digraph.findEdges "v42" graph0
    , testCase "unknown" $
        Digraph.FindEdgesNone @=? Digraph.findEdges "v42" graph1E
    , testCase "orphan" $
        Digraph.FindEdgesNone @=? Digraph.findEdges "v6" graph1E
    , testCase "leaf" $
        Digraph.FindEdgesNone @=? Digraph.findEdges "v9" graph1E
    , testCase "minimal" $
        let expected =
              Digraph.FindEdgesFound
                (NonEmpty.fromList ["g"])
                (NonEmpty.fromList ["g"])
        in  expected @=? Digraph.findEdges "v8" graph1E
    , testCase "root" $
        let expected =
              Digraph.FindEdgesFound
                (NonEmpty.fromList ["a", "b"])
                (NonEmpty.fromList ["a", "d", "e"])
        in  expected @=? Digraph.findEdges "v16" graph1E
    ]

testMapEdges :: TestTree
testMapEdges = testGroup "mapEdges" [
      testCase "empty" $
        graph0 @=? Digraph.mapEdges f graph0
    , testCase "no edges" $
        graph1V @=? Digraph.mapEdges f graph1V
    , testCase "with edges" $ do
        let expected = mkGraph graph1Vs $ map (first f) graph1Es
        expected @=? Digraph.mapEdges f graph1E
    ]
  where
    f :: E -> E
    f = List.map Char.toUpper

testMapVerticesOutgoingEdges :: TestTree
testMapVerticesOutgoingEdges = testGroup "mapVerticesOutgoingEdges" [
      testCase "empty" $
        graph0 @=? Digraph.mapVerticesOutgoingEdges f graph0
    , testCase "no edges" $
        graph1V @=? Digraph.mapVerticesOutgoingEdges f graph1V
    , testCase "with edges" $ do
        let nameV = \case
              "v1"  -> "v1:h,i,j"
              "v2"  -> "v2:c"
              "v7"  -> "v7:k"
              "v8"  -> "v8:g"
              "v10" -> "v10:f"
              "v14" -> "v14:d,e"
              "v16" -> "v16:a,b"
              v     -> v
            expected = mkGraphWith nameV id graph1Vs graph1Es
        expected @=? Digraph.mapVerticesOutgoingEdges f graph1E
    ]
  where
    f :: V -> Set E -> V
    f v edges
      | Set.null edges = v
      | otherwise      = v ++ ':' : List.intercalate "," (Set.elems edges)

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

mkV :: Int -> V
mkV = ('v' :) . show

mkVs :: Functor f => f Int -> f V
mkVs = fmap mkV

vSet :: [Int] -> Set V
vSet = Set.fromList . mkVs

mkGraph :: [V] -> [(E, (V, V))] -> G
mkGraph vs =
    Foldable.foldl' insertEdge $
      Foldable.foldl' (flip Digraph.insertVertex) Digraph.empty vs
  where
    insertEdge :: G -> (E, (V, V)) -> G
    insertEdge g (e, (fromV, toV)) = Digraph.insertEdge fromV e toV g

mkGraphWith :: (V -> V) -> (E -> E) -> [V] -> [(E, (V, V))] -> G
mkGraphWith nameV nameE vs =
    Foldable.foldl' insertEdge $
      Foldable.foldl' (flip Digraph.insertVertex) Digraph.empty (map nameV vs)
  where
    insertEdge :: G -> (E, (V, V)) -> G
    insertEdge g (e, (fromV, toV)) =
      Digraph.insertEdge (nameV fromV) (nameE e) (nameV toV) g

elemIndex :: (Eq a, HasCallStack, Show a) => a -> [a] -> IO Int
elemIndex x xs = case List.elemIndex x xs of
    Just idx -> return idx
    Nothing  -> assertFailure $ "not found: " ++ show x
