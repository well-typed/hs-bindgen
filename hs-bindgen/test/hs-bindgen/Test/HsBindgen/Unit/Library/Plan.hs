-- | The modules of a library and their order
module Test.HsBindgen.Unit.Library.Plan (tests) where

import Data.Foldable (toList)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, Property,
                              QuickCheckTests (..), chooseInt, classify,
                              conjoin, counterexample, frequency, shuffle,
                              sublistOf, testProperty, vectorOf, (===))

import HsBindgen.Library.Plan (Module (..), Uses (..), plan)

{-------------------------------------------------------------------------------
  List of tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Unit.Library.Plan" [
      testGroup "examples" [
          testCase "nothing is owned" $
            planOf [] [] [] [] @?= []
        , testCase "a header that owns nothing has no module" $
            planOf ["a", "b"] [("x", "b")] [] []
              @?= [one "b" ["x"] []]
        , testCase "independent headers come in include order" $
            planOf ["c", "a", "b"] [("x", "a"), ("y", "b"), ("z", "c")] [] []
              @?= [one "c" ["z"] [], one "a" ["x"] [], one "b" ["y"] []]
        , testCase "a header comes after the header that it uses" $
            -- forward_decl: b.h includes a.h and defines the struct that the
            -- function of a.h takes
            planOf
              ["a", "b"]
              [("shape_draw", "a"), ("struct shape", "b")]
              [("shape_draw", ["struct shape"])]
              [("b", ["a"])]
              @?= [ one "b" ["struct shape"] []
                  , one "a" ["shape_draw"]   ["b"]
                  ]
        , testCase "a round has every module that is ready, in include order" $
            -- mylib: ops.h and mylib.h use types.h, internal.h uses nothing
            planOf
              ["types", "ops", "internal", "mylib"]
              [ ("point_t", "types"), ("point_add", "ops")
              , ("secret", "internal"), ("point_zero", "mylib")
              ]
              [("point_add", ["point_t"]), ("point_zero", ["point_t"])]
              [("mylib", ["types", "ops", "internal"])]
              @?= [ one "types"    ["point_t"]    []
                  , one "internal" ["secret"]     []
                  , one "ops"      ["point_add"]  ["types"]
                  , one "mylib"    ["point_zero"] ["types"]
                  ]
        , testCase "a use through a declaration that nobody owns counts" $
            planOf
              ["a", "b"]
              [("f", "a"), ("t", "b")]
              [("f", ["vendor"]), ("vendor", ["t"])]
              []
              @?= [one "b" ["t"] [], one "a" ["f"] ["b"]]
        , testCase "headers that use each other share a module" $
            -- elaborate: the header that includes the other gives the name
            planOf
              ["inner", "elaborate"]
              [ ("inner_int", "inner"), ("INNER_A", "inner")
              , ("outer_int", "elaborate"), ("OUTER_B", "elaborate")
              ]
              [("inner_int", ["outer_int"]), ("OUTER_B", ["INNER_A"])]
              [("elaborate", ["inner"])]
              @?= [ Module{
                        headers = "elaborate" :| ["inner"]
                      , decls   = Set.fromList
                          ["INNER_A", "OUTER_B", "inner_int", "outer_int"]
                      , uses    = Set.empty
                      }
                  ]
        , testCase "headers that use each other by way of a third share it" $
            planOf
              ["a", "b", "c", "d"]
              [("x", "a"), ("y", "b"), ("z", "c"), ("w", "d")]
              [("x", ["y"]), ("y", ["z"]), ("z", ["x"]), ("w", ["x"])]
              [("c", ["a", "b"]), ("b", ["a"])]
              @?= [ Module{
                        headers = "c" :| ["a", "b"]
                      , decls   = Set.fromList ["x", "y", "z"]
                      , uses    = Set.empty
                      }
                  , one "d" ["w"] ["c"]
                  ]
        , testCase "of headers that include equally many, the first names" $
            planOf
              ["b", "a"]
              [("x", "a"), ("y", "b")]
              [("x", ["y"]), ("y", ["x"])]
              []
              @?= [ Module{
                        headers = "b" :| ["a"]
                      , decls   = Set.fromList ["x", "y"]
                      , uses    = Set.empty
                      }
                  ]
        ]
    , localOption (QuickCheckTests 1000) $ testGroup "properties" [
          testProperty "every owning header is in one module" prop_partition
        , testProperty "headers share a module iff they use each other" prop_sharing
        , testProperty "a module has the declarations of its headers" prop_decls
        , testProperty "a module uses the modules of the headers it uses" prop_uses
        , testProperty "the name is of the header that includes most" prop_name
        , testProperty "uses first, then include order" prop_order
        ]
    ]

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

-- | Plan a library that is written down as lists
planOf ::
     [String]              -- ^ The headers in include order
  -> [(String, String)]    -- ^ Declarations and the header that owns each
  -> [(String, [String])]  -- ^ What a declaration uses directly
  -> [(String, [String])]  -- ^ What a header includes, directly or indirectly
  -> [Module String String]
planOf order owner direct included = plan Uses{
      owner    = Map.fromList owner
    , uses     = closure (fmap Set.fromList (Map.fromList direct))
    , includes = \header ->
        maybe Set.empty Set.fromList (lookup header included)
    , order    = order
    }

-- | A module of one header
one :: String -> [String] -> [String] -> Module String String
one header decls uses = Module{
      headers = header :| []
    , decls   = Set.fromList decls
    , uses    = Set.fromList uses
    }

{-------------------------------------------------------------------------------
  Properties

  Each property states what the plan has to be like in terms that do not
  depend on how it is computed.
-------------------------------------------------------------------------------}

prop_partition :: Library -> Property
prop_partition lib = withPlan lib $ \modules ->
        List.sort (concatMap (toList . (.headers)) modules)
    === Set.toAscList (owning lib)

prop_sharing :: Library -> Property
prop_sharing lib = withPlan lib $ \modules ->
    classify (any shared modules) "some headers share a module" $
      conjoin [
            counterexample (show (a, b)) $
              (moduleOf a == moduleOf b) === (a == b || useEachOther a b)
          | let moduleOf h = List.findIndex (elem h . (.headers)) modules
          , a <- Set.toList (owning lib)
          , b <- Set.toList (owning lib)
          ]
  where
    useEachOther :: Int -> Int -> Bool
    useEachOther a b = a `reaches` b && b `reaches` a

    reaches :: Int -> Int -> Bool
    reaches a b = b `Set.member` closure (headerUses lib) (Set.singleton a)

prop_decls :: Library -> Property
prop_decls lib = withPlan lib $ \modules -> conjoin [
        m.decls === Map.keysSet (Map.filter (`elem` m.headers) lib.owner)
      | m <- modules
      ]

prop_uses :: Library -> Property
prop_uses lib = withPlan lib $ \modules -> conjoin [
        m.uses === Set.fromList [
            NonEmpty.head other.headers
          | other <- modules
          , other /= m
          , or [ b `Set.member` Map.findWithDefault Set.empty a (headerUses lib)
               | a <- toList m.headers
               , b <- toList other.headers
               ]
          ]
      | m <- modules
      ]

prop_name :: Library -> Property
prop_name lib = withPlan lib $ \modules -> conjoin [
        counterexample (show m) $ m.headers === best :| List.delete best inOrder
      | m <- modules
      , let inOrder  = List.sortOn (position lib) (toList m.headers)
            count h  = Set.size $ Set.intersection
                         (Map.findWithDefault Set.empty h lib.included)
                         (Set.delete h (Set.fromList inOrder))
      , best : _ <- [List.sortOn (\h -> (Down (count h), position lib h)) inOrder]
      ]

prop_order :: Library -> Property
prop_order lib = withPlan lib $ \modules ->
    classify (not (all (Set.null . (.uses)) modules)) "some module uses another" $
      let name m  = NonEmpty.head m.headers
          byName  = Map.fromList [(name m, m) | m <- modules]
          -- How many rounds of other modules have to be done first.  The
          -- uses between modules must not go in a circle; if they do, the
          -- bound on the depth makes that show as a wrong order.
          level :: Int -> Module Int Int -> Int
          level depth m
            | depth > length modules = depth
            | otherwise              =
                case mapMaybe (`Map.lookup` byName) (Set.toList m.uses) of
                  []   -> 0
                  used -> 1 + maximum (map (level (depth + 1)) used)
          key m   = (level 0 m, position lib (name m))
      in  map name modules === map name (List.sortOn key modules)

{-------------------------------------------------------------------------------
  Libraries
-------------------------------------------------------------------------------}

-- | A small library: headers and declarations are numbers
data Library = Library {
      -- | The headers in include order
      order :: [Int]
      -- | The header that owns a declaration, for those that are owned
    , owner :: Map Int Int
      -- | What each declaration uses directly
    , direct :: Map Int (Set Int)
      -- | What each header includes
    , included :: Map Int (Set Int)
    }
  deriving stock (Show)

instance Arbitrary Library where
  arbitrary = do
      nHeaders <- chooseInt (0, 6)
      nDecls   <- chooseInt (0, 12)
      let headers = [0 .. nHeaders - 1]
          decls   = [0 .. nDecls   - 1]
      -- Most of the time the order lists every header, as it does for a
      -- library.  The plan is defined when it does not.
      order    <- frequency [
                      (9, shuffle headers)
                    , (1, shuffle headers >>= sublistOf)
                    ]
      owner    <- fmap (Map.fromList . catMaybes) . sequence $ [
                      frequency $
                           (1, pure Nothing)
                         : [ (5, Just . (decl,) <$> chooseInt (0, nHeaders - 1))
                           | nHeaders > 0
                           ]
                    | decl <- decls
                    ]
      direct   <- fmap Map.fromList . sequence $ [
                      (decl,) . Set.fromList <$> some decls
                    | decl <- decls
                    ]
      included <- fmap Map.fromList . sequence $ [
                      (header,) . Set.fromList <$> sublistOf headers
                    | header <- headers
                    ]
      pure Library{
          order    = order
        , owner    = owner
        , direct   = direct
        , included = included
        }
    where
      -- A few of the given elements, most of the time none or one
      some :: [Int] -> Gen [Int]
      some [] = pure []
      some xs = do
          n <- frequency [(4, pure 0), (4, pure 1), (1, pure 2)]
          vectorOf n (chooseInt (0, length xs - 1))

-- | Plan the library, and show the plan if the property does not hold
withPlan :: Library -> ([Module Int Int] -> Property) -> Property
withPlan lib prop = counterexample (show modules) (prop modules)
  where
    modules :: [Module Int Int]
    modules = plan Uses{
          owner    = lib.owner
        , uses     = closure lib.direct
        , includes = \header -> Map.findWithDefault Set.empty header lib.included
        , order    = lib.order
        }

-- | The headers that own a declaration
owning :: Library -> Set Int
owning lib = Set.fromList (Map.elems lib.owner)

-- | The other headers that a header uses: a declaration that it owns uses,
-- directly or indirectly, a declaration that the other one owns
headerUses :: Library -> Map Int (Set Int)
headerUses lib = Map.fromListWith Set.union [
      (a, if a == b then Set.empty else Set.singleton b)
    | (declA, a) <- Map.toList lib.owner
    , declB      <- Set.toList (closure lib.direct (Set.singleton declA))
    , Just b     <- [Map.lookup declB lib.owner]
    ]

-- | Where a header is in include order
--
-- The order may leave a header out.  Such a header comes after the others.
position :: Library -> Int -> (Int, Int)
position lib header =
    (fromMaybe (length lib.order) (List.elemIndex header lib.order), header)

shared :: Module header decl -> Bool
shared m = length m.headers > 1

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | What is reached from the given elements, the elements included
closure :: forall a. Ord a => Map a (Set a) -> Set a -> Set a
closure next = go Set.empty . Set.toList
  where
    go :: Set a -> [a] -> Set a
    go seen []       = seen
    go seen (x : xs)
      | x `Set.member` seen = go seen xs
      | otherwise           =
          go (Set.insert x seen) $
            Set.toList (Map.findWithDefault Set.empty x next) ++ xs
