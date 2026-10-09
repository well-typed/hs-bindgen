-- | Binding specifications: reading and merging
--
-- The golden tests under @binding-specs@ check the outcome for a whole module.
-- These tests check the pieces on their own.
module Test.HsBindgen.Unit.BindingSpec (tests) where

import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import Clang.Paths (RealPath (..))

import HsBindgen.BindingSpec.Private.Common (BindingSpecMergeMsg (..),
                                             Omittable (..))
import HsBindgen.BindingSpec.Private.V1 (CTypeSpec (..), MergedBindingSpecs,
                                         ResolvedBindingSpec)
import HsBindgen.BindingSpec.Private.V1 qualified as BindingSpec
import HsBindgen.BindingSpec.Private.Version (BindingSpecCompatibility (..))
import HsBindgen.Imports (def, first)
import HsBindgen.IR.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer (nullTracer)

import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  List of tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests getTestResources = testGroup "Test.HsBindgen.Unit.BindingSpec" [
      testGroup "readFile" [
          testCase "file names no module" $
            readsAs getTestResources
              "binding-specs/merge/omit_twice/report_p.yaml"
              (Nothing, "Example")
        , testCase "file names a module" $
            readsAs getTestResources
              "binding-specs/merge/omit_and_bind/stopwatch_p.yaml"
              (Just "Lib.Stopwatch", "Lib.Stopwatch")
        ]
    , testGroup "merge" [
          testCase "omit, omit" $
            [omits "Lib.A" libH, omits "Lib.B" libH] `mergesTo` Omitted
        , testCase "omit, bind" $
            [omits "Lib.A" libH, binds "Lib.B" libH] `mergesTo` BoundIn "Lib.B"
        , testCase "bind, omit" $
            [binds "Lib.A" libH, omits "Lib.B" libH] `mergesTo` BoundIn "Lib.A"
        , testCase "bind, bind under one header" $
            [binds "Lib.A" libH, binds "Lib.B" libH] `mergesTo` Conflict
        , testCase "bind, bind under different headers" $
            [binds "Lib.A" libH, binds "Lib.B" otherH] `mergesTo` BoundIn "Lib.A"
        ]
    ]

{-------------------------------------------------------------------------------
  Reading a specification that may leave out its module
-------------------------------------------------------------------------------}

-- | Read a golden header's specification with default module @Example@
--
-- The expectation is the module that the file names, and the module of the
-- specification that was read.
readsAs ::
     IO TestResources
  -> FilePath
  -> (Maybe Hs.ModuleName, Hs.ModuleName)
  -> Assertion
readsAs getTestResources file expected = do
    testResources <- getTestResources
    let path = testResources.packageRoot
           </> "test-artefacts" </> "headers" </> "golden" </> file
    result <-
      BindingSpec.readFile nullTracer BindingSpecStrict (Just "Example") path
    fmap (fmap (.moduleName)) result @?= Just expected

{-------------------------------------------------------------------------------
  Merging two specifications for one C type
-------------------------------------------------------------------------------}

-- | What merging does with a declaration of 'stopwatch' in 'libH'
data Outcome =
    Omitted
  | BoundIn Hs.ModuleName
  | NoEntry
  | Conflict
  | OtherMessages [String]
  deriving stock (Eq, Show)

-- | Merge the specifications, all at once and one at a time
mergesTo :: [ResolvedBindingSpec] -> Outcome -> Assertion
mergesTo specs expected = do
    outcome (BindingSpec.merge specs) @?= expected
    outcome oneAtATime                @?= expected
  where
    oneAtATime :: ([BindingSpecMergeMsg], MergedBindingSpecs)
    oneAtATime = Foldable.foldl' mergeOne (BindingSpec.merge []) specs

    mergeOne ::
         ([BindingSpecMergeMsg], MergedBindingSpecs)
      -> ResolvedBindingSpec
      -> ([BindingSpecMergeMsg], MergedBindingSpecs)
    mergeOne (msgs, merged) spec =
        first (msgs ++) $ BindingSpec.mergeInto merged [spec]

    outcome :: ([BindingSpecMergeMsg], MergedBindingSpecs) -> Outcome
    outcome = \case
      ([], merged) ->
        case BindingSpec.lookupMergedBindingSpecs stopwatch libPaths merged of
          Just (hsModuleName, Require{}, _) -> BoundIn hsModuleName
          Just (_,            Omit,      _) -> Omitted
          Nothing                           -> NoEntry
      ([BindingSpecMergeConflict cDeclId], _)
        | cDeclId == stopwatch -> Conflict
      (msgs, _) -> OtherMessages (map show msgs)

    libPaths :: Set RealPath
    libPaths = Set.singleton (snd libH)

{-------------------------------------------------------------------------------
  Specifications
-------------------------------------------------------------------------------}

type Header = (C.HashIncludeArg, RealPath)

libH, otherH :: Header
libH   = ("lib.h",   RealPath "/include/lib.h")
otherH = ("other.h", RealPath "/include/other.h")

stopwatch :: C.DeclId
stopwatch = C.DeclId{
      name      = C.DeclName "stopwatch" (C.NameKindTagged C.TagKindStruct)
    , isUnnamed = False
    }

omits :: Hs.ModuleName -> Header -> ResolvedBindingSpec
omits hsModuleName header = BindingSpec.BindingSpec{
      moduleName = hsModuleName
    , cTypes     = Map.singleton stopwatch [(Set.singleton header, Omit)]
    , hsTypes    = Map.empty
    }

binds :: Hs.ModuleName -> Header -> ResolvedBindingSpec
binds hsModuleName header = BindingSpec.BindingSpec{
      moduleName = hsModuleName
    , cTypes     = Map.singleton stopwatch [
                       (Set.singleton header, Require cTypeSpec)
                     ]
    , hsTypes    = Map.singleton hsName def
    }
  where
    hsName :: Hs.Name Hs.NsTypeConstr
    hsName = Hs.UnsafeName "Stopwatch"

    cTypeSpec :: CTypeSpec
    cTypeSpec = CTypeSpec{
          hsName = Just hsName
        , enum   = Nothing
        }
