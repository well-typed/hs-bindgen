-- | Merging of external binding specifications
--
-- The golden tests under @binding-specs/merge@ check the outcome for a whole
-- module. These tests check the rule itself, in both orders.
module Test.HsBindgen.Unit.BindingSpec (tests) where

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import Clang.Paths (RealPath (..))

import HsBindgen.BindingSpec.Private.Common (BindingSpecMergeMsg (..),
                                             Omittable (..))
import HsBindgen.BindingSpec.Private.V1 (CTypeSpec (..), ResolvedBindingSpec)
import HsBindgen.BindingSpec.Private.V1 qualified as BindingSpec
import HsBindgen.Imports (def)
import HsBindgen.IR.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  List of tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Unit.BindingSpec" [
      testGroup "merge" [
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

mergesTo :: [ResolvedBindingSpec] -> Outcome -> Assertion
mergesTo specs expected = outcome @?= expected
  where
    outcome :: Outcome
    outcome = case BindingSpec.merge specs of
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
