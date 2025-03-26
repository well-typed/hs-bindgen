module HsBindgen.ExtBindings.Test (tests) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Yaml qualified as Yaml
import System.FilePath qualified as FilePath
import System.IO.Temp qualified as Temp
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)

import Clang.CNameSpelling
import Clang.Paths
import HsBindgen.ExtBindings
import HsBindgen.Imports
import HsBindgen.Lib
import HsBindgen.Resolve

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: ClangArgs -> TestTree
tests args = testGroup "HsBindgen.ExtBindings"
    [ testResolveExtBindings args
    , testMergeExtBindings
    , testLookupExtBindingsType
    , testLookupExtIdentifier
    , testLoadUnresolvedExtBindings
    ]

testResolveExtBindings :: ClangArgs -> TestTree
testResolveExtBindings args = testGroup "resolveExtBindings"
    [ testCase "empty" $ do
        (errs, extBindings) <-
          resolveExtBindings args emptyUnresolvedExtBindings
        Set.toList errs @?= []
        extBindings @?= emptyExtBindings
    , testCase "various" $ do
        (errs, extBindings) <-
          resolveExtBindings args variousUnresolvedExtBindings
        Set.toList errs
          @?= [ResolveHeaderNotFound (CHeaderQuoteIncludePath unknownH)]
        normalizeExtBindings extBindings @?= variousExtBindings
    ]

testMergeExtBindings :: TestTree
testMergeExtBindings = testGroup "mergeExtBindings"
    [ testCase "none" $ mergeExtBindings [] @?= Right emptyExtBindings
    , testCase "emptyL" $
        mergeExtBindings [emptyExtBindings, variousExtBindings]
          @?= Right variousExtBindings
    , testCase "emptyR" $
        mergeExtBindings [variousExtBindings, emptyExtBindings]
          @?= Right variousExtBindings
    , testCase "disjoint" $
        mergeExtBindings [variousExtBindings, disjointExtBindings]
          @?= Right mergedDisjointExtBindings
    , testCase "overlap" $
        mergeExtBindings [variousExtBindings, overlapExtBindings]
          @?= Right mergedOverlapExtBindings
    , testCase "conflict" $
        mergeExtBindings [overlapExtBindings, overlapExtBindings]
          @?= Left (MergeExtBindingsConflict (Set.fromList [sizeT]))
    ]

testLookupExtBindingsType :: TestTree
testLookupExtBindingsType = testGroup "lookupExtBindingsType"
    [ testCase "miss" $
        lookupExtBindingsType int32T emptyExtBindings @?= Nothing
    , testCase "hit" $
        lookupExtBindingsType int32T variousExtBindings
          @?= Just [(intPSet, int32ExtId)]
    ]

testLookupExtIdentifier :: TestTree
testLookupExtIdentifier = testGroup "lookupExtIdentifier"
    [ testCase "miss" $
        lookupExtIdentifier timePSet [(intPSet, int32ExtId)] @?= Nothing
    , testCase "hit" $
        let pSet = Set.singleton $ SourcePath (Text.pack stdintH)
        in  lookupExtIdentifier pSet [(intPSet, int32ExtId)] @?= Just int32ExtId
    ]

testLoadUnresolvedExtBindings :: TestTree
testLoadUnresolvedExtBindings = testGroup "loadUnresolvedExtBindings"
    [ testCase "various.yaml" $ do
        let path = "various.yaml"
        eeb <- loadUnresolvedExtBindings' path variousUnresolvedExtBindingsYaml
        case eeb of
          Right bindings -> bindings @?= variousUnresolvedExtBindings
          Left e -> assertFailure $ "unexpected error: " ++ show e
    , testCase "various.json" $ do
        let path = "various.json"
        eeb <- loadUnresolvedExtBindings' path variousUnresolvedExtBindingsJson
        case eeb of
          Right bindings -> bindings @?= variousUnresolvedExtBindings
          Left e -> assertFailure $ "unexpected error: " ++ show e
    , testCase "unknown-ext.data" $ do
        let path = "unknown-ext.data"
        eeb <- loadUnresolvedExtBindings' path ""
        case eeb of
          Left (LoadUnresolvedExtBindingsUnknownExtension p) ->
            FilePath.takeFileName p @?= path
          Left e -> assertFailure $ "unexpected error: " ++ show e
          Right bindings ->
            assertFailure $ "unexpected success: " ++ show bindings
    , testCase "empty-file.yaml" $ do
        let path = "empty-file.yaml"
        eeb <- loadUnresolvedExtBindings' path ""
        case eeb of
          Left (LoadUnresolvedExtBindingsYamlError p (Yaml.AesonException s))
            | " expected Object, but encountered Null" `List.isSuffixOf` s ->
                FilePath.takeFileName p @?= path
          Left e -> assertFailure $ "unexpected error: " ++ show e
          Right bindings ->
            assertFailure $ "unexpected success: " ++ show bindings
    , testCase "empty-file.json" $ do
        let path = "empty-file.json"
        eeb <- loadUnresolvedExtBindings' path ""
        case eeb of
          Left (LoadUnresolvedExtBindingsAesonError p s) -> do
            FilePath.takeFileName p @?= path
            s @?= "Unexpected end-of-input, expecting JSON value"
          Left e -> assertFailure $ "unexpected error: " ++ show e
          Right bindings ->
            assertFailure $ "unexpected success: " ++ show bindings
    , testCase "no-bindings.yaml" $ do
        let path = "no-bindings.yaml"
        eeb <- loadUnresolvedExtBindings' path "types: []"
        case eeb of
          Right bindings -> bindings @?= emptyUnresolvedExtBindings
          Left e -> assertFailure $ "unexpected error: " ++ show e
    , testCase "no-bindings.json" $ do
        let path = "no-bindings.json"
        eeb <- loadUnresolvedExtBindings' path "{\"types\":[]}"
        case eeb of
          Right bindings -> bindings @?= emptyUnresolvedExtBindings
          Left e -> assertFailure $ "unexpected error: " ++ show e
    , testCase "dup-key.yaml" $ do
        let path = "dup-key.yaml"
        eeb <- loadUnresolvedExtBindings' path $
          unlines ["types: []", "types: []"]
        case eeb of
          Left (LoadUnresolvedExtBindingsYamlWarning p ws) -> do
            FilePath.takeFileName p @?= path
            length ws @?= 1
          Left e -> assertFailure $ "unexpected error: " ++ show e
          Right bindings ->
            assertFailure $ "unexpected success: " ++ show bindings
    , testCase "conflict.yaml" $ do
        let path = "conflict.yaml"
        eeb <- loadUnresolvedExtBindings' path conflictYaml
        case eeb of
          Left (LoadUnresolvedExtBindingsConflict p cSet) -> do
            FilePath.takeFileName p @?= path
            cSet
              @?= Set.singleton
                    (CNameSpelling "foo_t", CHeaderQuoteIncludePath "test.h")
          Left e -> assertFailure $ "unexpected error: " ++ show e
          Right bindings ->
            assertFailure $ "unexpected success: " ++ show bindings
    , testCase "conflict.json" $ do
        let path = "conflict.json"
        eeb <- loadUnresolvedExtBindings' path conflictJson
        case eeb of
          Left (LoadUnresolvedExtBindingsConflict p cSet) -> do
            FilePath.takeFileName p @?= path
            cSet
              @?= Set.singleton
                    (CNameSpelling "foo_t", CHeaderQuoteIncludePath "test.h")
          Left e -> assertFailure $ "unexpected error: " ++ show e
          Right bindings ->
            assertFailure $ "unexpected success: " ++ show bindings
    ]

{-------------------------------------------------------------------------------
  Test data
-------------------------------------------------------------------------------}

emptyUnresolvedExtBindings :: UnresolvedExtBindings
emptyUnresolvedExtBindings = UnresolvedExtBindings
    { unresolvedExtBindingsTypes = Map.empty
    }

variousUnresolvedExtBindings :: UnresolvedExtBindings
variousUnresolvedExtBindings = UnresolvedExtBindings
    { unresolvedExtBindingsTypes = Map.fromList
        [ (int32T, [(intHSet,     int32ExtId)])
        , (int64T, [(intHSet,     int64ExtId)])
        , (sizeT,  [(sizeHSet,    sizeExtId)])
        , (timeT,  [(timeHSet,    timeExtId)])
        , (fooT,   [(unknownHSet, int64ExtId)])
        ]
    }
  where
    intHSet, sizeHSet, timeHSet, unknownHSet :: Set CHeaderIncludePath
    intHSet = Set.fromList $ CHeaderSystemIncludePath <$> [inttypesH, stdintH]
    sizeHSet = Set.fromList $ CHeaderSystemIncludePath <$> [stddefH, timeH]
    timeHSet = Set.fromList
      [ CHeaderSystemIncludePath timeH
      , CHeaderQuoteIncludePath unknownH
      ]
    unknownHSet = Set.singleton $ CHeaderQuoteIncludePath unknownH

variousExtBindings :: ExtBindings
variousExtBindings = ExtBindings
    { extBindingsTypes = Map.fromList
        [ (int32T, [(intPSet,  int32ExtId)])
        , (int64T, [(intPSet,  int64ExtId)])
        , (sizeT,  [(sizePSet, sizeExtId)])
        , (timeT,  [(timePSet, timeExtId)])
        ]
    }

disjointExtBindings, mergedDisjointExtBindings :: ExtBindings
disjointExtBindings = ExtBindings
    { extBindingsTypes = Map.singleton int16T [(intPSet, int16ExtId)]
    }
mergedDisjointExtBindings = ExtBindings
    { extBindingsTypes = Map.fromList
        [ (int16T, [(intPSet,  int16ExtId)])
        , (int32T, [(intPSet,  int32ExtId)])
        , (int64T, [(intPSet,  int64ExtId)])
        , (sizeT,  [(sizePSet, sizeExtId)])
        , (timeT,  [(timePSet, timeExtId)])
        ]
    }

overlapExtBindings, mergedOverlapExtBindings :: ExtBindings
overlapExtBindings = ExtBindings
    { extBindingsTypes = Map.singleton sizeT [(stdlibPSet, sizeExtId)]
    }
mergedOverlapExtBindings = ExtBindings
    { extBindingsTypes = Map.fromList
        [ (int32T, [(intPSet,    int32ExtId)])
        , (int64T, [(intPSet,    int64ExtId)])
        , (sizeT,  [(stdlibPSet, sizeExtId), (sizePSet, sizeExtId)])
        , (timeT,  [(timePSet,   timeExtId)])
        ]
    }

normalizeExtBindings :: ExtBindings -> ExtBindings
normalizeExtBindings ExtBindings{..} = ExtBindings
    { extBindingsTypes = aux extBindingsTypes
    }
  where
    aux ::
         Map CNameSpelling [(Set SourcePath, ExtIdentifier)]
      -> Map CNameSpelling [(Set SourcePath, ExtIdentifier)]
    aux = Map.map $ map $ first $ Set.map normalizeSourcePath

    normalizeSourcePath :: SourcePath -> SourcePath
    normalizeSourcePath =
      SourcePath . Text.pack . FilePath.takeFileName . getSourcePath

loadUnresolvedExtBindings' ::
     FilePath
  -> String
  -> IO (Either LoadUnresolvedExtBindingsException UnresolvedExtBindings)
loadUnresolvedExtBindings' filename content =
    Temp.withSystemTempDirectory "hs-bindgen-test-" $ \dir -> do
      let path = dir </> filename
      writeFile path content
      loadUnresolvedExtBindings path

fooT, int16T, int32T, int64T, sizeT, timeT :: CNameSpelling
fooT   = CNameSpelling "foo_t"
int16T = CNameSpelling "int16_t"
int32T = CNameSpelling "int32_t"
int64T = CNameSpelling "int64_t"
sizeT  = CNameSpelling "size_t"
timeT  = CNameSpelling "time_t"

inttypesH, stddefH, stdintH, stdlibH, timeH, unknownH :: FilePath
inttypesH = "inttypes.h"
stddefH   = "stddef.h"
stdintH   = "stdint.h"
stdlibH   = "stdlib.h"
timeH     = "time.h"
unknownH  = "hs_bindgen_test_unknown.h"

intPSet, sizePSet, stdlibPSet, timePSet :: Set SourcePath
intPSet = Set.fromList $ SourcePath . Text.pack <$> [inttypesH, stdintH]
sizePSet = Set.fromList $ SourcePath . Text.pack <$> [stddefH, timeH]
stdlibPSet = Set.singleton $ SourcePath (Text.pack stdlibH)
timePSet = Set.singleton $ SourcePath (Text.pack timeH)

int16ExtId, int32ExtId, int64ExtId, sizeExtId, timeExtId :: ExtIdentifier
int16ExtId = ExtIdentifier
    { extIdentifierPackage    = HsPackageName "base"
    , extIdentifierModule     = HsModuleName  "Data.Int"
    , extIdentifierIdentifier = HsIdentifier  "Int16"
    }
int32ExtId = ExtIdentifier
    { extIdentifierPackage    = HsPackageName "base"
    , extIdentifierModule     = HsModuleName  "Data.Int"
    , extIdentifierIdentifier = HsIdentifier  "Int32"
    }
int64ExtId = ExtIdentifier
    { extIdentifierPackage    = HsPackageName "base"
    , extIdentifierModule     = HsModuleName  "Data.Int"
    , extIdentifierIdentifier = HsIdentifier  "Int64"
    }
sizeExtId = ExtIdentifier
    { extIdentifierPackage    = HsPackageName "base"
    , extIdentifierModule     = HsModuleName  "Foreign.C.Types"
    , extIdentifierIdentifier = HsIdentifier  "CSize"
    }
timeExtId = ExtIdentifier
    { extIdentifierPackage    = HsPackageName "base"
    , extIdentifierModule     = HsModuleName  "Foreign.C.Types"
    , extIdentifierIdentifier = HsIdentifier  "CTime"
    }

variousUnresolvedExtBindingsYaml :: String
variousUnresolvedExtBindingsYaml = unlines
    [ "types:"
    , "  - cname: int32_t"
    , "    headers:"
    , "      - system:inttypes.h"
    , "      - system:stdint.h"
    , "    identifier: Int32"
    , "    module: Data.Int"
    , "    package: base"
    , "  - cname: int64_t"
    , "    headers:"
    , "      - system:inttypes.h"
    , "      - system:stdint.h"
    , "    identifier: Int64"
    , "    module: Data.Int"
    , "    package: base"
    , "  - cname: size_t"
    , "    headers:"
    , "      - system:stddef.h"
    , "      - system:time.h"
    , "    identifier: CSize"
    , "    module: Foreign.C.Types"
    , "    package: base"
    , "  - cname: time_t"
    , "    headers:"
    , "      - system:time.h"
    , "      - hs_bindgen_test_unknown.h"
    , "    identifier: CTime"
    , "    module: Foreign.C.Types"
    , "    package: base"
    , "  - cname: foo_t"
    , "    headers:"
    , "      - hs_bindgen_test_unknown.h"
    , "    identifier: Int64"
    , "    module: Data.Int"
    , "    package: base"
    ]

variousUnresolvedExtBindingsJson :: String
variousUnresolvedExtBindingsJson = unlines
    [ "{ \"types\":"
    , "    [ { \"cname\": \"int32_t\""
    , "      , \"headers\":"
    , "          [ \"system:inttypes.h\""
    , "          , \"system:stdint.h\""
    , "          ]"
    , "      , \"identifier\": \"Int32\""
    , "      , \"module\": \"Data.Int\""
    , "      , \"package\": \"base\""
    , "      }"
    , "    , { \"cname\": \"int64_t\""
    , "      , \"headers\":"
    , "          [ \"system:inttypes.h\""
    , "          , \"system:stdint.h\""
    , "          ]"
    , "      , \"identifier\": \"Int64\""
    , "      , \"module\": \"Data.Int\""
    , "      , \"package\": \"base\""
    , "      }"
    , "    , { \"cname\": \"size_t\""
    , "      , \"headers\":"
    , "          [ \"system:stddef.h\""
    , "          , \"system:time.h\""
    , "          ]"
    , "      , \"identifier\": \"CSize\""
    , "      , \"module\": \"Foreign.C.Types\""
    , "      , \"package\": \"base\""
    , "      }"
    , "    , { \"cname\": \"time_t\""
    , "      , \"headers\":"
    , "          [ \"system:time.h\""
    , "          , \"hs_bindgen_test_unknown.h\""
    , "          ]"
    , "      , \"identifier\": \"CTime\""
    , "      , \"module\": \"Foreign.C.Types\""
    , "      , \"package\": \"base\""
    , "      }"
    , "    , { \"cname\": \"foo_t\""
    , "      , \"headers\":"
    , "          [ \"hs_bindgen_test_unknown.h\""
    , "          ]"
    , "      , \"identifier\": \"Int64\""
    , "      , \"module\": \"Data.Int\""
    , "      , \"package\": \"base\""
    , "      }"
    , "    ]"
    , "}"
    ]

conflictYaml :: String
conflictYaml = unlines $ "types:" : foo ++ foo
  where
    foo :: [String]
    foo =
      [ "  - cname: foo_t"
      , "    headers:"
      , "      - test.h"
      , "    identifier: FooT"
      , "    module: Test"
      , "    package: test"
      ]

conflictJson :: String
conflictJson = unlines
    [ "{ \"types\":"
    , "    [ { \"cname\": \"foo_t\""
    , "      , \"headers\": [\"test.h\"]"
    , "      , \"identifier\": \"FooT\""
    , "      , \"module\": \"Test\""
    , "      , \"package\": \"base\""
    , "      }"
    , "    , { \"cname\": \"foo_t\""
    , "      , \"headers\": [\"test.h\"]"
    , "      , \"identifier\": \"FooT\""
    , "      , \"module\": \"Test\""
    , "      , \"package\": \"base\""
    , "      }"
    , "    ]"
    , "}"
    ]
