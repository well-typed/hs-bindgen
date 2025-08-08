module HsBindgen.Backend.Artefact.Test.Hs (
    genTestsHs
  ) where

--import Data.Bits qualified as Bits
--import Data.List qualified as List
--import Data.Vec.Lazy qualified as Vec
--import System.FilePath qualified as FilePath

import HsBindgen.Errors
--import HsBindgen.Backend.Artefact.Test.Internal
import HsBindgen.Backend.Hs.AST qualified as Hs
--import HsBindgen.Backend.Hs.AST.Name
--import Text.SimplePrettyPrint

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

-- | Generate Haskell test modules
genTestsHs ::
     FilePath  -- ^ Test module path
  -> FilePath  -- ^ Spec module path
  -> FilePath  -- ^ Main module path
  -> String    -- ^ Generated Haskell module name
  -> FilePath  -- ^ C test header file path
  -> Int       -- ^ Maximum line length
  -> [Hs.Decl] -- ^ Declarations
  -> IO ()
genTestsHs = throwPure_TODO 22 "generate test suite"

{-
genTestsHs
  hsTestPath
  hsSpecPath
  hsMainPath
  moduleName
  cTestHeaderPath
  lineLength
  decls = do
    writeFile hsTestPath $ renderPretty ctx HsTestModule{..}
    writeFile hsSpecPath $ renderPretty ctx (HsSpecModule hsTestModuleName)
    writeFile hsMainPath $ renderPretty ctx HsMainModule
  where
    ctx :: Context
    ctx = mkContext lineLength

    cTestHeaderFilename :: FilePath
    cTestHeaderFilename = FilePath.takeFileName cTestHeaderPath

    cFunPrefix :: CFunPrefix
    cFunPrefix = getCFunPrefix cTestHeaderFilename

    hsTestModulePragmas :: [Pragma]
    hsTestModulePragmas =
      [ "LANGUAGE CApiFFI"
      , "LANGUAGE RecordWildCards"
      , "LANGUAGE TypeApplications"
      , "OPTIONS_GHC -Wno-orphans"
      ]

    hsTestModuleName :: ModuleName
    hsTestModuleName = moduleName ++ ".Test"

    hsTestModuleImports :: [Import]
    hsTestModuleImports = List.sort
      -- source below ordered by abbreviation
      [ ("Foreign",                             "F")
      , ("Foreign.C",                           "FC")
      , ("HsBindgen.TestRuntime.GenSeq",        "GenSeq")
      , ("HsBindgen.TestRuntime.Preturb",       "Preturb")
      , ("Data.Proxy",                          "Proxy")
      , ("Test.QuickCheck",                     "QC")
      , ("HsBindgen.TestRuntime.RepZero",       "RepZero")
      , ("HsBindgen.TestRuntime.SameSemantics", "SameSemantics")
      , ("HsBindgen.TestRuntime.Storable",      "Storable")
      , ("Test.Tasty",                          "Tasty")
      , ("Test.Tasty.HUnit",                    "THU")
      , ("Test.Tasty.QuickCheck",               "TQC")
      , (moduleName,                            "X")
      ]

    hsTestModuleFfiFunctions :: [FfiFunction]
    hsTestModuleFfiFunctions =
      concatMap (getFfiFunctions cTestHeaderFilename cFunPrefix) decls

    hsTestModuleOrphanInstances :: [OrphanInstance]
    hsTestModuleOrphanInstances = concatMap getOrphanInstances decls

    hsTestModuleTypeTests :: [TypeTest]
    hsTestModuleTypeTests = concatMap getTypeTests decls

    hsTestModuleTestsFun :: TestsFun
    hsTestModuleTestsFun =
      TestsFun moduleName $ concatMap getTestsFunNames decls

getFfiFunctions :: IncludeFile -> CFunPrefix -> Hs.Decl -> [FfiFunction]
getFfiFunctions includeFile cFunPrefix = \case
    Hs.DeclDefineInstance instanceDecl -> case instanceDecl of
      Hs.InstanceStorable Hs.Struct{..} _storableInstance ->
        [ FfiSizeof   includeFile cFunPrefix structName
        , FfiAlignof  includeFile cFunPrefix structName
        , FfiGenSeqHs includeFile cFunPrefix structName
        , FfiGenSeqC  includeFile cFunPrefix structName
        , FfiPreturb  includeFile cFunPrefix structName
        ]
      _otherwise -> []
    _otherwise -> []

getOrphanInstances :: Hs.Decl -> [OrphanInstance]
getOrphanInstances = \case
    Hs.DeclDefineInstance instanceDecl -> case instanceDecl of
      Hs.InstanceStorable Hs.Struct{..} _storableInstance ->
        let fieldNames = Hs.fieldName <$> Vec.toList structFields
        in  [ ArbitraryInstance     structName structConstr fieldNames
            , GenSeqInstance        structName structConstr fieldNames
            , PreturbInstance       structName structConstr fieldNames
            , SameSemanticsInstance structName              fieldNames
            ]
      _otherwise -> []
    _otherwise -> []

getTypeTests :: Hs.Decl -> [TypeTest]
getTypeTests = \case
    Hs.DeclDefineInstance instanceDecl -> case instanceDecl of
      Hs.InstanceStorable Hs.Struct{..} _storableInstance ->
        [TypeTest structName]
      _otherwise -> []
    _otherwise -> []

getTestsFunNames :: Hs.Decl -> [HsName NsTypeConstr]
getTestsFunNames = \case
    Hs.DeclDefineInstance instanceDecl -> case instanceDecl of
      Hs.InstanceStorable Hs.Struct{..} _storableInstance -> [structName]
      _otherwise -> []
    _otherwise -> []

{-------------------------------------------------------------------------------
  AST
-------------------------------------------------------------------------------}

type Import      = (ModuleName, ImportAbbr)
type ImportAbbr  = String
type IncludeFile = String
type ModuleName  = String
type Pragma      = String

data HsTestModule = HsTestModule {
      hsTestModulePragmas         :: [Pragma]
    , hsTestModuleName            :: ModuleName
    , hsTestModuleImports         :: [Import]
    , hsTestModuleFfiFunctions    :: [FfiFunction]
    , hsTestModuleOrphanInstances :: [OrphanInstance]
    , hsTestModuleTypeTests       :: [TypeTest]
    , hsTestModuleTestsFun        :: TestsFun
    }

data FfiFunction =
    FfiSizeof   IncludeFile CFunPrefix (HsName NsTypeConstr)
  | FfiAlignof  IncludeFile CFunPrefix (HsName NsTypeConstr)
  | FfiGenSeqHs IncludeFile CFunPrefix (HsName NsTypeConstr)
  | FfiGenSeqC  IncludeFile CFunPrefix (HsName NsTypeConstr)
  | FfiPreturb  IncludeFile CFunPrefix (HsName NsTypeConstr)

data OrphanInstance =
    ArbitraryInstance     (HsName NsTypeConstr) (HsName NsConstr) [HsName NsVar]
  | GenSeqInstance        (HsName NsTypeConstr) (HsName NsConstr) [HsName NsVar]
  | PreturbInstance       (HsName NsTypeConstr) (HsName NsConstr) [HsName NsVar]
  | SameSemanticsInstance (HsName NsTypeConstr)                   [HsName NsVar]

newtype TypeTest = TypeTest (HsName NsTypeConstr)

data TestsFun = TestsFun ModuleName [HsName NsTypeConstr]

newtype HsSpecModule = HsSpecModule ModuleName

data HsMainModule = HsMainModule

{-------------------------------------------------------------------------------
  Pretty printing
-------------------------------------------------------------------------------}

instance Pretty HsTestModule where
  pretty HsTestModule{..} = vsep $
      vcat
        [ hsep ["{-#", string pragma, "#-}"]
        | pragma <- hsTestModulePragmas
        ]
    : hcat ["module ", string hsTestModuleName, " (tests) where"]
    : vcat
        [ hsep ["import qualified", string moduleName, "as", string importAbbr]
        | (moduleName, importAbbr) <- hsTestModuleImports
        ]
    : prettySection "FFI"
    : map pretty hsTestModuleFfiFunctions
    ++ prettySection "Instances"
    : map pretty hsTestModuleOrphanInstances
    ++ prettySection "Tests"
    : map pretty hsTestModuleTypeTests
    ++ [pretty hsTestModuleTestsFun]

instance Pretty FfiFunction where
  pretty = \case
    FfiSizeof   inc pfx name -> prettyFfiSizeof   inc pfx name
    FfiAlignof  inc pfx name -> prettyFfiAlignof  inc pfx name
    FfiGenSeqHs inc pfx name -> prettyFfiGenSeqHs inc pfx name
    FfiGenSeqC  inc pfx name -> prettyFfiGenSeqC  inc pfx name
    FfiPreturb  inc pfx name -> prettyFfiPreturb  inc pfx name

prettyFfiSizeof :: IncludeFile -> CFunPrefix -> HsName NsTypeConstr -> CtxDoc
prettyFfiSizeof inc pfx name =
    hang
      ( hcat
          [ "foreign import capi unsafe \"", string inc, " "
          , string pfx, "_sizeof_", prettyHsName name, "\""
          ]
      )
      2
      (hcat ["sizeof", prettyHsName name, " :: IO FC.CSize"])

prettyFfiAlignof :: IncludeFile -> CFunPrefix -> HsName NsTypeConstr -> CtxDoc
prettyFfiAlignof inc pfx name =
    hang
      ( hcat
          [ "foreign import capi unsafe \"", string inc, " "
          , string pfx, "_alignof_", prettyHsName name, "\""
          ]
      )
      2
      (hcat ["alignof", prettyHsName name, " :: IO FC.CSize"])

prettyFfiGenSeqHs :: IncludeFile -> CFunPrefix -> HsName NsTypeConstr -> CtxDoc
prettyFfiGenSeqHs inc pfx' name' = vsep
    [ hang
        ( hcat ["genSeqHs", name, " :: X.", name, " -> IO Bool"]
            $$ hcat ["genSeqHs", name, " x = F.alloca $ \\ptr -> do"]
        )
        4
        ( "F.poke ptr x"
            $$ hcat ["(/= 0) <$> ", pfx, "_genseqhs_", name, " ptr"]
        )
    , hang
        ( hcat
            [ "foreign import capi unsafe \"", string inc, " "
            , pfx, "_genseqhs_", name, "\""
            ]
        )
        2
        ( hcat
            [pfx, "_genseqhs_", name, " :: F.Ptr X.", name, " -> IO FC.CBool"]
        )
    ]
  where
    pfx, name :: CtxDoc
    pfx  = string pfx'
    name = prettyHsName name'

prettyFfiGenSeqC :: IncludeFile -> CFunPrefix -> HsName NsTypeConstr -> CtxDoc
prettyFfiGenSeqC inc pfx' name' = vsep
    [ hang
        ( hcat ["genSeqC", name, " :: IO X.", name]
            $$ hcat ["genSeqC", name, " x = F.alloca $ \\ptr -> do"]
        )
        4
        ( hcat [pfx, "_genseqc_", name, " ptr"]
            $$ "F.peek ptr"
        )
    , hang
        ( hcat
            [ "foreign import capi unsafe \"", string inc, " "
            , pfx, "_genseqc_", name, "\""
            ]
        )
        2
        (hcat [pfx, "_genseqc_", name, " :: F.Ptr X.", name, " -> IO ()"])
    ]
  where
    pfx, name :: CtxDoc
    pfx  = string pfx'
    name = prettyHsName name'

prettyFfiPreturb :: IncludeFile -> CFunPrefix -> HsName NsTypeConstr -> CtxDoc
prettyFfiPreturb inc pfx' name' = vsep
    [ hang
        ( hcat ["preturb", name, " :: FC.CLong -> X.", name, " -> IO X.", name]
            $$ hcat ["preturb", name, " size source ="]
        )
        4
        ( hang "F.alloca $ \\sourcePtr ->" 2
            . hang "F.alloca $ \\targetPtr -> do" 2
            $ vcat
                [ "F.poke sourcePtr source"
                , hcat [pfx, "_preturb_", name, " size sourcePtr targetPtr"]
                , "F.peek targetPtr"
                ]
        )
    , hang
        ( hcat
            [ "foreign import capi unsafe \"", string inc, " "
            , pfx, "_preturb_", name, "\""
            ]
        )
        2
        ( hcat
            [ pfx, "_preturb_", name, " :: FC.CLong -> F.Ptr X.", name
            , " -> F.Ptr X.", name, " -> IO ()"
            ]
        )
    ]
  where
    pfx, name :: CtxDoc
    pfx  = string pfx'
    name = prettyHsName name'

instance Pretty OrphanInstance where
  pretty = \case
    ArbitraryInstance typeName cnstName fieldNames ->
      prettyArbitraryInstance typeName cnstName fieldNames
    GenSeqInstance typeName cnstName fieldNames ->
      prettyGenSeqInstance typeName cnstName fieldNames
    PreturbInstance typeName cnstName fieldNames ->
      prettyPreturbInstance typeName cnstName fieldNames
    SameSemanticsInstance typeName fieldNames ->
      prettySameSemanticsInstance typeName fieldNames

prettyArbitraryInstance ::
     HsName NsTypeConstr
  -> HsName NsConstr
  -> [HsName NsVar]
  -> CtxDoc
prettyArbitraryInstance typeName' cnstName' fieldNames' =
    hangs
      (hcat ["instance QC.Arbitrary X.", typeName, " where"])
      2
      [ hang "arbitrary = do" 2 . vcat $
          [fieldName <+> "<- QC.arbitrary" | fieldName <- fieldNames]
            ++ [hcat ["pure X.", cnstName, "{..}"]]
      , hang (hcat ["shrink X.", cnstName, "{..} ="]) 2 $
          vlist '[' ']' shrinkList
      ]
  where
    typeName, cnstName :: CtxDoc
    typeName = prettyHsName typeName'
    cnstName = prettyHsName cnstName'

    fieldNames :: [CtxDoc]
    fieldNames = prettyHsName <$> fieldNames'

    shrinkList :: [CtxDoc]
    shrinkList =
      [ hsep $
            hcat ["X.", cnstName]
          : [ if n `Bits.testBit` i then fieldName else "RepZero.repZero"
            | (i, fieldName) <- zip [0..] fieldNames
            ]
      | n <- reverse @Word [0 .. 2 ^ length fieldNames - 2]
      ]

prettyGenSeqInstance ::
     HsName NsTypeConstr
  -> HsName NsConstr
  -> [HsName NsVar]
  -> CtxDoc
prettyGenSeqInstance typeName' cnstName' fieldNames' =
    hang
      (hcat ["instance GenSeq.GenSeq X.", typeName, " where"])
      2
      ( hang "genSeq n =" 2 . vcat $
             [ if i == 0
                 then hcat ["let ", fieldName, " = GenSeq.genSeq n"]
                 else nest 4 $ hcat
                        [ fieldName, " = GenSeq.genSeq (n + "
                        , showToCtxDoc @Word i, ")"
                        ]
             | (i, fieldName) <- zip [0..] fieldNames
             ]
          ++ [hcat ["in  X.", cnstName, "{..}"]]
      )
  where
    typeName, cnstName :: CtxDoc
    typeName = prettyHsName typeName'
    cnstName = prettyHsName cnstName'

    fieldNames :: [CtxDoc]
    fieldNames = prettyHsName <$> fieldNames'

prettyPreturbInstance ::
     HsName NsTypeConstr
  -> HsName NsConstr
  -> [HsName NsVar]
  -> CtxDoc
prettyPreturbInstance typeName' cnstName' fieldNames' =
    hang
      (hcat ["instance Preturb.Preturb X.", typeName, " where"])
      2
      ( hang
          (hcat ["preturb size X.", cnstName, "{..} = X.", cnstName])
          2
          ( vlist '{' '}'
              [ hcat [fieldName, " = Preturb.preturb size ", fieldName]
              | fieldName <- fieldNames
              ]
          )
      )
  where
    typeName, cnstName :: CtxDoc
    typeName = prettyHsName typeName'
    cnstName = prettyHsName cnstName'

    fieldNames :: [CtxDoc]
    fieldNames = prettyHsName <$> fieldNames'

prettySameSemanticsInstance ::
     HsName NsTypeConstr
  -> [HsName NsVar]
  -> CtxDoc
prettySameSemanticsInstance typeName' fieldNames' =
    hang
      (hcat ["instance SameSemantics.SameSemantics X.", typeName, " where"])
      2
      ( hang "sameSemantics l r = and" 2 $ vlist '[' ']'
          [ hcat ["SameSemantics.sameSemanticsOn X.", fieldName, " l r"]
          | fieldName <- fieldNames
          ]
      )
  where
    typeName :: CtxDoc
    typeName = prettyHsName typeName'

    fieldNames :: [CtxDoc]
    fieldNames = prettyHsName <$> fieldNames'

instance Pretty TypeTest where
  pretty (TypeTest name) = prettyTypeTest name

prettyTypeTest :: HsName NsTypeConstr -> CtxDoc
prettyTypeTest name' =
    hang
      ( hcat ["test", name, " :: Tasty.TestTree"]
          $$ hcat ["test", name, " = Tasty.testGroup \"", name, "\""]
      )
      2
      ( vlist '[' ']'
          [ hang "THU.testCase Storable.nameHsSizeOfXEqCSizeOfX $" 2 $
              hcat
                [ "Storable.assertHsSizeOfXEqCSizeOfX @X.", name
                , " Proxy.Proxy sizeof", name
                ]
          , hang "THU.testCase Storable.nameHsAlignOfXEqCAlignOfX $" 2 $
              hcat
                [ "Storable.assertHsAlignOfXEqCAlignOfX @X.", name
                , " Proxy.Proxy alignof", name
                ]
          , hang "THU.testProperty Storable.namePokePeekXSameSemanticsX $" 2 $
              hcat ["Storable.prop_PokePeekXSameSemanticsX @X.", name]
          , hang "THU.testCase GenSeq.nameHsGenSeq1CEq $" 2 $
              hcat ["GenSeq.assertHsGenSeq1CEq genSeqHs", name]
          , hang "THU.testCase GenSeq.nameHsGenSeq1SameSemanticsCGenSeq1 $" 2 $
              hcat ["GenSeq.assertHsGenSeq1SameSemanticsCGenSeq1 genSeqC", name]
          , hang "TQC.testProperty Preturb.nameHsPreturbNXSameSemanticsCPreturbNX $" 2 $
              hcat
                [ "Preturb.prop_HsPreturbNXSameSemanticsCPreturbNX preturb"
                , name
                ]
          ]
      )
  where
    name :: CtxDoc
    name = prettyHsName name'

instance Pretty TestsFun where
  pretty (TestsFun moduleName typeNames) = prettyTestsFun moduleName typeNames

prettyTestsFun :: ModuleName -> [HsName NsTypeConstr] -> CtxDoc
prettyTestsFun moduleName typeNames =
    hang
      ( "tests :: Tasty.TestTree"
          $$ hcat ["tests = Tasty.testGroup \"", string moduleName, "\""]
      )
      4
      ( vlist '[' ']'
          [ "test" >< prettyHsName typeName
          | typeName <- typeNames
          ]
      )

instance Pretty HsSpecModule where
  pretty (HsSpecModule moduleName) = vsep
    [ "module Spec (tests) where"
    , "import Test.Tasty (TestTree, testGroup)"
    , "import qualified" <+> string moduleName
    , vcat
        [ "tests :: TestTree"
        , "tests = testGroup \"test-hs-bindgen\""
        , nest 4 $ vlist '[' ']' [string moduleName >< ".tests"]
        ]
    ]

instance Pretty HsMainModule where
  pretty HsMainModule = vsep
    [ "module Main (main) where"
    , "import Test.Tasty (defaultMain)"
    , "import qualified Spec"
    , vcat
        [ "main :: IO ()"
        , "main = defaultMain Spec.tests"
        ]
    ]

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

prettySection :: String -> CtxDoc
prettySection label = renderedLines $ \w ->
    let rule = List.replicate (w - 1) '-'
    in  ['{' : rule, "  " ++ label, rule ++ "}"]
-}
