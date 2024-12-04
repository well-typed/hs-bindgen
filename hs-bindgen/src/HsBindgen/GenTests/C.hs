module HsBindgen.GenTests.C (
    genTestsC
  ) where

import Data.Char qualified as Char
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as T
import System.FilePath qualified as FilePath

import HsBindgen.C.AST.Name
import HsBindgen.C.AST qualified as C
import HsBindgen.GenTests.Internal
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type
import HsBindgen.Imports
import HsBindgen.SHs.AST qualified as SHs
import Text.SimplePrettyPrint

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

genTestsC ::
     FilePath  -- ^ C test header file path
  -> FilePath  -- ^ C test source file path
  -> Int       -- ^ Maximum line length
  -> FilePath  -- ^ C header path
  -> [TestRecord]
  -> IO ()
genTestsC
  cTestHeaderPath
  cTestSourcePath
  lineLength
  cHeaderPath
  testRecords = do
    writeFile cTestHeaderPath $
      renderPretty (mkContext lineLength) CTestHeader{..}
    writeFile cTestSourcePath $
      renderPretty (mkContext lineLength) CTestSource{..}
  where
    cTestHeaderFilename, cHeaderFilename :: FilePath
    cTestHeaderFilename = FilePath.takeFileName cTestHeaderPath
    cHeaderFilename     = FilePath.takeFileName cHeaderPath

    cFunPrefix :: CFunPrefix
    cFunPrefix = getCFunPrefix cTestHeaderFilename

    testRecordsC :: [TestRecord]
    testRecordsC = filter isCReferenceable testRecords

    cTestHeaderIncludeGuard :: IncludeGuard
    cTestHeaderIncludeGuard = getIncludeGuard cTestHeaderFilename

    cTestHeaderUsrIncludes, cTestHeaderSysIncludes :: [IncludeFile]
    cTestHeaderUsrIncludes = [cHeaderFilename]
    cTestHeaderSysIncludes = ["stdbool.h", "stddef.h"]

    cTestHeaderDecls :: [CTestDecl]
    cTestHeaderDecls = concatMap (getTestHeaderDecls cFunPrefix) testRecordsC

    cTestSourceUsrIncludes, cTestSourceSysIncludes :: [IncludeFile]
    cTestSourceUsrIncludes = List.sort
      ["hs_bindgen_testlib.h" , cHeaderFilename , cTestHeaderFilename]
    cTestSourceSysIncludes = ["stdalign.h", "stdbool.h", "stddef.h"]

    cTestSourceDefns :: [CTestDefn]
    cTestSourceDefns = concatMap (getTestSourceDefns cFunPrefix) testRecordsC

getTestHeaderDecls :: CFunPrefix -> TestRecord -> [CTestDecl]
getTestHeaderDecls cFunPrefix testRecord = case testRecord of
    TestStruct{..}
      | Set.member SHs.Storable_Storable testStructInstances ->
          [ CTestSizeofDecl   cFunPrefix name
          , CTestAlignofDecl  cFunPrefix name
          , CTestGenseqhsDecl cFunPrefix name cts
          , CTestGenseqcDecl  cFunPrefix name cts
          , CTestPreturbDecl  cFunPrefix name cts
          ]
      | otherwise -> []
    TestEnum{} -> [] -- TODO
    TestTypedef{} -> [] -- TODO
  where
    name :: HsName NsTypeConstr
    cts  :: CTypeSpelling
    (name, cts) = getNameAndSpelling testRecord

getTestSourceDefns :: CFunPrefix -> TestRecord -> [CTestDefn]
getTestSourceDefns cFunPrefix testRecord = case testRecord of
    TestStruct{..}
      | Set.member SHs.Storable_Storable testStructInstances ->
          let fieldPairs = getFieldPairs testStructC testStructDecl
          in  [ CTestSizeofDefn   cFunPrefix name cts
              , CTestAlignofDefn  cFunPrefix name cts
              , CTestGenseqhsDefn cFunPrefix name cts fieldPairs
              , CTestGenseqcDefn  cFunPrefix name cts fieldPairs
              , CTestPreturbDefn  cFunPrefix name cts fieldPairs
              ]
      | otherwise -> []
    TestEnum{} -> [] -- TODO
    TestTypedef{} -> [] -- TODO
  where
    name :: HsName NsTypeConstr
    cts  :: CTypeSpelling
    (name, cts) = getNameAndSpelling testRecord

getFieldPairs ::
     C.Struct
  -> Either SHs.Newtype SHs.Record
  -> [FieldPair]
getFieldPairs cStruct eNR =
    let cNames = C.fieldName <$> C.structFields cStruct
        hsTypeNames = case eNR of
          Left SHs.Newtype{..} -> [fieldTypeName newtypeField]
          Right SHs.Record{..} -> fieldTypeName <$> dataFields
    in  if length cNames == length hsTypeNames
          then zip cNames hsTypeNames
          else fail $ "field count mismatch: " ++ show eNR
  where
    fieldTypeName :: SHs.Field -> HsTypeName
    fieldTypeName field = case SHs.fieldType field of
      SHs.TGlobal (SHs.PrimType hsPrimType) -> case hsPrimType of
        HsPrimVoid    -> fail "unsupported field type: " ++ show HsPrimVoid
        HsPrimCChar   -> "CChar"
        HsPrimCSChar  -> "CSChar"
        HsPrimCUChar  -> "CUChar"
        HsPrimCInt    -> "CInt"
        HsPrimCUInt   -> "CUInt"
        HsPrimCShort  -> "CShort"
        HsPrimCUShort -> "CUShort"
        HsPrimCLong   -> "CLong"
        HsPrimCULong  -> "CULong"
        HsPrimCLLong  -> "CLLong"
        HsPrimCULLong -> "CULLong"
        HsPrimCBool   -> "CBool"
        HsPrimCFloat  -> "CFloat"
        HsPrimCDouble -> "CDouble"
      fieldType -> fail $ "unsupported field type: " ++ show fieldType

{-------------------------------------------------------------------------------
  AST
-------------------------------------------------------------------------------}

type IncludeGuard  = String
type IncludeFile   = String
type CTypeSpelling = Text
type HsTypeName    = String
type FieldPair     = (CName, HsTypeName)

data CTestHeader = CTestHeader {
      cTestHeaderIncludeGuard :: IncludeGuard
    , cTestHeaderUsrIncludes  :: [IncludeFile]
    , cTestHeaderSysIncludes  :: [IncludeFile]
    , cTestHeaderDecls        :: [CTestDecl]
    }

data CTestDecl =
      CTestSizeofDecl   CFunPrefix (HsName NsTypeConstr)
    | CTestAlignofDecl  CFunPrefix (HsName NsTypeConstr)
    | CTestGenseqhsDecl CFunPrefix (HsName NsTypeConstr) CTypeSpelling
    | CTestGenseqcDecl  CFunPrefix (HsName NsTypeConstr) CTypeSpelling
    | CTestPreturbDecl  CFunPrefix (HsName NsTypeConstr) CTypeSpelling

data CTestSource = CTestSource {
      cTestSourceUsrIncludes :: [IncludeFile]
    , cTestSourceSysIncludes :: [IncludeFile]
    , cTestSourceDefns       :: [CTestDefn]
    }

data CTestDefn =
      CTestSizeofDefn   CFunPrefix (HsName NsTypeConstr) CTypeSpelling
    | CTestAlignofDefn  CFunPrefix (HsName NsTypeConstr) CTypeSpelling
    | CTestGenseqhsDefn CFunPrefix (HsName NsTypeConstr) CTypeSpelling [FieldPair]
    | CTestGenseqcDefn  CFunPrefix (HsName NsTypeConstr) CTypeSpelling [FieldPair]
    | CTestPreturbDefn  CFunPrefix (HsName NsTypeConstr) CTypeSpelling [FieldPair]

{-------------------------------------------------------------------------------
  Pretty printing
-------------------------------------------------------------------------------}

instance Pretty CTestHeader where
  pretty CTestHeader{..} = vsep $
      vcat
        [ "#ifndef" <+> string cTestHeaderIncludeGuard
        , "#define" <+> string cTestHeaderIncludeGuard
        ]
    : vcat ["#include \"" >< string inc >< "\"" | inc <- cTestHeaderUsrIncludes]
    : vcat ["#include <" >< string inc >< ">" | inc <- cTestHeaderSysIncludes]
    : map pretty cTestHeaderDecls
    ++ ["#endif //" <+> string cTestHeaderIncludeGuard]

instance Pretty CTestDecl where
  pretty = \case
    CTestSizeofDecl pfx name ->
      prettyFun
        (hcat ["size_t ", string pfx, "_sizeof_", prettyHsName name])
        ["void"]
        ";"
    CTestAlignofDecl pfx name ->
      prettyFun
        (hcat ["size_t ", string pfx, "_alignof_", prettyHsName name])
        ["void"]
        ";"
    CTestGenseqhsDecl pfx name cts ->
      prettyFun
        (hcat ["bool ", string pfx, "_genseqhs_", prettyHsName name])
        [prettyCTS cts <+> "const *const"]
        ";"
    CTestGenseqcDecl pfx name cts ->
      prettyFun
        (hcat ["void ", string pfx, "_genseqc_", prettyHsName name])
        [prettyCTS cts <+> "*const"]
        ";"
    CTestPreturbDecl pfx name cts ->
      prettyFun
        (hcat ["void ", string pfx, "_preturb_", prettyHsName name])
        [ "long"
        , prettyCTS cts <+> "const *const"
        , prettyCTS cts <+> "*const"
        ]
        ";"

instance Pretty CTestSource where
  pretty CTestSource{..} = vsep $
      vcat ["#include \"" >< string inc >< "\"" | inc <- cTestSourceUsrIncludes]
    : vcat ["#include <" >< string inc >< ">" | inc <- cTestSourceSysIncludes]
    : map pretty cTestSourceDefns

instance Pretty CTestDefn where
  pretty = \case
    CTestSizeofDefn pfx name cts -> vcat
      [ prettyFun
          (hcat ["size_t ", string pfx, "_sizeof_", prettyHsName name])
          ["void"]
          " {"
      , nest 2 $ hcat ["return sizeof(", prettyCTS cts, ");"]
      , "}"
      ]
    CTestAlignofDefn pfx name cts -> vcat
      [ prettyFun
          (hcat ["size_t ", string pfx, "_alignof_", prettyHsName name])
          ["void"]
          " {"
      , nest 2 $ hcat ["return alignof(", prettyCTS cts, ");"]
      , "}"
      ]
    CTestGenseqhsDefn pfx name cts fieldPairs ->
      let numFieldPairs = length fieldPairs
      in  vcat
            [ prettyFun
                (hcat ["bool ", string pfx, "_genseqhs_", prettyHsName name])
                [prettyCTS cts <+> "const *const source"]
                " {"
            , nest 2 $ "return" <+> fsep
                [ hcat
                    [ "source->"
                    , prettyCName cField
                    , " == hsbg_genseq_"
                    , string hsFieldType
                    , "("
                    , showToCtxDoc idx
                    , ")"
                    , if idx == numFieldPairs
                        then ";"
                        else " &&"
                    ]
                | (idx, (cField, hsFieldType)) <- zip [1..] fieldPairs
                ]
            , "}"
            ]
    CTestGenseqcDefn pfx name cts fieldPairs -> vcat $
        prettyFun
          (hcat ["void ", string pfx, "_genseqc_", prettyHsName name])
          [prettyCTS cts <+> "*const target"]
          " {"
      : [ nest 2 $ hcat
            [ "target->"
            , prettyCName cField
            , " = hsbg_genseq_"
            , string hsFieldType
            , "("
            , showToCtxDoc @Int idx
            , ");"
            ]
        | (idx, (cField, hsFieldType)) <- zip [1..] fieldPairs
        ]
      ++ ["}"]
    CTestPreturbDefn pfx name cts fieldPairs -> vcat $
        prettyFun
          (hcat ["void ", string pfx, "_preturb_", prettyHsName name])
          [ "long size"
          , prettyCTS cts <+> "const *const source"
          , prettyCTS cts <+> "*const target"
          ]
          " {"
      : [ nest 2 $ hcat
            [ "target->"
            , prettyCName cField
            , " = hsbg_preturb_"
            , string hsFieldType
            , "(size, source->"
            , prettyCName cField
            , ");"
            ]
        | (cField, hsFieldType) <- fieldPairs
        ]
      ++ ["}"]

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

getIncludeGuard ::
     FilePath  -- ^ C test header filename
  -> IncludeGuard
getIncludeGuard = List.map aux
  where
    aux :: Char -> Char
    aux c
      | Char.isAlphaNum c = Char.toUpper c
      | otherwise         = '_'

prettyCTS :: CTypeSpelling -> CtxDoc
prettyCTS = string . T.unpack

prettyCName :: CName -> CtxDoc
prettyCName = string . T.unpack . getCName

prettyFun ::
     CtxDoc    -- ^ part before opening paren
  -> [CtxDoc]  -- ^ parameters/arguments
  -> CtxDoc    -- ^ part after closing paren
  -> CtxDoc
prettyFun lDoc args rDoc = lDoc >< "(" >< fsep
    [ if idx < numArgs then arg >< "," else arg >< ")" >< rDoc
    | (idx, arg) <- zip [1..] args
    ]
  where
    numArgs :: Int
    numArgs = length args
