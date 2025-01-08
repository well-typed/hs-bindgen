module HsBindgen.GenTests.C (
    genTestsC
  ) where

import Data.Char qualified as Char
import Data.List qualified as List
import Data.Text qualified as T
import Data.Vec.Lazy qualified as Vec
import System.FilePath qualified as FilePath

import HsBindgen.C.AST qualified as C
import HsBindgen.C.AST.Name
import HsBindgen.GenTests.Internal
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type qualified as HsT
import Text.SimplePrettyPrint

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

-- | Generate C test header and source files
genTestsC ::
     FilePath  -- ^ C test header file path
  -> FilePath  -- ^ C test source file path
  -> Int       -- ^ Maximum line length
  -> FilePath  -- ^ C header path
  -> [Hs.Decl] -- ^ Declarations
  -> IO ()
genTestsC cTestHeaderPath cTestSourcePath lineLength cHeaderPath decls = do
    writeFile cTestHeaderPath $ renderPretty ctx CTestHeader{..}
    writeFile cTestSourcePath $ renderPretty ctx CTestSource{..}
  where
    ctx :: Context
    ctx = mkContext lineLength

    cTestHeaderFilename, cHeaderFilename :: FilePath
    cTestHeaderFilename = FilePath.takeFileName cTestHeaderPath
    cHeaderFilename     = FilePath.takeFileName cHeaderPath

    cFunPrefix :: CFunPrefix
    cFunPrefix = getCFunPrefix cTestHeaderFilename

    cTestHeaderIncludeGuard :: IncludeGuard
    cTestHeaderIncludeGuard = getIncludeGuard cTestHeaderFilename

    cTestHeaderUsrIncludes, cTestHeaderSysIncludes :: [IncludeFile]
    cTestHeaderUsrIncludes = [cHeaderFilename]
    cTestHeaderSysIncludes = ["stdbool.h", "stddef.h"]

    cTestHeaderDecls :: [CTestDecl]
    cTestHeaderDecls = concatMap (getTestHeaderDecls cFunPrefix) decls

    cTestSourceUsrIncludes, cTestSourceSysIncludes :: [IncludeFile]
    cTestSourceUsrIncludes = List.sort
      ["hs_bindgen_testlib.h" , cHeaderFilename , cTestHeaderFilename]
    cTestSourceSysIncludes = ["stdalign.h", "stdbool.h", "stddef.h"]

    cTestSourceDefns :: [CTestDefn]
    cTestSourceDefns = concatMap (getTestSourceDefns cFunPrefix) decls

getTestHeaderDecls :: CFunPrefix -> Hs.Decl -> [CTestDecl]
getTestHeaderDecls cFunPrefix = \case
    Hs.DeclData{} -> []
    Hs.DeclEmpty{} -> []
    Hs.DeclNewtype{} -> []
    Hs.DeclPatSyn{} -> []
    Hs.DeclInstance instanceDecl -> case instanceDecl of
      Hs.InstanceStorable Hs.Struct{..} _storableInstance ->
        case getStructCTypeSpelling structOrigin of
          Just cts ->
            [ CTestSizeofDecl   cFunPrefix structName
            , CTestAlignofDecl  cFunPrefix structName
            , CTestGenseqhsDecl cFunPrefix structName cts
            , CTestGenseqcDecl  cFunPrefix structName cts
            , CTestPreturbDecl  cFunPrefix structName cts
            ]
          Nothing -> []
      Hs.InstanceHasFLAM {} -> []
    Hs.DeclNewtypeInstance{} -> []
    Hs.DeclForeignImport{} -> []
    Hs.DeclVar{} -> []

getTestSourceDefns :: CFunPrefix -> Hs.Decl -> [CTestDefn]
getTestSourceDefns cFunPrefix = \case
    Hs.DeclData{} -> []
    Hs.DeclEmpty{} -> []
    Hs.DeclNewtype{} -> []
    Hs.DeclPatSyn{} -> []
    Hs.DeclInstance instanceDecl -> case instanceDecl of
      Hs.InstanceStorable Hs.Struct{..} _storableInstance ->
        case getStructCTypeSpelling structOrigin of
          Just cts ->
            let fieldPs = getFieldP <$> Vec.toList structFields
            in  [ CTestSizeofDefn   cFunPrefix structName cts
                , CTestAlignofDefn  cFunPrefix structName cts
                , CTestGenseqhsDefn cFunPrefix structName cts fieldPs
                , CTestGenseqcDefn  cFunPrefix structName cts fieldPs
                , CTestPreturbDefn  cFunPrefix structName cts fieldPs
                ]
          Nothing -> []
      Hs.InstanceHasFLAM {} -> []
    Hs.DeclNewtypeInstance{} -> []
    Hs.DeclForeignImport{} -> []
    Hs.DeclVar{} -> []

getStructCTypeSpelling :: Hs.StructOrigin -> Maybe CTypeSpelling
getStructCTypeSpelling = \case
    Hs.StructOriginStruct C.Struct{..} -> case structDeclPath of
      C.DeclPathStruct declName _declPath -> case declName of
        C.DeclNameNone -> Nothing
        C.DeclNameTag cName -> Just $ "struct " ++ T.unpack (C.getCName cName)
        C.DeclNameTypedef cName -> Just $ T.unpack (C.getCName cName)
      _otherwise -> Nothing
    Hs.StructOriginEnum{} -> Nothing

getFieldP :: Hs.Field -> FieldP
getFieldP Hs.Field{..} = (cName, hsTypeName)
  where
    cName :: CName
    cName = case fieldOrigin of
      Hs.FieldOriginNone -> error "unexpected FieldOriginNone in struct"
      Hs.FieldOriginStructField cStructField -> C.fieldName cStructField

    hsTypeName :: HsTypeName
    hsTypeName = case fieldType of
      Hs.HsType{} -> error "not supported: HsType"
      Hs.HsPrimType hsPrimType -> case hsPrimType of
        HsT.HsPrimVoid    -> error "not supported: HsPrimVoid"
        HsT.HsPrimCChar   -> "CChar"
        HsT.HsPrimCSChar  -> "CSChar"
        HsT.HsPrimCUChar  -> "CUChar"
        HsT.HsPrimCInt    -> "CInt"
        HsT.HsPrimCUInt   -> "CUInt"
        HsT.HsPrimCShort  -> "CShort"
        HsT.HsPrimCUShort -> "CUShort"
        HsT.HsPrimCLong   -> "CLong"
        HsT.HsPrimCULong  -> "CULong"
        HsT.HsPrimCLLong  -> "CLLong"
        HsT.HsPrimCULLong -> "CULLong"
        HsT.HsPrimCBool   -> "CBool"
        HsT.HsPrimCFloat  -> "CFloat"
        HsT.HsPrimCDouble -> "CDouble"
      Hs.HsTypRef{} -> error "not supported: HsTypRef"
      Hs.HsConstArray{} -> error "not supported: HsConstArray"
      Hs.HsPtr{} -> error "not supported: HsPtr"
      Hs.HsFunPtr{} -> error "not supported: HsFunPtr"
      Hs.HsIO{} -> error "not supported: HsIO"
      Hs.HsFun{} -> error "not supported: HsFun"

{-------------------------------------------------------------------------------
  AST
-------------------------------------------------------------------------------}

type CTypeSpelling = String
type FieldP        = (CName, HsTypeName)
type HsTypeName    = String
type IncludeFile   = String
type IncludeGuard  = String

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
    | CTestGenseqhsDefn CFunPrefix (HsName NsTypeConstr) CTypeSpelling [FieldP]
    | CTestGenseqcDefn  CFunPrefix (HsName NsTypeConstr) CTypeSpelling [FieldP]
    | CTestPreturbDefn  CFunPrefix (HsName NsTypeConstr) CTypeSpelling [FieldP]

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
        [string cts <+> "const *const"]
        ";"
    CTestGenseqcDecl pfx name cts ->
      prettyFun
        (hcat ["void ", string pfx, "_genseqc_", prettyHsName name])
        [string cts <+> "*const"]
        ";"
    CTestPreturbDecl pfx name cts ->
      prettyFun
        (hcat ["void ", string pfx, "_preturb_", prettyHsName name])
        [ "long"
        , string cts <+> "const *const"
        , string cts <+> "*const"
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
      , nest 2 $ hcat ["return sizeof(", string cts, ");"]
      , "}"
      ]
    CTestAlignofDefn pfx name cts -> vcat
      [ prettyFun
          (hcat ["size_t ", string pfx, "_alignof_", prettyHsName name])
          ["void"]
          " {"
      , nest 2 $ hcat ["return alignof(", string cts, ");"]
      , "}"
      ]
    CTestGenseqhsDefn pfx name cts fieldPairs ->
      let numFieldPairs = length fieldPairs
      in  vcat
            [ prettyFun
                (hcat ["bool ", string pfx, "_genseqhs_", prettyHsName name])
                [string cts <+> "const *const source"]
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
          [string cts <+> "*const target"]
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
          , string cts <+> "const *const source"
          , string cts <+> "*const target"
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
  Auxillary functions
-------------------------------------------------------------------------------}

getIncludeGuard ::
     FilePath  -- ^ C test header filename
  -> String
getIncludeGuard = List.map aux
  where
    aux :: Char -> Char
    aux c
      | Char.isAlphaNum c = Char.toUpper c
      | otherwise         = '_'

prettyCName :: CName -> CtxDoc
prettyCName = string . T.unpack . getCName

prettyFun ::
     CtxDoc    -- ^ Part before opening paren
  -> [CtxDoc]  -- ^ Parameters/arguments
  -> CtxDoc    -- ^ Part after closing paren
  -> CtxDoc
prettyFun lDoc args rDoc = lDoc >< "(" >< fsep
    [ if idx < numArgs then arg >< "," else arg >< ")" >< rDoc
    | (idx, arg) <- zip [1..] args
    ]
  where
    numArgs :: Int
    numArgs = length args
