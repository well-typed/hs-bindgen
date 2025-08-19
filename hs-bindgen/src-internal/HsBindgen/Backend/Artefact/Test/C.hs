module HsBindgen.Backend.Artefact.Test.C (
    genTestsC
  ) where

--import Data.Char qualified as Char
--import Data.List qualified as List
--import Data.Text qualified as T
--import Data.Typeable (typeOf)
--import Data.Vec.Lazy qualified as Vec
--import System.FilePath qualified as FilePath
--import System.FilePath.Posix qualified as Posix


--import HsBindgen.Errors
--import HsBindgen.C.AST qualified as C
--import HsBindgen.C.AST.Name
import HsBindgen.Errors
import HsBindgen.Frontend.RootHeader
--import HsBindgen.Backend.Artefact.Test.Internal
--    ( CFunPrefix, getCFunPrefix, prettyHsName )
import HsBindgen.Backend.Hs.AST qualified as Hs
--import HsBindgen.Backend.Hs.AST.Name
--import HsBindgen.Backend.Hs.AST.Type qualified as HsT
--import Text.SimplePrettyPrint

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

-- | Generate C test header and source files
genTestsC ::
     FilePath             -- ^ C test header file path
  -> FilePath             -- ^ C test source file path
  -> [HashIncludeArg] -- ^ C header paths
  -> [Hs.Decl]            -- ^ Declarations
  -> IO ()
genTestsC = throwPure_TODO 22 "generate test suite"

{-
genTestsC cTestHeaderPath cTestSourcePath lineLength cHeaderPath decls = do
    writeFile cTestHeaderPath $ renderPretty ctx CTestHeader{..}
    writeFile cTestSourcePath $ renderPretty ctx CTestSource{..}
  where
    ctx :: Context
    ctx = mkContext lineLength

    cTestHeaderFilename, cHeaderFilename :: FilePath
    cTestHeaderFilename = FilePath.takeFileName cTestHeaderPath
    cHeaderFilename = Posix.takeFileName $ getHashIncludeArg cHeaderPath

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
      ["hs_bindgen_test_runtime.h" , cHeaderFilename , cTestHeaderFilename]
    cTestSourceSysIncludes = ["stdalign.h", "stdbool.h", "stddef.h"]

    cTestSourceDefns :: [CTestDefn]
    cTestSourceDefns = concatMap (getTestSourceDefns cFunPrefix) decls

getTestHeaderDecls :: CFunPrefix -> Hs.Decl -> [CTestDecl]
getTestHeaderDecls cFunPrefix = \case
    Hs.DeclDefineInstance instanceDecl -> case instanceDecl of
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
      _otherwise -> []
    _otherwise -> []

getTestSourceDefns :: CFunPrefix -> Hs.Decl -> [CTestDefn]
getTestSourceDefns cFunPrefix = \case
    Hs.DeclDefineInstance instanceDecl -> case instanceDecl of
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
      _otherwise -> []
    _otherwise -> []

getStructCTypeSpelling :: Hs.StructOrigin -> Maybe CTypeSpelling
getStructCTypeSpelling = \case
    Hs.StructOriginStruct C.Struct{..} -> case structDeclPath of
      C.DeclPathAnon ctxt ->
        case ctxt of
          C.DeclPathCtxtTypedef typedefName ->
            Just $ T.unpack (C.getName typedefName)
          _otherwise ->
            Nothing
      C.DeclPathName cName ->
          Just $ "struct " ++ T.unpack (C.getName cName)
    Hs.StructOriginEnum{} -> Nothing

getFieldP :: Hs.Field -> FieldP
getFieldP Hs.Field{..} = (cName, hsTypeName)
  where
    cName :: C.Name
    cName = case fieldOrigin of
      Hs.FieldOriginNone -> panicPure "unexpected FieldOriginNone in struct"
      Hs.FieldOriginStructField cStructField -> C.fieldName cStructField

    hsTypeName :: HsTypeName
    hsTypeName = case fieldType of
      Hs.HsPrimType hsPrimType -> case hsPrimType of
        HsT.HsPrimCChar    -> "CChar"
        HsT.HsPrimCSChar   -> "CSChar"
        HsT.HsPrimCUChar   -> "CUChar"
        HsT.HsPrimCInt     -> "CInt"
        HsT.HsPrimCUInt    -> "CUInt"
        HsT.HsPrimCShort   -> "CShort"
        HsT.HsPrimCUShort  -> "CUShort"
        HsT.HsPrimCLong    -> "CLong"
        HsT.HsPrimCULong   -> "CULong"
        HsT.HsPrimCLLong   -> "CLLong"
        HsT.HsPrimCULLong  -> "CULLong"
        HsT.HsPrimCBool    -> "CBool"
        HsT.HsPrimCFloat   -> "CFloat"
        HsT.HsPrimCDouble  -> "CDouble"
        HsT.HsPrimCPtrDiff -> "CPtrdiff"
        HsT.HsPrimCSize    -> "CSize"
        x -> throwPure_TODO 447 $ "not supported: " ++ show (typeOf x)
      x -> throwPure_TODO 447 $ "not supported: " ++ show (typeOf x)

{-------------------------------------------------------------------------------
  AST
-------------------------------------------------------------------------------}

type CTypeSpelling = String
type FieldP        = (C.Name, HsTypeName)
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

prettyCName :: C.Name -> CtxDoc
prettyCName = string . T.unpack . C.getName

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
-}
