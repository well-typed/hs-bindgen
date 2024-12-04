module HsBindgen.GenTests.Internal where

import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import System.FilePath qualified as FilePath

import HsBindgen.C.AST qualified as C
import HsBindgen.Hs.AST.Name
import HsBindgen.Imports
import HsBindgen.SHs.AST qualified as SHs
import Text.SimplePrettyPrint

{-------------------------------------------------------------------------------
  C Function Prefix
-------------------------------------------------------------------------------}

type CFunPrefix = String

getCFunPrefix ::
     FilePath  -- ^ C test header filename
  -> CFunPrefix
getCFunPrefix = List.map aux . FilePath.dropExtension
  where
    aux :: Char -> Char
    aux c
      | Char.isAlphaNum c = Char.toLower c
      | otherwise         = '_'

{-------------------------------------------------------------------------------
  TestRecord
-------------------------------------------------------------------------------}

data TestRecord =
    TestStruct {
        testStructDecl       :: Either SHs.Newtype SHs.Record
      , testStructInstances  :: Set SHs.Global
      , testStructC          :: C.Struct
      }
  | TestEnum {
        testEnumDecl         :: SHs.Newtype
      , testEnumInstances    :: Set SHs.Global
      , testEnumC            :: C.Enu
      }
  | TestTypedef {
        testTypedefDecl      :: SHs.Newtype
      , testTypedefInstances :: Set SHs.Global
      , testTypedefC         :: C.Typedef
      }
  deriving Show

isCReferenceable :: TestRecord -> Bool
isCReferenceable = not . T.isSuffixOf ")" .  \case
    TestStruct{..}  -> C.structTypeSpelling testStructC
    TestEnum{..}    -> C.enumTypeSpelling testEnumC
    TestTypedef{..} -> C.typedefTypeSpelling testTypedefC

getNameAndSpelling :: TestRecord -> (HsName NsTypeConstr, Text)
getNameAndSpelling = \case
    TestStruct{..} ->
      ( either SHs.newtypeName SHs.dataType testStructDecl
      , C.structTypeSpelling testStructC
      )
    TestEnum{..} -> (SHs.newtypeName testEnumDecl, C.enumTypeSpelling testEnumC)
    TestTypedef{..} ->
      (SHs.newtypeName testTypedefDecl, C.typedefTypeSpelling testTypedefC)

getTestRecords ::
     C.Header
  -> [SHs.SDecl]
  -> [TestRecord]
getTestRecords cHeader = Map.elems . Map.mapMaybeWithKey aux . getTestRecords'
  where
    cDeclMap :: Map Text C.Decl
    cDeclMap = indexCHeader cHeader

    aux :: HsName NsTypeConstr -> TestRecord' -> Maybe TestRecord
    aux name TestRecord'{..} = case testRecordDecls' of
      [Left nt] -> do
        typeSpelling <- SHs.newtypeTypeSpelling nt
        cDecl <- Map.lookup typeSpelling cDeclMap
        case cDecl of
          C.DeclStruct struct -> Just TestStruct {
              testStructDecl      = Left nt
            , testStructInstances = testRecordInstances'
            , testStructC         = struct
            }
          C.DeclOpaqueStruct{} -> Nothing
          C.DeclTypedef tdef -> Just TestTypedef {
              testTypedefDecl      = nt
            , testTypedefInstances = testRecordInstances'
            , testTypedefC         = tdef
            }
          C.DeclEnum enu -> Just TestEnum {
              testEnumDecl      = nt
            , testEnumInstances = testRecordInstances'
            , testEnumC         = enu
            }
          C.DeclMacro{} -> Nothing
      [Right rec] -> do
        typeSpelling <- SHs.dataTypeSpelling rec
        cDecl <- Map.lookup typeSpelling cDeclMap
        case cDecl of
          C.DeclStruct struct -> Just TestStruct {
              testStructDecl      = Right rec
            , testStructInstances = testRecordInstances'
            , testStructC         = struct
            }
          C.DeclOpaqueStruct{} -> Nothing
          C.DeclTypedef{} -> fail $ "typedef Record for " ++ show name
          C.DeclEnum{} -> fail $ "enum Record for " ++ show name
          C.DeclMacro{} -> Nothing
      [] -> Nothing
      _otherwise -> fail $ "multiple declarations for " ++ show name

data TestRecord' = TestRecord' {
      testRecordDecls'     :: [Either SHs.Newtype SHs.Record]
    , testRecordInstances' :: Set SHs.Global
    }
  deriving Show

instance Semigroup TestRecord' where
  l <> r = TestRecord' {
      testRecordDecls'     = testRecordDecls' l <> testRecordDecls' r
    , testRecordInstances' = testRecordInstances' l <> testRecordInstances' r
    }

getTestRecords' :: [SHs.SDecl] -> Map (HsName NsTypeConstr) TestRecord'
getTestRecords' = Map.fromListWith (<>) . mapMaybe aux
  where
    aux :: SHs.SDecl -> Maybe (HsName NsTypeConstr, TestRecord')
    aux = \case
      SHs.DVar{} -> Nothing
      SHs.DInst inst -> Just
        ( SHs.instanceType inst
        , TestRecord' {
              testRecordDecls'     = []
            , testRecordInstances' = Set.singleton $ SHs.instanceClass inst
            }
        )
      SHs.DRecord rec -> Just
        ( SHs.dataType rec
        , TestRecord' {
              testRecordDecls'     = [Right rec]
            , testRecordInstances' = Set.empty
            }
        )
      SHs.DNewtype nt -> Just
        ( SHs.newtypeName nt
        , TestRecord' {
              testRecordDecls'     = [Left nt]
            , testRecordInstances' = Set.empty
            }
        )
      SHs.DEmptyData{} -> Nothing
      SHs.DDerivingNewtypeInstance closedType -> case closedType of
        SHs.TApp (SHs.TGlobal instClass) (SHs.TCon instType) -> Just
          ( instType
          , TestRecord' {
                testRecordDecls'     = []
              , testRecordInstances' = Set.singleton instClass
              }
          )
        _otherwise -> Nothing

indexCHeader :: C.Header -> Map Text C.Decl
indexCHeader = foldr mkMap Map.empty . C.headerDecls
  where
    mkMap :: C.Decl -> Map Text C.Decl -> Map Text C.Decl
    mkMap decl acc = case decl of
      C.DeclStruct struct ->
        Map.singleton (C.structTypeSpelling struct) decl <> acc
      C.DeclOpaqueStruct{} -> acc
      C.DeclTypedef tdef ->
        Map.singleton (C.typedefTypeSpelling tdef) decl <> acc
      C.DeclEnum enu ->
        Map.singleton (C.enumTypeSpelling enu) decl <> acc
      C.DeclMacro{} -> acc

{-------------------------------------------------------------------------------
  Pretty printing
-------------------------------------------------------------------------------}

prettyHsName :: HsName ns -> CtxDoc
prettyHsName = string . T.unpack . getHsName
