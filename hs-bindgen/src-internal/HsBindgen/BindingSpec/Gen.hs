-- | Binding specification generation
--
-- Intended for qualified import.
--
-- > import HsBindgen.BindingSpec.Gen qualified as BindingSpec
module HsBindgen.BindingSpec.Gen (
    -- * Public API
    genBindingSpec

    -- * Internal API
  , genBindingSpecYaml
  ) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as HsOrigin
import HsBindgen.BindingSpec.Private.Common
import HsBindgen.BindingSpec.Private.V1 (UnresolvedBindingSpec)
import HsBindgen.BindingSpec.Private.V1 qualified as BindingSpec
import HsBindgen.Errors
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

-- | Generate binding specification
--
-- The format is determined by filename extension.  The following formats are
-- supported:
--
-- * YAML (@.yaml@ extension)
-- * JSON (@.json@ extension)
genBindingSpec ::
     Hs.ModuleName
  -> FilePath
  -> GetMainHeaders
  -> Map C.QualName SourcePath
  -> [Hs.Decl]
  -> IO ()
genBindingSpec hsModuleName path getMainHeaders omitTypes =
      BindingSpec.writeFile path
    . genBindingSpec' hsModuleName getMainHeaders omitTypes

{-------------------------------------------------------------------------------
  Internal API (for tests)
-------------------------------------------------------------------------------}

-- | Generate binding specification
genBindingSpecYaml ::
     Hs.ModuleName
  -> GetMainHeaders
  -> Map C.QualName SourcePath
  -> [Hs.Decl]
  -> ByteString
genBindingSpecYaml hsModuleName getMainHeaders omitTypes =
      BindingSpec.encodeYaml
    . genBindingSpec' hsModuleName getMainHeaders omitTypes

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

type CTypeSpec = (C.QualName, Set HashIncludeArg, BindingSpec.CTypeSpec)

-- TODO aliases
genBindingSpec' ::
     Hs.ModuleName
  -> GetMainHeaders
  -> Map C.QualName SourcePath
  -> [Hs.Decl]
  -> UnresolvedBindingSpec
genBindingSpec' hsModuleName getMainHeaders omitTypes = foldr aux omitSpec
  where
    omitSpec :: UnresolvedBindingSpec
    omitSpec = BindingSpec.empty {
        BindingSpec.bindingSpecTypes = Map.fromListWith (++) [
            (cQualName, [(getMainHeaders' path, Omit)])
          | (cQualName, path) <- Map.toList omitTypes
          ]
      }

    getMainHeaders' :: SourcePath -> Set HashIncludeArg
    getMainHeaders' =
        either
          (\s -> panicPure ("auxStruct: getMainHeaders: " ++ s))
          (Set.fromList . NonEmpty.toList)
      . getMainHeaders

    aux ::
         Hs.Decl
      -> UnresolvedBindingSpec
      -> UnresolvedBindingSpec
    aux = \case
      Hs.DeclData struct      -> insertType $ auxStruct    struct
      Hs.DeclEmpty edata      -> insertType $ auxEmptyData edata
      Hs.DeclNewtype ntype    -> insertType $ auxNewtype   ntype
      Hs.DeclPatSyn{}         -> id
      Hs.DeclDefineInstance{} -> id
      Hs.DeclDeriveInstance{} -> id
      Hs.DeclForeignImport{}  -> id
      Hs.DeclFunction{}       -> id
      Hs.DeclMacroExpr{}      -> id
      Hs.DeclUnionGetter{}    -> id
      Hs.DeclUnionSetter{}    -> id
      Hs.DeclSimple{}         -> id

    insertType :: CTypeSpec -> UnresolvedBindingSpec -> UnresolvedBindingSpec
    insertType (cQualName, headers, typeSpec) spec = spec {
        BindingSpec.bindingSpecTypes =
          Map.insertWith (++) cQualName [(headers, Require typeSpec)] $
            BindingSpec.bindingSpecTypes spec
      }

    auxStruct :: Hs.Struct n -> CTypeSpec
    auxStruct hsStruct = case Hs.structOrigin hsStruct of
      Nothing -> panicPure "auxStruct: structOrigin is Nothing"
      Just originDecl ->
        let declInfo = HsOrigin.declInfo originDecl
            cQualName = getCQualName declInfo $
              case HsOrigin.declKind originDecl of
                HsOrigin.Struct{} -> C.NameKindTagged C.TagKindStruct
            hsIdentifier = Hs.Identifier $ Hs.getName (Hs.structName hsStruct)
            C.DeclSpec typeSpec' = HsOrigin.declSpec originDecl
            typeSpec = BindingSpec.CTypeSpec {
                cTypeSpecModule     = Just hsModuleName
              , cTypeSpecIdentifier = Just hsIdentifier
              , cTypeSpecInstances  =
                  BindingSpec.cTypeSpecInstances typeSpec'
                    <> mkInstSpecs (Hs.structInstances hsStruct)
              }
        in  (cQualName, getHeaders declInfo, typeSpec)

    auxEmptyData :: Hs.EmptyData -> CTypeSpec
    auxEmptyData edata =
      let originDecl = Hs.emptyDataOrigin edata
          declInfo = HsOrigin.declInfo originDecl
          cQualName = getCQualName declInfo $
            case HsOrigin.declKind originDecl of
              HsOrigin.Opaque cNameKind -> cNameKind
          hsIdentifier = Hs.Identifier $ Hs.getName (Hs.emptyDataName edata)
          typeSpec = BindingSpec.CTypeSpec {
              cTypeSpecModule     = Just hsModuleName
            , cTypeSpecIdentifier = Just hsIdentifier
            , cTypeSpecInstances  = Map.empty
            }
      in  (cQualName, getHeaders declInfo, typeSpec)

    auxNewtype :: Hs.Newtype -> CTypeSpec
    auxNewtype hsNewtype =
      let originDecl = Hs.newtypeOrigin hsNewtype
          declInfo = HsOrigin.declInfo originDecl
          cQualName = getCQualName declInfo $
            case HsOrigin.declKind originDecl of
              HsOrigin.Enum{}    -> C.NameKindTagged C.TagKindEnum
              HsOrigin.Typedef{} -> C.NameKindOrdinary
              HsOrigin.Union{}   -> C.NameKindTagged C.TagKindUnion
              HsOrigin.Macro{}   -> C.NameKindOrdinary
          hsIdentifier = Hs.Identifier $ Hs.getName (Hs.newtypeName hsNewtype)
          C.DeclSpec typeSpec' = HsOrigin.declSpec originDecl
          typeSpec = BindingSpec.CTypeSpec {
              cTypeSpecModule     = Just hsModuleName
            , cTypeSpecIdentifier = Just hsIdentifier
            , cTypeSpecInstances  =
                BindingSpec.cTypeSpecInstances typeSpec'
                  <> mkInstSpecs (Hs.newtypeInstances hsNewtype)
            }
      in  (cQualName, getHeaders declInfo, typeSpec)

    getHeaders :: C.DeclInfo -> Set HashIncludeArg
    getHeaders = getMainHeaders' . singleLocPath . C.declLoc

getCQualName :: C.DeclInfo -> C.NameKind -> C.QualName
getCQualName declInfo cNameKind = case C.declOrigin declInfo of
    C.NameOriginInSource -> C.QualName cName cNameKind
    C.NameOriginGenerated{} ->
      let cName' = fromMaybe cName (listToMaybe (C.declAliases declInfo))
      in  C.QualName cName' C.NameKindOrdinary
    C.NameOriginRenamedFrom fromCName -> C.QualName fromCName cNameKind
    C.NameOriginBuiltin -> C.QualName cName C.NameKindOrdinary
  where
    cName :: C.Name
    cName = C.nameC (C.declId declInfo)

mkInstSpecs ::
     Set Hs.TypeClass
  -> Map Hs.TypeClass (Omittable BindingSpec.InstanceSpec)
mkInstSpecs = Map.fromAscList . map (, oInstSpec) . Set.toAscList
  where
    oInstSpec :: Omittable BindingSpec.InstanceSpec
    oInstSpec = Require BindingSpec.InstanceSpec {
        instanceSpecStrategy    = Nothing -- TODO strategy?
      , instanceSpecConstraints = []      -- TODO constraints
      }
