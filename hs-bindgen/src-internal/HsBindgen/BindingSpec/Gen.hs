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
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text

import Clang.Paths
import HsBindgen.Backend.PP.Translation (HsModuleOpts(..))
import HsBindgen.BindingSpec.Private (UnresolvedBindingSpec)
import HsBindgen.BindingSpec.Private qualified as BindingSpec
import HsBindgen.Config (Config(..))
import HsBindgen.Errors
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.Pass.MangleNames.IsPass qualified as MangleNames
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.Origin qualified as HsOrigin
import HsBindgen.Imports
import HsBindgen.Language.Haskell

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
     Config
  -> [CHeaderIncludePath]
  -> FilePath
  -> [Hs.Decl]
  -> IO ()
genBindingSpec Config{..} headerIncludePaths path =
      BindingSpec.writeFile path
    . genBindingSpec' headerIncludePaths hsModuleName
  where
    hsModuleName :: HsModuleName
    hsModuleName =
      HsModuleName $ Text.pack (hsModuleOptsName configHsModuleOpts)

{-------------------------------------------------------------------------------
  Internal API (for tests)
-------------------------------------------------------------------------------}

-- | Generate binding specification
genBindingSpecYaml ::
     [CHeaderIncludePath]
  -> HsModuleName
  -> [Hs.Decl]
  -> ByteString
genBindingSpecYaml headerIncludePaths hsModuleName =
    BindingSpec.encodeYaml . genBindingSpec' headerIncludePaths hsModuleName

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- TODO omitted types
genBindingSpec' ::
     [CHeaderIncludePath]
  -> HsModuleName
  -> [Hs.Decl]
  -> UnresolvedBindingSpec
genBindingSpec' headerIncludePaths hsModuleName = foldr aux BindingSpec.empty
  where
    aux ::
         Hs.Decl
      -> UnresolvedBindingSpec
      -> UnresolvedBindingSpec
    aux = \case
      Hs.DeclData struct      -> insertType $ getStructSpec hsModuleName struct
      Hs.DeclEmpty edata      -> insertType $ getEmptyDataSpec hsModuleName edata
      Hs.DeclNewtype ntype    -> insertType $ getNewtypeSpec hsModuleName ntype
      Hs.DeclPatSyn{}         -> id
      Hs.DeclDefineInstance{} -> id
      Hs.DeclDeriveInstance{} -> id
      Hs.DeclInlineCInclude{} -> id
      Hs.DeclInlineC{}        -> id
      Hs.DeclForeignImport{}  -> id
      Hs.DeclVar{}            -> id
      Hs.DeclUnionGetter{}    -> id
      Hs.DeclUnionSetter{}    -> id
      Hs.DeclSimple{}         -> id

    insertType ::
         Spec
      -> UnresolvedBindingSpec
      -> UnresolvedBindingSpec
    insertType (cQualName, oTypeSpec) spec = spec {
        BindingSpec.bindingSpecTypes =
          Map.insert cQualName [(headers, oTypeSpec)] $
            BindingSpec.bindingSpecTypes spec
      }

    headers :: Set CHeaderIncludePath
    headers = Set.fromList headerIncludePaths

type Spec = (C.QualName, BindingSpec.Omittable BindingSpec.TypeSpec)

-- TODO aliases
getStructSpec :: HsModuleName -> Hs.Struct n -> Spec
getStructSpec hsModuleName hsStruct = case Hs.structOrigin hsStruct of
    Nothing -> panicPure "getStructSpec: structOrigin is Nothing"
    Just originDecl ->
      let cQualName = getCQualName (HsOrigin.declInfo originDecl) $
            case HsOrigin.declKind originDecl of
              HsOrigin.Struct{} -> C.NameKindStruct
          hsIdentifier = HsIdentifier $ getHsName (Hs.structName hsStruct)
          MangleNames.DeclSpec typeSpec' = HsOrigin.declSpec originDecl
          typeSpec = BindingSpec.TypeSpec {
              typeSpecModule     = Just hsModuleName
            , typeSpecIdentifier = Just hsIdentifier
            , typeSpecInstances  =
                 BindingSpec.typeSpecInstances typeSpec'
                   <> mkInstSpecs (Hs.structInstances hsStruct)
            }
      in  (cQualName, BindingSpec.Require typeSpec)

-- TODO aliases
getEmptyDataSpec :: HsModuleName -> Hs.EmptyData -> Spec
getEmptyDataSpec hsModuleName edata =
    let originDecl = Hs.emptyDataOrigin edata
        cQualName = getCQualName (HsOrigin.declInfo originDecl) $
          case HsOrigin.declKind originDecl of
            HsOrigin.OpaqueStruct -> C.NameKindStruct
            HsOrigin.OpaqueEnum   -> C.NameKindEnum
            HsOrigin.OpaqueUnion  -> C.NameKindUnion
        hsIdentifier = HsIdentifier $ getHsName (Hs.emptyDataName edata)
        typeSpec = BindingSpec.TypeSpec {
            typeSpecModule     = Just hsModuleName
          , typeSpecIdentifier = Just hsIdentifier
          , typeSpecInstances  = Map.empty
          }
    in  (cQualName, BindingSpec.Require typeSpec)

-- TODO aliases
getNewtypeSpec :: HsModuleName -> Hs.Newtype -> Spec
getNewtypeSpec hsModuleName hsNewtype =
    let originDecl = Hs.newtypeOrigin hsNewtype
        cQualName = getCQualName (HsOrigin.declInfo originDecl) $
          case HsOrigin.declKind originDecl of
            HsOrigin.Enum{}    -> C.NameKindEnum
            HsOrigin.Typedef{} -> C.NameKindOrdinary
            HsOrigin.Union{}   -> C.NameKindUnion
            HsOrigin.Macro{}   -> C.NameKindOrdinary
        hsIdentifier = HsIdentifier $ getHsName (Hs.newtypeName hsNewtype)
        MangleNames.DeclSpec typeSpec' = HsOrigin.declSpec originDecl
        typeSpec = BindingSpec.TypeSpec {
            typeSpecModule     = Just hsModuleName
          , typeSpecIdentifier = Just hsIdentifier
          , typeSpecInstances  =
              BindingSpec.typeSpecInstances typeSpec'
                <> mkInstSpecs (Hs.newtypeInstances hsNewtype)
          }
    in  (cQualName, BindingSpec.Require typeSpec)

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
    cName = MangleNames.nameC (C.declId declInfo)

mkInstSpecs ::
     Set HsTypeClass
  -> Map HsTypeClass (BindingSpec.Omittable BindingSpec.InstanceSpec)
mkInstSpecs = Map.fromAscList . map (, oInstSpec) . Set.toAscList
  where
    oInstSpec :: BindingSpec.Omittable BindingSpec.InstanceSpec
    oInstSpec = BindingSpec.Require BindingSpec.InstanceSpec {
        instanceSpecStrategy    = Nothing -- TODO strategy?
      , instanceSpecConstraints = []      -- TODO constraints
      }
