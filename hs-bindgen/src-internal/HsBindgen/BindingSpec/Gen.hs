module HsBindgen.BindingSpec.Gen (
    -- * API
    genBindingSpec
  ) where

import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set

import Clang.Paths
import HsBindgen.BindingSpec (UnresolvedBindingSpec)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Errors
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.Pass.MangleNames.IsPass qualified as MangleNames
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.Origin qualified as HsOrigin
import HsBindgen.Imports
import HsBindgen.Language.C
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- TODO omitted types
genBindingSpec ::
     [CHeaderIncludePath]
  -> HsModuleName
  -> [Hs.Decl]
  -> UnresolvedBindingSpec
genBindingSpec headerIncludePaths hsModuleName = foldr aux BindingSpec.empty
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
    insertType (cSpelling, oTypeSpec) spec = spec {
        BindingSpec.bindingSpecTypes =
          Map.insert cSpelling [(headers, oTypeSpec)] $
            BindingSpec.bindingSpecTypes spec
      }

    headers :: Set CHeaderIncludePath
    headers = Set.fromList headerIncludePaths

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

type Spec = (C.Spelling, BindingSpec.Omittable BindingSpec.TypeSpec)

-- TODO aliases
getStructSpec :: HsModuleName -> Hs.Struct n -> Spec
getStructSpec hsModuleName hsStruct = case Hs.structOrigin hsStruct of
    Nothing -> panicPure "getStructSpec: structOrigin is Nothing"
    Just originDecl ->
      let cSpelling = getCSpelling (HsOrigin.declInfo originDecl) $
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
      in  (cSpelling, BindingSpec.Require typeSpec)

-- TODO aliases
getEmptyDataSpec :: HsModuleName -> Hs.EmptyData -> Spec
getEmptyDataSpec hsModuleName edata =
    let originDecl = Hs.emptyDataOrigin edata
        cSpelling = getCSpelling (HsOrigin.declInfo originDecl) $
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
    in  (cSpelling, BindingSpec.Require typeSpec)

-- TODO aliases
getNewtypeSpec :: HsModuleName -> Hs.Newtype -> Spec
getNewtypeSpec hsModuleName hsNewtype =
    let originDecl = Hs.newtypeOrigin hsNewtype
        cSpelling = getCSpelling (HsOrigin.declInfo originDecl) $
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
    in  (cSpelling, BindingSpec.Require typeSpec)

getCSpelling :: C.DeclInfo -> C.NameKind -> C.Spelling
getCSpelling declInfo cNameKind = case C.declOrigin declInfo of
    C.NameOriginInSource    -> C.Spelling cName cNameKind
    C.NameOriginGenerated{} ->
      let cName' = fromMaybe cName (listToMaybe (C.declAliases declInfo))
      in  C.Spelling cName' C.NameKindOrdinary
    C.NameOriginRenamedFrom fromCName -> C.Spelling fromCName cNameKind
  where
    cName :: CName
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
