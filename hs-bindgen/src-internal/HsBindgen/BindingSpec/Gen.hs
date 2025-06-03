module HsBindgen.BindingSpec.Gen (
    -- * API
    genBindingSpec
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.CNameSpelling
import Clang.Paths
import HsBindgen.BindingSpec (UnresolvedBindingSpec)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Errors
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.Pass.NameMangler.IsPass qualified as NameMangler
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.Origin qualified as Origin
import HsBindgen.Imports
import HsBindgen.Language.C
import HsBindgen.Language.Haskell

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- TODO omitted types
genBindingSpec ::
     CHeaderIncludePath
  -> HsModuleName
  -> [Hs.Decl]
  -> UnresolvedBindingSpec
genBindingSpec headerIncludePath hsModuleName = foldr aux BindingSpec.empty
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

    insertType ::
         Spec
      -> UnresolvedBindingSpec
      -> UnresolvedBindingSpec
    insertType (cname, oTypeSpec) spec = BindingSpec.BindingSpec {
        bindingSpecTypes = Map.insert cname [(headers, oTypeSpec)] $
          BindingSpec.bindingSpecTypes spec
      }

    headers :: Set CHeaderIncludePath
    headers = Set.singleton headerIncludePath

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

type Spec = (CNameSpelling, BindingSpec.Omittable BindingSpec.TypeSpec)

-- TODO aliases
getStructSpec :: HsModuleName -> Hs.Struct n -> Spec
getStructSpec hsModuleName hsStruct = case Hs.structOrigin hsStruct of
    Nothing -> panicPure "getStructSpec: structOrigin is Nothing"
    Just originDecl ->
      let cname = NameMangler.nameC $ C.declId (Origin.declInfo originDecl)
          -- TODO correct CNameSpelling depends on how named
          cnameSpelling = CNameSpelling $ "struct " <> getCName cname
          hsIdentifier = HsIdentifier $ getHsName (Hs.structName hsStruct)
          NameMangler.DeclSpec typeSpec' = Origin.declSpec originDecl
          typeSpec = BindingSpec.TypeSpec {
              typeSpecModule     = Just hsModuleName
            , typeSpecIdentifier = Just hsIdentifier
            , typeSpecInstances  =
                 BindingSpec.typeSpecInstances typeSpec'
                   <> mkInstSpecs (Hs.structInstances hsStruct)
            }
      in  (cnameSpelling, BindingSpec.Require typeSpec)

-- TODO aliases
getEmptyDataSpec :: HsModuleName -> Hs.EmptyData -> Spec
getEmptyDataSpec hsModuleName edata =
    let originDecl = Hs.emptyDataOrigin edata
        cname = NameMangler.nameC $ C.declId (Origin.declInfo originDecl)
        cnameSpelling = CNameSpelling $ case Origin.declKind originDecl of
          Origin.OpaqueStruct -> "struct " <> getCName cname
          Origin.OpaqueEnum   -> "enum "   <> getCName cname
        hsIdentifier = HsIdentifier $ getHsName (Hs.emptyDataName edata)
        typeSpec = BindingSpec.TypeSpec {
            typeSpecModule     = Just hsModuleName
          , typeSpecIdentifier = Just hsIdentifier
          , typeSpecInstances  = Map.empty
          }
    in  (cnameSpelling, BindingSpec.Require typeSpec)

-- TODO aliases
getNewtypeSpec :: HsModuleName -> Hs.Newtype -> Spec
getNewtypeSpec hsModuleName hsNewtype =
    let originDecl = Hs.newtypeOrigin hsNewtype
        cname = NameMangler.nameC $ C.declId (Origin.declInfo originDecl)
        cnameSpelling = CNameSpelling $ case Origin.declKind originDecl of
          Origin.Enum{}    -> "enum " <> getCName cname
          Origin.Typedef{} -> getCName cname
          Origin.Union{}   -> "union " <> getCName cname
          Origin.Macro{}   -> getCName cname
        hsIdentifier = HsIdentifier $ getHsName (Hs.newtypeName hsNewtype)
        NameMangler.DeclSpec typeSpec' = Origin.declSpec originDecl
        typeSpec = BindingSpec.TypeSpec {
            typeSpecModule     = Just hsModuleName
          , typeSpecIdentifier = Just hsIdentifier
          , typeSpecInstances  =
              BindingSpec.typeSpecInstances typeSpec'
                <> mkInstSpecs (Hs.newtypeInstances hsNewtype)
          }
    in  (cnameSpelling, BindingSpec.Require typeSpec)

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
