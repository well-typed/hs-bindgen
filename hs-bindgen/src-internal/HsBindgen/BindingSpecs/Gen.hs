module HsBindgen.BindingSpecs.Gen (
    -- * API
    genBindingSpecs
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.CNameSpelling
import Clang.Paths
import HsBindgen.BindingSpecs (UnresolvedBindingSpecs)
import HsBindgen.BindingSpecs qualified as BindingSpecs
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

-- TODO omitted binding specs
genBindingSpecs ::
     CHeaderIncludePath
  -> HsModuleName
  -> [Hs.Decl]
  -> UnresolvedBindingSpecs
genBindingSpecs headerIncludePath hsModuleName = foldr aux BindingSpecs.empty
  where
    aux ::
         Hs.Decl
      -> UnresolvedBindingSpecs
      -> UnresolvedBindingSpecs
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
      -> UnresolvedBindingSpecs
      -> UnresolvedBindingSpecs
    insertType (cname, oTypeSpec) specs = BindingSpecs.BindingSpecs {
        bindingSpecsTypes = Map.insert cname [(headers, oTypeSpec)] $
          BindingSpecs.bindingSpecsTypes specs
      }

    headers :: Set CHeaderIncludePath
    headers = Set.singleton headerIncludePath

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

type Spec = (CNameSpelling, BindingSpecs.Omittable BindingSpecs.TypeSpec)

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
          typeSpec = BindingSpecs.TypeSpec {
              typeSpecModule     = Just hsModuleName
            , typeSpecIdentifier = Just hsIdentifier
            , typeSpecInstances  =
                 BindingSpecs.typeSpecInstances typeSpec'
                   <> mkInstSpecs (Hs.structInstances hsStruct)
            }
      in  (cnameSpelling, BindingSpecs.Require typeSpec)

-- TODO aliases
getEmptyDataSpec :: HsModuleName -> Hs.EmptyData -> Spec
getEmptyDataSpec hsModuleName edata =
    let originDecl = Hs.emptyDataOrigin edata
        cname = NameMangler.nameC $ C.declId (Origin.declInfo originDecl)
        cnameSpelling = CNameSpelling $ case Origin.declKind originDecl of
          Origin.OpaqueStruct -> "struct " <> getCName cname
          Origin.OpaqueEnum   -> "enum "   <> getCName cname
        hsIdentifier = HsIdentifier $ getHsName (Hs.emptyDataName edata)
        typeSpec = BindingSpecs.TypeSpec {
            typeSpecModule     = Just hsModuleName
          , typeSpecIdentifier = Just hsIdentifier
          , typeSpecInstances  = Map.empty
          }
    in  (cnameSpelling, BindingSpecs.Require typeSpec)

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
        typeSpec = BindingSpecs.TypeSpec {
            typeSpecModule     = Just hsModuleName
          , typeSpecIdentifier = Just hsIdentifier
          , typeSpecInstances  =
              BindingSpecs.typeSpecInstances typeSpec'
                <> mkInstSpecs (Hs.newtypeInstances hsNewtype)
          }
    in  (cnameSpelling, BindingSpecs.Require typeSpec)

mkInstSpecs ::
     Set HsTypeClass
  -> Map HsTypeClass (BindingSpecs.Omittable BindingSpecs.InstanceSpec)
mkInstSpecs = Map.fromAscList . map (, oInstSpec) . Set.toAscList
  where
    oInstSpec :: BindingSpecs.Omittable BindingSpecs.InstanceSpec
    oInstSpec = BindingSpecs.Require BindingSpecs.InstanceSpec {
        instanceSpecStrategy    = Nothing -- TODO strategy?
      , instanceSpecConstraints = []      -- TODO constraints
      }
