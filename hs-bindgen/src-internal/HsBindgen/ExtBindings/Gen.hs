module HsBindgen.ExtBindings.Gen (
    -- * API
    genExtBindings
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.CNameSpelling
import Clang.Paths
import HsBindgen.C.AST.Macro qualified as C
import HsBindgen.C.AST.Name
import HsBindgen.C.AST.Type qualified as C
import HsBindgen.ExtBindings
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Name
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

genExtBindings ::
     CHeaderIncludePath
  -> HsModuleName
  -> [Hs.Decl]
  -> UnresolvedExtBindings
genExtBindings headerIncludePath extIdentifierModule =
    foldr aux emptyUnresolvedExtBindings
  where
    aux :: Hs.Decl -> UnresolvedExtBindings -> UnresolvedExtBindings
    aux = \case
      Hs.DeclData struct      -> insertTypes $ getStructExtBindings struct
      Hs.DeclEmpty edata      -> insertTypes $ getEmptyDataExtBindings edata
      Hs.DeclNewtype ntype    -> insertTypes $ getNewtypeExtBindings ntype
      Hs.DeclPatSyn{}         -> id
      Hs.DeclDefineInstance{} -> id
      Hs.DeclDeriveInstance{} -> id
      Hs.DeclForeignImport{}  -> id
      Hs.DeclVar{}            -> id
      Hs.DeclUnionGetter{}    -> id
      Hs.DeclUnionSetter{}    -> id
      Hs.DeclInlineC{}        -> id
      Hs.DeclInlineCInclude{} -> id

    insertTypes :: [EB] -> UnresolvedExtBindings -> UnresolvedExtBindings
    insertTypes ebs UnresolvedExtBindings{..} =
      UnresolvedExtBindings {
          unresolvedExtBindingsTypes =
            foldr insertType unresolvedExtBindingsTypes ebs
        }

    insertType ::
         EB
      -> Map CNameSpelling [(Set CHeaderIncludePath, ExtIdentifier)]
      -> Map CNameSpelling [(Set CHeaderIncludePath, ExtIdentifier)]
    insertType (EB extIdentifierIdentifier extIdentifierInstances cname) =
      Map.insert cname [(headerSet, ExtIdentifier{..})]

    headerSet :: Set CHeaderIncludePath
    headerSet = Set.singleton headerIncludePath

    emptyUnresolvedExtBindings :: UnresolvedExtBindings
    emptyUnresolvedExtBindings = UnresolvedExtBindings {
        unresolvedExtBindingsTypes = Map.empty
      }

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

data EB = EB HsIdentifier (Set HsTypeClass) CNameSpelling

getStructExtBindings :: Hs.Struct n -> [EB]
getStructExtBindings hsStruct = fmap (EB hsId insts) . catMaybes $
    case Hs.structOrigin hsStruct of
      Hs.StructOriginStruct C.Struct{..} ->
        getCNS "struct " structDeclPath : map (Just . getCNS_alias) structAliases
      Hs.StructOriginEnum C.Enu{..} ->
        getCNS "enum " enumDeclPath : map (Just . getCNS_alias) enumAliases
  where
    hsId :: HsIdentifier
    hsId = HsIdentifier $ getHsName (Hs.structName hsStruct)

    insts :: Set HsTypeClass
    insts = Hs.structInstances hsStruct

getEmptyDataExtBindings :: Hs.EmptyData -> [EB]
getEmptyDataExtBindings edata = fmap (EB hsId insts) . catMaybes $
    case Hs.emptyDataOrigin edata of
      Hs.EmptyDataOriginOpaqueStruct C.OpaqueStruct{..} ->
        Just (CNameSpelling ("struct " <> getCName opaqueStructTag))
          : map (Just . getCNS_alias) opaqueStructAliases
      Hs.EmptyDataOriginOpaqueEnum C.OpaqueEnum{..} ->
        Just (CNameSpelling ("enum " <> getCName opaqueEnumTag))
          : map (Just . getCNS_alias) opaqueEnumAliases
  where
    hsId :: HsIdentifier
    hsId = HsIdentifier $ getHsName (Hs.emptyDataName edata)

    insts :: Set HsTypeClass
    insts = Set.empty

getNewtypeExtBindings :: Hs.Newtype -> [EB]
getNewtypeExtBindings hsNewtype = fmap (EB hsId insts) . catMaybes $
    case Hs.newtypeOrigin hsNewtype of
      Hs.NewtypeOriginEnum C.Enu{..} ->
        getCNS "enum " enumDeclPath : map (Just . getCNS_alias) enumAliases
      Hs.NewtypeOriginTypedef C.Typedef{..} ->
        [Just (CNameSpelling (getCName typedefName))]
      Hs.NewtypeOriginUnion C.Union{..} ->
        getCNS "union " unionDeclPath : map (Just . getCNS_alias) unionAliases
      Hs.NewtypeOriginMacro C.Macro{..} ->
        [Just (CNameSpelling (getCName macroName))]
  where
    hsId :: HsIdentifier
    hsId = HsIdentifier $ getHsName (Hs.newtypeName hsNewtype)

    insts :: Set HsTypeClass
    insts = Hs.newtypeInstances hsNewtype

getCNS :: Text -> C.DeclPath -> Maybe CNameSpelling
getCNS prefix (C.DeclPathName cname) =
    Just $ CNameSpelling (prefix <> getCName cname)
getCNS _prefix (C.DeclPathAnon ctxt) =
    case ctxt of
      C.DeclPathCtxtTypedef typedefName ->
        Just $ CNameSpelling (getCName typedefName)
      _otherwise ->
        Nothing

getCNS_alias :: CName -> CNameSpelling
getCNS_alias = CNameSpelling . getCName

