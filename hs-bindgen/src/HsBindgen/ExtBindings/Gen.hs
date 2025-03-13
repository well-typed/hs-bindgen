module HsBindgen.ExtBindings.Gen (
    -- * API
    genExtBindings
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import HsBindgen.C.AST.Name
import HsBindgen.C.AST.Type qualified as C
import HsBindgen.Clang.CNameSpelling
import HsBindgen.Clang.Paths
import HsBindgen.ExtBindings
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Name
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

genExtBindings ::
     CHeaderIncludePath
  -> HsPackageName
  -> HsModuleName
  -> [Hs.Decl]
  -> UnresolvedExtBindings
genExtBindings headerIncludePath extIdentifierPackage extIdentifierModule =
    foldr aux emptyUnresolvedExtBindings
  where
    aux :: Hs.Decl -> UnresolvedExtBindings -> UnresolvedExtBindings
    aux = \case
      Hs.DeclData struct      -> maybeInsertType $ getStructExtBinding struct
      Hs.DeclEmpty{}          -> id
      Hs.DeclNewtype ntype    -> maybeInsertType $ getNewtypeExtBinding ntype
      Hs.DeclPatSyn{}         -> id
      Hs.DeclDefineInstance{} -> id
      Hs.DeclDeriveInstance{} -> id
      Hs.DeclForeignImport{}  -> id
      Hs.DeclVar{}            -> id

    insertType ::
         (CNameSpelling, HsIdentifier)
      -> UnresolvedExtBindings
      -> UnresolvedExtBindings
    insertType (cname, extIdentifierIdentifier) UnresolvedExtBindings{..} =
      UnresolvedExtBindings {
          unresolvedExtBindingsTypes =
            Map.insert
              cname
              [(headerSet, ExtIdentifier{..})]
              unresolvedExtBindingsTypes
        }

    maybeInsertType ::
         Maybe (CNameSpelling, HsIdentifier)
      -> UnresolvedExtBindings
      -> UnresolvedExtBindings
    maybeInsertType = maybe id insertType

    headerSet :: Set CHeaderIncludePath
    headerSet = Set.singleton headerIncludePath

    emptyUnresolvedExtBindings :: UnresolvedExtBindings
    emptyUnresolvedExtBindings = UnresolvedExtBindings {
        unresolvedExtBindingsTypes = Map.empty
      }

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

getStructExtBinding :: Hs.Struct n -> Maybe (CNameSpelling, HsIdentifier)
getStructExtBinding hsStruct = fmap (, hsId) $
    case Hs.structOrigin hsStruct of
      Hs.StructOriginStruct C.Struct{..} -> case structDeclPath of
        C.DeclPathStruct declName _declPath -> getCNS "struct " declName
        C.DeclPathUnion  declName _declPath -> getCNS "union "  declName
        _otherwise                          -> Nothing
      Hs.StructOriginEnum C.Enu{..} ->
        Just $ CNameSpelling ("enum " <> getCName enumTag)
  where
    hsId :: HsIdentifier
    hsId = HsIdentifier $ getHsName (Hs.structName hsStruct)

getNewtypeExtBinding :: Hs.Newtype -> Maybe (CNameSpelling, HsIdentifier)
getNewtypeExtBinding hsNewtype = fmap (, hsId) $
    case Hs.newtypeOrigin hsNewtype of
      Hs.NewtypeOriginEnum C.Enu{..} ->
        Just $ CNameSpelling ("enum " <> getCName enumTag)
      Hs.NewtypeOriginTypedef C.Typedef{..} ->
        Just $ CNameSpelling (getCName typedefName)
      Hs.NewtypeOriginMacro{} -> Nothing
  where
    hsId :: HsIdentifier
    hsId = HsIdentifier $ getHsName (Hs.newtypeName hsNewtype)

getCNS :: Text -> C.DeclName -> Maybe CNameSpelling
getCNS prefix = \case
    C.DeclNameTag     cname -> Just $ CNameSpelling (prefix <> getCName cname)
    C.DeclNameTypedef cname -> Just $ CNameSpelling (getCName cname)
    C.DeclNameNone          -> Nothing
