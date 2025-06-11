module HsBindgen.Frontend.Pass.Rename (rename) where

import Prelude hiding (lookup)

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph (..), UseOfDecl (..))
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (Usage (..), ValOrRef (..))
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Rename.IsPass
import HsBindgen.Frontend.Pass.Sort.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Rename anonymous declarations
rename :: C.TranslationUnit HandleMacros -> C.TranslationUnit RenameAnon
rename C.TranslationUnit{unitDecls, unitIncludeGraph, unitAnn} =
    reassemble $ mapMaybe (renameDef env) unitDecls
  where
    env :: RenameEnv
    env = RenameEnv{
          envDeclIndex = declIndex unitAnn
        , envDeclUse   = DeclUseGraph.fromUseDecl (declUsage unitAnn)
        }

    reassemble :: [C.Decl RenameAnon] -> C.TranslationUnit RenameAnon
    reassemble decls' = C.TranslationUnit{
          unitDecls = decls'
        , unitIncludeGraph
        , unitAnn
        }

{-------------------------------------------------------------------------------
  Def sites: declarations
-------------------------------------------------------------------------------}

renameDef :: RenameEnv -> C.Decl HandleMacros -> Maybe (C.Decl RenameAnon)
renameDef env decl = do
    guard $ not (squash decl)
    mkDecl <$> renameDeclId env declId (declNamespace declKind)
  where
    C.Decl{declInfo = C.DeclInfo{declLoc, declId}, declKind, declAnn} = decl

    mkDecl :: CName -> C.Decl RenameAnon
    mkDecl newId = C.Decl{
          declInfo = C.DeclInfo{
              declId = newId
            , declLoc
            }
        , declKind = renameUses env declKind
        , declAnn
        }

-- | Rename 'DeclId'
--
-- Returns 'Nothing' if this is an anonymous type without any use.
renameDeclId :: RenameEnv -> DeclId -> Namespace -> Maybe CName
renameDeclId _       (DeclNamed n) _  = Just n
renameDeclId env uid@(DeclAnon  _) ns =
    nameForAnon <$> findNamedUseOf env qid
  where
    qid :: QualId HandleMacros
    qid = C.QualId uid ns

-- | Should we squash this declaration?
squash :: forall p. Id p ~ DeclId => C.Decl p -> Bool
squash C.Decl{declInfo = C.DeclInfo{declId}, declKind} =
    case declKind of
      C.DeclTypedef typedef ->
        let typedefName =
              case declId of
                DeclNamed n -> n
                DeclAnon  _ -> panicPure "unexpected anonymous typedef"
        in isJust $ squashTypedef typedefName typedef
      _otherwise ->
        False

{-------------------------------------------------------------------------------
  Internal auxiliary: environment used for renaming
-------------------------------------------------------------------------------}

data RenameEnv = RenameEnv {
      envDeclIndex :: DeclIndex
    , envDeclUse   :: DeclUseGraph
    }

findNamedUseOf :: Id p ~ DeclId => RenameEnv -> QualId p -> Maybe UseOfDecl
findNamedUseOf RenameEnv{envDeclIndex, envDeclUse} qid =
    DeclUseGraph.findNamedUseOf envDeclIndex envDeclUse (coercePass qid)

lookup :: RenameEnv -> QualId Parse -> Maybe (Decl Parse)
lookup RenameEnv{envDeclIndex} qid =
    DeclIndex.lookup qid envDeclIndex

{-------------------------------------------------------------------------------
  Use sites
-------------------------------------------------------------------------------}

class RenameUseSites a where
  renameUses :: RenameEnv -> a HandleMacros -> a RenameAnon


instance RenameUseSites C.DeclKind where
  renameUses env = \case
      C.DeclStruct struct    -> C.DeclStruct (renameUses env struct)
      C.DeclStructOpaque     -> C.DeclStructOpaque
      C.DeclUnion union      -> C.DeclUnion (renameUses env union)
      C.DeclUnionOpaque      -> C.DeclUnionOpaque
      C.DeclEnum enum        -> C.DeclEnum (renameUses env enum)
      C.DeclEnumOpaque       -> C.DeclEnumOpaque
      C.DeclTypedef typedef  -> C.DeclTypedef (renameUses env typedef)
      C.DeclMacro macro      -> C.DeclMacro (renameUses env macro)
      C.DeclFunction fun     -> C.DeclFunction (renameUses env fun)

instance RenameUseSites C.Struct where
  renameUses env C.Struct{..} = C.Struct{
        structFields = map (renameUses env) structFields
      , ..
      }

instance RenameUseSites C.StructField where
  renameUses env C.StructField{..} = C.StructField{
        structFieldType = renameUses env structFieldType
      , ..
      }

instance RenameUseSites C.Union where
  renameUses env C.Union{..} = C.Union{
        unionFields = map (renameUses env) unionFields
      , ..
      }

instance RenameUseSites C.UnionField where
  renameUses env C.UnionField{..} = C.UnionField{
        unionFieldType = renameUses env unionFieldType
      , ..
      }

instance RenameUseSites C.Enum where
  renameUses env C.Enum{..} = C.Enum{
        enumType      = renameUses env enumType
      , enumConstants =  map (renameUses env) enumConstants
      , ..
      }

instance RenameUseSites C.EnumConstant where
  renameUses _ C.EnumConstant{..} = C.EnumConstant{..}

instance RenameUseSites C.Typedef where
  renameUses env C.Typedef{..} = C.Typedef{
        typedefType = renameUses env typedefType
      , ..
      }

instance RenameUseSites C.Function where
  renameUses env C.Function{..} = C.Function{
        functionArgs = map (renameUses env) functionArgs
      , functionRes = renameUses env functionRes
      , ..
      }

instance RenameUseSites CheckedMacro where
  renameUses env (MacroType typ)  = MacroType (renameUses env typ)
  renameUses _  (MacroExpr expr) = MacroExpr expr

instance RenameUseSites CheckedMacroType where
  renameUses env CheckedMacroType{..} = CheckedMacroType{
        macroType = renameUses env macroType
      , ..
      }

instance RenameUseSites C.Type where
  renameUses = renameType

{-------------------------------------------------------------------------------
  Types

  We generalize this, as we might also call it on types (constructed from
  declarations) that we get from the UseDecl graph.
-------------------------------------------------------------------------------}

renameType :: forall p.
     ( Id p ~ DeclId
     , TypedefRef p ~ CName
     )
  => RenameEnv -> Type p -> Type RenameAnon
renameType env = go
  where
    go :: Type p -> Type RenameAnon
    go (C.TypePrim prim) =
        C.TypePrim prim
    go (C.TypeStruct uid) =
        let qid = C.QualId uid NamespaceStruct :: QualId p
        in C.TypeStruct (renameUse env qid)
    go (C.TypeUnion uid) =
        let qid = C.QualId uid NamespaceUnion :: QualId p
        in C.TypeUnion (renameUse env qid)
    go (C.TypeEnum uid) =
        let qid = C.QualId uid NamespaceEnum :: QualId p
        in C.TypeEnum (renameUse env qid)
    go (C.TypeTypedef uid) =
        C.TypeTypedef (renameTypedefRef env uid)
    go (C.TypeMacroTypedef uid) =
        let qid = C.QualId uid NamespaceMacro :: QualId p
        in C.TypeMacroTypedef (renameUse env qid)
    go (C.TypePointer ty) =
        C.TypePointer (go ty)
    go (C.TypeFun args res) =
        C.TypeFun (map go args) (go res)
    go (C.TypeVoid) =
        C.TypeVoid
    go (C.TypeExtBinding cSpelling extHsRef typeSpec) =
        C.TypeExtBinding cSpelling extHsRef typeSpec
    go (C.TypeConstArray n ty) =
        C.TypeConstArray n (go ty)
    go (C.TypeIncompleteArray ty) =
        C.TypeIncompleteArray (go ty)

-- | Rename specific use site
--
-- NOTE: there /must/ be at least one use site, because we are renaming one!
renameUse :: Id p ~ DeclId => RenameEnv -> C.QualId p -> CName
renameUse env qid@(C.QualId uid _namespace) =
    case uid of
      DeclNamed name -> name
      DeclAnon  _    ->
       case findNamedUseOf env qid of
         Just useOfAnon -> nameForAnon useOfAnon
         Nothing        -> panicPure "impossible"

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

-- | Should this /declaration/ of a typedef be squashed?
--
-- If so, we return its underlying type.
squashTypedef :: forall p.
     Id p ~ DeclId
  => CName -> Typedef p -> Maybe (C.Type p)
squashTypedef typedefName C.Typedef{typedefType = typ} =
    case typ of
      C.TypeStruct uid -> guard (anonOrSameName uid) >> return typ
      C.TypeUnion  uid -> guard (anonOrSameName uid) >> return typ
      C.TypeEnum   uid -> guard (anonOrSameName uid) >> return typ
      _otherwise       -> Nothing
  where
    anonOrSameName :: DeclId -> Bool
    anonOrSameName (DeclNamed name) = typedefName == name
    anonOrSameName (DeclAnon _)     = True

-- | Typedef use-sites
--
-- In order to know if the typedef is squashed or not, we need to look up
-- its declaration. If no declaration is found (perhaps because it's external),
-- we assume that it should not be squashed.
renameTypedefRef :: RenameEnv -> CName -> RenamedTypedefRef RenameAnon
renameTypedefRef env typedefName =
    case squashTypedef typedefName =<< mDecl of
       Nothing -> TypedefRegular typedefName
       Just ty -> TypedefSquashed typedefName $ renameType env ty
  where
    typedefId :: QualId Parse
    typedefId = C.QualId (DeclNamed typedefName) NamespaceTypedef

    mDecl :: Maybe (Typedef Parse)
    mDecl = do
        decl@C.Decl{declKind} <- lookup env typedefId
        case declKind of
          DeclTypedef typedef ->
            return typedef
          _otherwise ->
             panicPure $ concat [
                 "Unexpected decl " ++ show decl
               , " for typedef " ++ show typedefName
               ]

{-------------------------------------------------------------------------------
  Name generation

  NOTE: The name generation for anonymous types is /not/ configurable. This
  makes it possible to use these names also in binding specifications (which
  users can then use to override the names if desired).
-------------------------------------------------------------------------------}

-- | Construct name for anonymous declaration
nameForAnon :: UseOfDecl -> CName
nameForAnon = \case
      UsedByNamed (UsedInTypedef ByValue) (name, _namespace) ->
        name
      UsedByNamed (UsedInTypedef ByRef) (name, _namespace) ->
        name <> "_Deref"
      UsedByNamed (UsedInField _valOrRef field) (name, _namespace) ->
        name <> "_" <> field
      UsedByNamed (UsedInFunction _valOrRef) (name, _namespace) ->
        name
      UsedByAnon (UsedInTypedef _valOrRef) _useOfAnon ->
        panicPure $ "nameForAnon: unexpected anonymous typedef"
      UsedByAnon (UsedInField _valOrRef field) useOfAnon ->
        nameForAnon useOfAnon <> "_" <> field
      UsedByAnon (UsedInFunction _valOrRef) _useOfAnon ->
        panicPure $ "nameForAnon: unexpected anonymous function argument or return type"
