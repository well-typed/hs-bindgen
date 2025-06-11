module HsBindgen.Frontend.Pass.Rename (rename) where

import HsBindgen.Errors
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Graph.DeclUse (DeclUseGraph (..), UseOfDecl (..))
import HsBindgen.Frontend.Graph.DeclUse qualified as DeclUseGraph
import HsBindgen.Frontend.Graph.UseDecl (Usage (..), ValOrRef (..))
import HsBindgen.Frontend.Graph.UseDecl qualified as UseDecl
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Rename.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Rename anonymous declarations
rename :: C.TranslationUnit HandleMacros -> C.TranslationUnit RenameAnon
rename C.TranslationUnit{unitDecls, unitIncludeGraph, unitAnn} =
    reassemble $ mapMaybe (renameDef du) unitDecls
  where
    du :: DeclUseGraph
    du = DeclUseGraph.fromUseDecl (fst unitAnn)

    reassemble :: [C.Decl RenameAnon] -> C.TranslationUnit RenameAnon
    reassemble decls' = C.TranslationUnit{
          unitDecls = decls'
        , unitIncludeGraph
        , unitAnn
        }

{-------------------------------------------------------------------------------
  Def sites: declarations
-------------------------------------------------------------------------------}

renameDef :: DeclUseGraph -> C.Decl HandleMacros -> Maybe (C.Decl RenameAnon)
renameDef du decl = do
    guard $ not (squash decl)
    mkDecl <$> renameDeclId du declId (declNamespace declKind)
  where
    C.Decl{declInfo = C.DeclInfo{declLoc, declId}, declKind, declAnn} = decl

    mkDecl :: CName -> C.Decl RenameAnon
    mkDecl newId = C.Decl{
          declInfo = C.DeclInfo{
              declId = newId
            , declLoc
            }
        , declKind = renameUses du declKind
        , declAnn
        }

-- | Rename 'DeclId'
--
-- Returns 'Nothing' if this is an anonymous type without any use.
renameDeclId :: DeclUseGraph -> DeclId -> Namespace -> Maybe CName
renameDeclId _      (DeclNamed n) _  = Just n
renameDeclId du uid@(DeclAnon  _) ns =
    nameForAnon <$> DeclUseGraph.findNamedUseOf du (C.QualId uid ns)

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
  Use sites
-------------------------------------------------------------------------------}

class RenameUseSites a where
  renameUses :: DeclUseGraph -> a HandleMacros -> a RenameAnon

instance RenameUseSites C.DeclKind where
  renameUses du = \case
      C.DeclStruct struct    -> C.DeclStruct (renameUses du struct)
      C.DeclStructOpaque     -> C.DeclStructOpaque
      C.DeclUnion union      -> C.DeclUnion (renameUses du union)
      C.DeclUnionOpaque      -> C.DeclUnionOpaque
      C.DeclEnum enum        -> C.DeclEnum (renameUses du enum)
      C.DeclEnumOpaque       -> C.DeclEnumOpaque
      C.DeclTypedef typedef  -> C.DeclTypedef (renameUses du typedef)
      C.DeclMacro macro      -> C.DeclMacro (renameUses du macro)
      C.DeclFunction fun     -> C.DeclFunction (renameUses du fun)

instance RenameUseSites C.Struct where
  renameUses du C.Struct{..} = C.Struct{
        structFields = map (renameUses du) structFields
      , ..
      }

instance RenameUseSites C.StructField where
  renameUses du C.StructField{..} = C.StructField{
        structFieldType = renameUses du structFieldType
      , ..
      }

instance RenameUseSites C.Union where
  renameUses du C.Union{..} = C.Union{
        unionFields = map (renameUses du) unionFields
      , ..
      }

instance RenameUseSites C.UnionField where
  renameUses du C.UnionField{..} = C.UnionField{
        unionFieldType = renameUses du unionFieldType
      , ..
      }

instance RenameUseSites C.Enum where
  renameUses du C.Enum{..} = C.Enum{
        enumType      = renameUses du enumType
      , enumConstants =  map (renameUses du) enumConstants
      , ..
      }

instance RenameUseSites C.EnumConstant where
  renameUses _ C.EnumConstant{..} = C.EnumConstant{..}

instance RenameUseSites C.Typedef where
  renameUses du C.Typedef{..} = C.Typedef{
        typedefType = renameUses du typedefType
      , ..
      }

instance RenameUseSites C.Function where
  renameUses du C.Function{..} = C.Function{
        functionArgs = map (renameUses du) functionArgs
      , functionRes = renameUses du functionRes
      , ..
      }

instance RenameUseSites CheckedMacro where
  renameUses du (MacroType typ)  = MacroType (renameUses du typ)
  renameUses _  (MacroExpr expr) = MacroExpr expr

instance RenameUseSites CheckedMacroType where
  renameUses du CheckedMacroType{..} = CheckedMacroType{
        macroType = renameUses du macroType
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
  => DeclUseGraph -> Type p -> Type RenameAnon
renameType du = go
  where
    go :: Type p -> Type RenameAnon
    go (C.TypePrim prim) =
        C.TypePrim prim
    go (C.TypeStruct uid) =
        let qid = C.QualId uid NamespaceStruct :: QualId p
        in C.TypeStruct (renameUse du qid)
    go (C.TypeUnion uid) =
        let qid = C.QualId uid NamespaceUnion :: QualId p
        in C.TypeUnion (renameUse du qid)
    go (C.TypeEnum uid) =
        let qid = C.QualId uid NamespaceEnum :: QualId p
        in C.TypeEnum (renameUse du qid)
    go (C.TypeTypedef uid) =
        C.TypeTypedef (renameTypedefRef du uid)
    go (C.TypeMacroTypedef uid) =
        let qid = C.QualId uid NamespaceMacro :: QualId p
        in C.TypeMacroTypedef (renameUse du qid)
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
renameUse :: Id p ~ DeclId => DeclUseGraph -> C.QualId p -> CName
renameUse du qid@(C.QualId uid _namespace) =
    case uid of
      DeclNamed name -> name
      DeclAnon  _    ->
       case DeclUseGraph.findNamedUseOf du (coercePass qid) of
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
renameTypedefRef :: DeclUseGraph -> CName -> RenamedTypedefRef RenameAnon
renameTypedefRef du@(DeclUseGraph ud) typedefName =
    case squashTypedef typedefName =<< mDecl of
       Nothing -> TypedefRegular typedefName
       Just ty -> TypedefSquashed typedefName $ renameType du ty
  where
    typedefId :: QualId Parse
    typedefId = C.QualId (DeclNamed typedefName) NamespaceTypedef

    mDecl :: Maybe (Typedef Parse)
    mDecl = do
        decl@C.Decl{declKind} <- UseDecl.lookup typedefId ud
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
