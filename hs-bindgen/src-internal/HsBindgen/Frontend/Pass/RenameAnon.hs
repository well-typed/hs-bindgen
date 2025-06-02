module HsBindgen.Frontend.Pass.RenameAnon (
    module HsBindgen.Frontend.Pass.RenameAnon.IsPass
  , renameAnon
  ) where

import HsBindgen.Errors
import HsBindgen.Frontend.AST.Internal
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Graph.DefUse (DefUseGraph (..))
import HsBindgen.Frontend.Graph.DefUse (UseOfDecl (..))
import HsBindgen.Frontend.Graph.DefUse qualified as DefUseGraph
import HsBindgen.Frontend.Graph.UseDef (Usage (..), ValOrRef (..))
import HsBindgen.Frontend.Graph.UseDef qualified as UseDef
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleMacros
import HsBindgen.Frontend.Pass.Parse
import HsBindgen.Frontend.Pass.RenameAnon.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Rename anonymous declarations
--
-- Precondition: input must be ordered so that def sites come before use sites.
-- This is required so that we can drop typedefs around anonymous declarations.
renameAnon :: C.TranslationUnit HandleMacros -> C.TranslationUnit RenameAnon
renameAnon C.TranslationUnit{unitDecls, unitIncludeGraph, unitAnn} =
    reassemble $ mapMaybe (renameDef du) unitDecls
  where
    du :: DefUseGraph
    du = DefUseGraph.fromUseDef unitAnn

    reassemble :: [C.Decl RenameAnon] -> C.TranslationUnit RenameAnon
    reassemble decls' = C.TranslationUnit{
          unitDecls = decls'
        , unitIncludeGraph
        , unitAnn
        }

{-------------------------------------------------------------------------------
  Def sites: declarations
-------------------------------------------------------------------------------}

renameDef :: DefUseGraph -> C.Decl HandleMacros -> Maybe (C.Decl RenameAnon)
renameDef du decl = do
    guard $ not (squash decl)
    mkDecl <$>
      case uid of
        DeclNamed n -> Just n
        DeclAnon  _ -> nameForAnon <$>
                         DefUseGraph.findNamedUseOf du (C.coerceQualId qid)
  where
    C.Decl{declInfo = C.DeclInfo{declLoc}, declKind, declAnn} = decl

    qid :: C.QualId HandleMacros
    qid@(C.QualId uid _namespace) = C.declQualId decl

    mkDecl :: CName -> C.Decl RenameAnon
    mkDecl newId = C.Decl{
          declInfo = C.DeclInfo{
              declId = newId
            , declLoc
            }
        , declKind = renameUses du declKind
        , declAnn
        }

-- | Should we squash this declaration?
squash :: forall p. Id p ~ DeclId => C.Decl p -> Bool
squash C.Decl{declInfo = C.DeclInfo{declId}, declKind} =
    case declKind of
      C.DeclTypedef typedef -> aroundAnon (C.typedefType typedef)
      _otherwise            -> False
  where
    aroundAnon :: C.Type p -> Bool
    aroundAnon (C.TypeStruct uid) = anonOrSameName uid
    aroundAnon (C.TypeUnion  uid) = anonOrSameName uid
    aroundAnon (C.TypeEnum   uid) = anonOrSameName uid
    aroundAnon _otherwise         = False

    anonOrSameName :: DeclId -> Bool
    anonOrSameName (DeclNamed name) =
        case declId of
          DeclNamed name' -> name == name'
          DeclAnon  _     -> panicPure "unexpected anonymous typedef"
    anonOrSameName (DeclAnon  _) =
        True

{-------------------------------------------------------------------------------
  Use sites
-------------------------------------------------------------------------------}

class RenameUseSites a where
  renameUses :: DefUseGraph -> a HandleMacros -> a RenameAnon

instance RenameUseSites C.DeclKind where
  renameUses du = \case
      C.DeclStruct struct    -> C.DeclStruct (renameUses du struct)
      C.DeclStructOpaque     -> C.DeclStructOpaque
      C.DeclUnion union      -> C.DeclUnion (renameUses du union)
      C.DeclEnum enum        -> C.DeclEnum (renameUses du enum)
      C.DeclEnumOpaque       -> C.DeclEnumOpaque
      C.DeclTypedef typedef  -> C.DeclTypedef (renameUses du typedef)
      C.DeclMacro macro       -> C.DeclMacro (renameUses du macro)
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
  renameUses du = \case
      C.TypePrim prim ->
        C.TypePrim prim
      C.TypeStruct uid ->
        let qid = C.QualId uid NamespaceStruct
        in C.TypeStruct (renameUse du qid)
      C.TypeUnion uid ->
        let qid = C.QualId uid NamespaceUnion
        in C.TypeUnion (renameUse du qid)
      C.TypeEnum uid ->
        let qid = C.QualId uid NamespaceEnum
        in C.TypeEnum (renameUse du qid)
      C.TypeTypedef uid NoAnn ->
        let qid = C.QualId uid NamespaceTypedef
        in C.TypeTypedef (renameUse du qid) (squashed du qid)
      C.TypePointer ty ->
        C.TypePointer (renameUses du ty)
      C.TypeFun args res ->
        C.TypeFun (map (renameUses du) args) (renameUses du res)
      C.TypeVoid ->
        C.TypeVoid
      C.TypeExtBinding extHsRef typeSpec ->
        C.TypeExtBinding extHsRef typeSpec
      C.TypeConstArray n ty ->
        C.TypeConstArray n (renameUses du ty)
      C.TypeIncompleteArray ty ->
        C.TypeIncompleteArray (renameUses du ty)

-- | Rename specific use site
--
-- NOTE: there /must/ be at least one use site, because we are renaming one!
renameUse :: DefUseGraph -> C.QualId HandleMacros -> CName
renameUse du qid@(C.QualId uid _namespace) =
    case uid of
      DeclNamed name -> name
      DeclAnon  _    ->
       case DefUseGraph.findNamedUseOf du (C.coerceQualId qid) of
         Just useOfAnon -> nameForAnon useOfAnon
         Nothing        -> panicPure "impossible"

squashed :: DefUseGraph -> C.QualId HandleMacros -> TypedefSquashed
squashed (DefUseGraph ud) qid =
    case UseDef.lookup (C.coerceQualId qid) ud of
      Just decl | squash decl -> SquashedTypedef
      _otherwise              -> KeptTypedef

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
