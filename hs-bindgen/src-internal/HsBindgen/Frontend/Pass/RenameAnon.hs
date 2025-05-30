module HsBindgen.Frontend.Pass.RenameAnon (
    module HsBindgen.Frontend.Pass.RenameAnon.IsPass
  , renameAnon
  ) where

import HsBindgen.Errors
import HsBindgen.Frontend.AST
import HsBindgen.Frontend.Graph.DefUse (DefUseGraph (..))
import HsBindgen.Frontend.Graph.DefUse qualified as DefUseGraph
import HsBindgen.Frontend.Graph.UseDef qualified as UseDef
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleMacros
import HsBindgen.Frontend.Pass.Parse
import HsBindgen.Frontend.Pass.RenameAnon.IsPass
import HsBindgen.Frontend.Pass.RenameAnon.ProduceCName
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Rename anonymous declarations
--
-- Precondition: input must be ordered so that def sites come before use sites.
-- This is required so that we can drop typedefs around anonymous declarations.
renameAnon :: TranslationUnit HandleMacros -> TranslationUnit RenameAnon
renameAnon TranslationUnit{unitDecls, unitIncludeGraph, unitAnn} =
    reassemble $ mapMaybe (renameDef du) unitDecls
  where
    du :: DefUseGraph
    du = DefUseGraph.fromUseDef unitAnn

    reassemble :: [Decl RenameAnon] -> TranslationUnit RenameAnon
    reassemble decls' = TranslationUnit{
          unitDecls = decls'
        , unitIncludeGraph
        , unitAnn
        }

{-------------------------------------------------------------------------------
  Def sites: declarations
-------------------------------------------------------------------------------}

renameDef :: DefUseGraph -> Decl HandleMacros -> Maybe (Decl RenameAnon)
renameDef du decl = do
    guard $ not (squash decl)
    mkDecl <$>
      case uid of
        DeclNamed n -> Just $ CName n
        DeclAnon  _ -> nameForAnon <$>
                         DefUseGraph.findNamedUseOf du (coerceQualId qid)
  where
    Decl{declInfo = DeclInfo{declLoc}, declKind, declAnn} = decl

    qid :: QualId HandleMacros
    qid@(QualId uid _namespace) = declQualId decl

    mkDecl :: CName -> Decl RenameAnon
    mkDecl newId = Decl{
          declInfo = DeclInfo{
              declId = newId
            , declLoc
            }
        , declKind = renameUses du declKind
        , declAnn
        }

-- | Should we squash this declaration?
squash :: forall p. Id p ~ DeclId => Decl p -> Bool
squash Decl{declInfo = DeclInfo{declId}, declKind} =
    case declKind of
      DeclTypedef typedef -> aroundAnon (typedefType typedef)
      _otherwise          -> False
  where
    aroundAnon :: Type p -> Bool
    aroundAnon (TypePrim   _)     = False
    aroundAnon (TypeStruct uid)   = anonOrSameName uid
    aroundAnon (TypeUnion uid)    = anonOrSameName uid
    aroundAnon (TypeEnum uid)     = anonOrSameName uid
    aroundAnon (TypeTypedef _ _)  = False
    aroundAnon (TypePointer _)    = False
    aroundAnon (TypeFunction _ _) = False
    aroundAnon TypeVoid = False

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

instance RenameUseSites DeclKind where
  renameUses du = \case
      DeclStruct struct    -> DeclStruct (renameUses du struct)
      DeclStructOpaque     -> DeclStructOpaque
      DeclUnion fields     -> DeclUnion (map (renameUses du) fields)
      DeclUnionOpaque      -> DeclUnionOpaque
      DeclEnum enumerators -> DeclEnum enumerators
      DeclEnumOpaque       -> DeclEnumOpaque
      DeclTypedef typedef  -> DeclTypedef (renameUses du typedef)
      DeclMacro unparsed   -> DeclMacro unparsed
      DeclFunction fun     -> DeclFunction (renameUses du fun)

instance RenameUseSites Struct where
  renameUses du Struct{..} = Struct{
        structFields = map (renameUses du) structFields
      , ..
      }

instance RenameUseSites StructField where
  renameUses du StructField{..} = StructField{
        structFieldType = renameUses du structFieldType
      , ..
      }

instance RenameUseSites UnionField where
  renameUses du UnionField{..} = UnionField{
        unionFieldType = renameUses du unionFieldType
      , ..
      }

instance RenameUseSites Typedef where
  renameUses du Typedef{..} = Typedef{
        typedefType = renameUses du typedefType
      , ..
      }

instance RenameUseSites Function where
  renameUses du Function{..} = Function{
        functionArgs = map (renameUses du) functionArgs
      , functionRes = renameUses du functionRes
      , ..
      }

instance RenameUseSites Type where
  renameUses du = \case
      TypePrim prim ->
        TypePrim prim
      TypeStruct uid ->
        let qid = QualId uid NamespaceStruct
        in TypeStruct (renameUse du qid)
      TypeUnion uid ->
        let qid = QualId uid NamespaceUnion
        in TypeUnion (renameUse du qid)
      TypeEnum    uid ->
        let qid = QualId uid NamespaceEnum
        in TypeEnum (renameUse du qid)
      TypeTypedef uid NoAnn ->
        let qid = QualId uid NamespaceTypedef
        in TypeTypedef (renameUse du qid) (squashed du qid)
      TypePointer ty ->
        TypePointer (renameUses du ty)
      TypeFunction tys ty ->
        TypeFunction (map (renameUses du) tys) (renameUses du ty)
      TypeVoid ->
        TypeVoid

-- | Rename specific use site
--
-- NOTE: there /must/ be at least one use site, because we are renaming one!
renameUse :: DefUseGraph -> QualId HandleMacros -> CName
renameUse du qid@(QualId uid _namespace) =
    case uid of
      DeclNamed name -> CName name
      DeclAnon  _    ->
       case DefUseGraph.findNamedUseOf du (coerceQualId qid) of
         Just useOfAnon -> nameForAnon useOfAnon
         Nothing        -> panicPure "impossible"

squashed :: DefUseGraph -> QualId HandleMacros -> SquashedTypedef
squashed (DefUseGraph ud) qid =
    case UseDef.lookup (coerceQualId qid) ud of
      Just decl | squash decl -> SquashedTypedef
      _otherwise              -> KeptTypedef
