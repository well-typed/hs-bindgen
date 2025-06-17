module HsBindgen.Frontend.Pass.NameAnon (nameAnon) where

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph, UseOfDecl(..))
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (Usage(..), ValOrRef (..))
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.NameAnon.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Sort.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C (CName)
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Assign name to all anonymous declarations
nameAnon :: C.TranslationUnit HandleMacros -> C.TranslationUnit NameAnon
nameAnon C.TranslationUnit{..} = C.TranslationUnit{
      unitDecls = mapMaybe (nameDecl env) unitDecls
    , ..
    }
  where
    env :: RenameEnv
    env = RenameEnv{
          envDeclIndex = declIndex unitAnn
        , envDeclUse   = DeclUseGraph.fromUseDecl (declUsage unitAnn)
        }

{-------------------------------------------------------------------------------
  Internal auxiliary: environment used for renaming
-------------------------------------------------------------------------------}

data RenameEnv = RenameEnv {
      envDeclIndex :: DeclIndex
    , envDeclUse   :: DeclUseGraph
    }

findNamedUseOf :: RenameEnv -> C.QualId HandleMacros -> Maybe UseOfDecl
findNamedUseOf RenameEnv{envDeclIndex, envDeclUse} qid =
    DeclUseGraph.findNamedUseOf envDeclIndex envDeclUse (coercePass qid)

findAliasesOf :: RenameEnv -> C.QualId HandleMacros -> [CName]
findAliasesOf RenameEnv{envDeclUse} =
    DeclUseGraph.findAliasesOf envDeclUse . coercePass

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

-- | Declaration
--
-- Returns 'Nothing' if the declaration is anonymous and unused.
nameDecl :: RenameEnv -> C.Decl HandleMacros -> Maybe (C.Decl NameAnon)
nameDecl env decl = do
    (name, origin) <- case declId of
      DeclNamed n -> Just (n, C.NameOriginInSource)
      DeclAnon{} ->
        (, C.NameOriginGenerated) . nameForAnon <$> findNamedUseOf env qid
    return $ C.Decl{
        declInfo = C.DeclInfo{
            declId      = name
          , declLoc
          , declOrigin  = origin
          , declAliases = findAliasesOf env qid
          }
      , declKind = nameUseSites env declKind
      , declAnn
      }
  where
    C.Decl{declInfo, declKind, declAnn} = decl
    C.DeclInfo{declLoc, declId} = declInfo

    qid :: C.QualId HandleMacros
    qid = C.declQualId decl

{-------------------------------------------------------------------------------
  Use sites
-------------------------------------------------------------------------------}

class NameUseSites a where
  nameUseSites :: RenameEnv -> a HandleMacros -> a NameAnon

instance NameUseSites C.DeclKind where
  nameUseSites env = \case
      C.DeclStruct struct    -> C.DeclStruct (nameUseSites env struct)
      C.DeclStructOpaque     -> C.DeclStructOpaque
      C.DeclUnion union      -> C.DeclUnion (nameUseSites env union)
      C.DeclUnionOpaque      -> C.DeclUnionOpaque
      C.DeclEnum enum        -> C.DeclEnum (nameUseSites env enum)
      C.DeclEnumOpaque       -> C.DeclEnumOpaque
      C.DeclTypedef typedef  -> C.DeclTypedef (nameUseSites env typedef)
      C.DeclMacro macro      -> C.DeclMacro (nameUseSites env macro)
      C.DeclFunction fun     -> C.DeclFunction (nameUseSites env fun)

instance NameUseSites C.Struct where
  nameUseSites env C.Struct{..} = C.Struct{
        structFields = map (nameUseSites env) structFields
      , ..
      }

instance NameUseSites C.StructField where
  nameUseSites env C.StructField{..} = C.StructField{
        structFieldType = nameUseSites env structFieldType
      , ..
      }

instance NameUseSites C.Union where
  nameUseSites env C.Union{..} = C.Union{
        unionFields = map (nameUseSites env) unionFields
      , ..
      }

instance NameUseSites C.UnionField where
  nameUseSites env C.UnionField{..} = C.UnionField{
        unionFieldType = nameUseSites env unionFieldType
      , ..
      }

instance NameUseSites C.Enum where
  nameUseSites env C.Enum{..} = C.Enum{
        enumType      = nameUseSites env enumType
      , enumConstants = map coercePass enumConstants
      , ..
      }

instance NameUseSites C.Typedef where
  nameUseSites env C.Typedef{..} = C.Typedef{
        typedefType = nameUseSites env typedefType
      , ..
      }

instance NameUseSites C.CheckedMacro where
  nameUseSites env (C.MacroType typ)  = C.MacroType (nameUseSites env typ)
  nameUseSites _   (C.MacroExpr expr) = C.MacroExpr expr

instance NameUseSites C.CheckedMacroType where
  nameUseSites env C.CheckedMacroType{..} = C.CheckedMacroType{
        macroType = nameUseSites env macroType
      , ..
      }

instance NameUseSites C.Function where
  nameUseSites env C.Function{..} = C.Function{
        functionArgs = map (nameUseSites env) functionArgs
      , functionRes  = nameUseSites env functionRes
      , ..
      }

instance NameUseSites C.Type where
  nameUseSites env = go
    where
      go :: C.Type HandleMacros -> C.Type NameAnon
      go (C.TypePrim prim) =
          C.TypePrim prim
      go (C.TypeStruct uid _) =
          let qid = C.QualId uid C.NameKindStruct
          in uncurry C.TypeStruct (nameUseSite qid)
      go (C.TypeUnion uid _) =
          let qid = C.QualId uid C.NameKindUnion
          in uncurry C.TypeUnion (nameUseSite qid)
      go (C.TypeEnum uid _) =
          let qid = C.QualId uid C.NameKindEnum
          in uncurry C.TypeEnum (nameUseSite qid)
      go (C.TypeTypedef name) =
          C.TypeTypedef name
      go (C.TypeMacroTypedef uid _) =
          let qid = C.QualId uid C.NameKindOrdinary
          in uncurry C.TypeMacroTypedef (nameUseSite qid)
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

      -- Rename specific use site
      --
      -- NOTE: there /must/ be at least one use site, because we are renaming one!
      nameUseSite :: C.QualId HandleMacros -> (CName, C.NameOrigin)
      nameUseSite qid@(C.QualId uid _nameKind) =
          case uid of
            DeclNamed name -> (name, C.NameOriginInSource)
            DeclAnon  _    ->
             case findNamedUseOf env qid of
               Just useOfAnon -> (nameForAnon useOfAnon, C.NameOriginGenerated)
               Nothing        -> panicPure "impossible"

{-------------------------------------------------------------------------------
  Name generation

  NOTE: The name generation for anonymous types is /not/ configurable. This
  makes it possible to use these names also in binding specifications (which
  users can then use to override the names if desired).
-------------------------------------------------------------------------------}

-- | Construct name for anonymous declaration
nameForAnon :: UseOfDecl -> CName
nameForAnon = \case
      UsedByNamed (UsedInTypedef ByValue) (name, _nameKind) ->
        name
      UsedByNamed (UsedInTypedef ByRef) (name, _nameKind) ->
        name <> "_Deref"
      UsedByNamed (UsedInField _valOrRef field) (name, _nameKind) ->
        name <> "_" <> field
      UsedByNamed (UsedInFunction _valOrRef) (name, _nameKind) ->
        name
      UsedByAnon (UsedInTypedef _valOrRef) _useOfAnon ->
        panicPure $ "nameForAnon: unexpected anonymous typedef"
      UsedByAnon (UsedInField _valOrRef field) useOfAnon ->
        nameForAnon useOfAnon <> "_" <> field
      UsedByAnon (UsedInFunction _valOrRef) _useOfAnon ->
        panicPure $ "nameForAnon: unexpected anonymous argument or return type"
