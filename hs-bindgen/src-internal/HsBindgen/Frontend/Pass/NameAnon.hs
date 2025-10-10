module HsBindgen.Frontend.Pass.NameAnon (
    nameAnon
  ) where

import Data.Either (partitionEithers)

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph, UseOfDecl (..))
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (Usage (..), ValOrRef (..))
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.NameAnon.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass (OrigTypedefRef (..),
                                             ParseMsgKey (..), mapParseMsgs)
import HsBindgen.Frontend.Pass.Sort.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Assign name to all anonymous declarations
nameAnon ::
      C.TranslationUnit HandleMacros
  -> (C.TranslationUnit NameAnon, [Msg NameAnon])
nameAnon C.TranslationUnit{..} = (
      C.TranslationUnit{
          unitDecls = unitDecls'
        , unitAnn = unitAnn {
            declParseMsgs = mapParseMsgs (getDeclIdParseMsgKey env) $
              declParseMsgs unitAnn
          }
        , ..
        }
    , msgs
    )
  where
    msgs       :: [Msg NameAnon]
    unitDecls' :: [C.Decl NameAnon]
    (msgs, unitDecls') = partitionEithers (map (nameDecl env) unitDecls)

    env :: RenameEnv
    env = RenameEnv{
          envDeclIndex = declIndex unitAnn
        , envDeclUse   = declDeclUse unitAnn
        }

{-------------------------------------------------------------------------------
  Internal auxiliary: environment used for renaming
-------------------------------------------------------------------------------}

data RenameEnv = RenameEnv {
      envDeclIndex :: DeclIndex
    , envDeclUse   :: DeclUseGraph
    }

findNamedUseOf :: RenameEnv -> C.QualPrelimDeclId -> Maybe UseOfDecl
findNamedUseOf RenameEnv{envDeclIndex, envDeclUse} nsid =
    DeclUseGraph.findNamedUseOf envDeclIndex envDeclUse nsid

findAliasesOf :: RenameEnv -> C.QualPrelimDeclId -> [C.Name]
findAliasesOf RenameEnv{envDeclUse} = DeclUseGraph.findAliasesOf envDeclUse

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

-- | Declaration
--
-- Returns 'Left' message if the declaration is anonymous and unused.
nameDecl ::
     RenameEnv
  -> C.Decl HandleMacros
  -> Either (Msg NameAnon) (C.Decl NameAnon)
nameDecl env decl = do
    case getDeclId env nsId declId of
      Left _        -> Left  $ NameAnonSkipped (coercePass declInfo)
      Right declId' -> Right $ C.Decl{
        declInfo = C.DeclInfo{
            declId = declId'
          , declAliases = findAliasesOf env nsId
          , declLoc
          , declHeaderInfo
          , declAvailability
          , declComment = fmap (nameUseSites env) declComment
          }
      , declKind = nameUseSites env declKind
      , declAnn
      }
  where
    C.Decl{declInfo, declKind, declAnn} = decl
    C.DeclInfo{..} = declInfo

    nsId :: C.QualPrelimDeclId
    nsId = C.declQualPrelimDeclId decl

-- Get the declaration identifier. May fail for anonymous declarations, if they
-- have no use sites; in which case we used to return 'Nothing'. However, we do
-- not want to lose parse messages, so we use an 'Either'. See
-- 'getDeclIdParseMsgKey'; related:
-- https://github.com/well-typed/hs-bindgen/issues/1036.
getDeclId ::
     RenameEnv
  -> C.QualPrelimDeclId
  -> Id HandleMacros
  -> Either (Id NameAnon) (Id NameAnon)
getDeclId env nsid declId =
   case declId of
     C.PrelimDeclIdNamed n ->
       Right $ C.DeclId n C.NameOriginInSource
     C.PrelimDeclIdAnon anonId ->
       let orig :: C.NameOrigin
           orig = C.NameOriginGenerated anonId
       in  case nameForAnon <$> findNamedUseOf env nsid of
             Nothing   -> Left  $ C.DeclId "unused_anonymous_declaration" orig
             Just name -> Right $ C.DeclId name                           orig
     C.PrelimDeclIdBuiltin name ->
       Right $ C.DeclId name C.NameOriginInSource

getDeclIdParseMsgKey :: RenameEnv -> ParseMsgKey HandleMacros -> ParseMsgKey NameAnon
getDeclIdParseMsgKey env key = key{parseMsgDeclId = declId'}
  where
    declId :: Id HandleMacros
    declId = parseMsgDeclId key

    nsId :: C.QualPrelimDeclId
    nsId = C.qualPrelimDeclId declId (parseMsgDeclKind key)

    declId' :: Id NameAnon
    declId' = either id id $ getDeclId env nsId declId

{-------------------------------------------------------------------------------
  Use sites
-------------------------------------------------------------------------------}

class NameUseSites a where
  nameUseSites :: RenameEnv -> a HandleMacros -> a NameAnon

instance NameUseSites C.DeclKind where
  nameUseSites env = \case
      C.DeclStruct struct    -> C.DeclStruct (nameUseSites env struct)
      C.DeclUnion union      -> C.DeclUnion (nameUseSites env union)
      C.DeclEnum enum        -> C.DeclEnum (nameUseSites env enum)
      C.DeclTypedef typedef  -> C.DeclTypedef (nameUseSites env typedef)
      C.DeclOpaque cNameKind -> C.DeclOpaque cNameKind
      C.DeclMacro macro      -> C.DeclMacro (nameUseSites env macro)
      C.DeclFunction fun     -> C.DeclFunction (nameUseSites env fun)
      C.DeclGlobal ty        -> C.DeclGlobal (nameUseSites env ty)

instance NameUseSites C.FieldInfo where
  nameUseSites env C.FieldInfo{..} =
    C.FieldInfo {
      fieldComment = nameUseSites env <$> fieldComment
    , ..
    }

instance NameUseSites C.CommentRef where
  nameUseSites _ (C.ById t) = C.ById (nameUseSite t)
    where
      nameUseSite :: C.PrelimDeclId -> C.DeclId
      nameUseSite nsid = case nsid of
        C.PrelimDeclIdNamed name   -> C.DeclId name C.NameOriginInSource
        C.PrelimDeclIdBuiltin name -> C.DeclId name C.NameOriginBuiltin
        C.PrelimDeclIdAnon _       -> panicPure "Anonymous reference"

instance NameUseSites C.Comment where
  nameUseSites env (C.Comment comment) =
    C.Comment (fmap (nameUseSites env) comment)

instance NameUseSites C.Struct where
  nameUseSites env C.Struct{..} = C.Struct{
        structFields = map (nameUseSites env) structFields
      , ..
      }

instance NameUseSites C.StructField where
  nameUseSites env C.StructField{..} = C.StructField{
        structFieldInfo = nameUseSites env structFieldInfo
      , structFieldType = nameUseSites env structFieldType
      , ..
      }

instance NameUseSites C.Union where
  nameUseSites env C.Union{..} = C.Union{
        unionFields = map (nameUseSites env) unionFields
      , ..
      }

instance NameUseSites C.UnionField where
  nameUseSites env C.UnionField{..} = C.UnionField{
        unionFieldInfo = nameUseSites env unionFieldInfo
      , unionFieldType = nameUseSites env unionFieldType
      , ..
      }

instance NameUseSites C.EnumConstant where
  nameUseSites env C.EnumConstant{..} = C.EnumConstant{
        enumConstantInfo = nameUseSites env enumConstantInfo
      , ..
      }

instance NameUseSites C.Enum where
  nameUseSites env C.Enum{..} = C.Enum{
        enumType      = nameUseSites env enumType
      , enumConstants = map (nameUseSites env) enumConstants
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
        functionArgs = map (bimap id (nameUseSites env)) functionArgs
      , functionRes  = nameUseSites env functionRes
      , ..
      }

instance NameUseSites C.Type where
  nameUseSites env = go
    where
      go :: C.Type HandleMacros -> C.Type NameAnon

      -- Cases where we actually need to do work
      go (C.TypeStruct uid) = C.TypeStruct . nameUseSite $
        C.qualPrelimDeclId uid (C.NameKindTagged C.TagKindStruct)
      go (C.TypeUnion uid) = C.TypeUnion . nameUseSite $
        C.qualPrelimDeclId uid (C.NameKindTagged C.TagKindUnion)
      go (C.TypeEnum uid) = C.TypeEnum . nameUseSite $
        C.qualPrelimDeclId uid (C.NameKindTagged C.TagKindEnum)
      go (C.TypeMacroTypedef uid) = C.TypeMacroTypedef . nameUseSite $
        C.qualPrelimDeclId uid C.NameKindOrdinary

      -- Recursive cases
      go (C.TypePointer ty)         = C.TypePointer (go ty)
      go (C.TypeFun args res)       = C.TypeFun (map go args) (go res)
      go (C.TypeConstArray n ty)    = C.TypeConstArray n (go ty)
      go (C.TypeIncompleteArray ty) = C.TypeIncompleteArray (go ty)
      go (C.TypeBlock ty)           = C.TypeBlock (go ty)
      go (C.TypeConst ty)           = C.TypeConst (go ty)

      -- Simple cases
      go (C.TypePrim prim)      = C.TypePrim prim
      go (C.TypeTypedef ref)   = C.TypeTypedef (nameUseSitesTypedefRef env ref)
      go (C.TypeVoid)           = C.TypeVoid
      go (C.TypeExtBinding ext) = absurd ext
      go (C.TypeComplex prim)   = C.TypeComplex prim

      -- Rename specific use site
      --
      -- NOTE: there /must/ be at least one use site, because we are renaming one!
      nameUseSite :: C.QualPrelimDeclId -> C.DeclId
      nameUseSite nsid = case nsid of
        C.QualPrelimDeclIdNamed   name   _ns -> C.DeclId name C.NameOriginInSource
        C.QualPrelimDeclIdBuiltin name       -> C.DeclId name C.NameOriginBuiltin
        C.QualPrelimDeclIdAnon    anonId _tk -> case findNamedUseOf env nsid of
          Just useOfAnon ->
            C.DeclId (nameForAnon useOfAnon) (C.NameOriginGenerated anonId)
          Nothing -> panicPure "impossible"

nameUseSitesTypedefRef :: RenameEnv -> TypedefRef HandleMacros -> TypedefRef NameAnon
nameUseSitesTypedefRef env = unTypedefRefWrapper . nameUseSites env . TypedefRefWrapper

instance NameUseSites TypedefRefWrapper where
  nameUseSites env (TypedefRefWrapper (OrigTypedefRef n uTy)) =
      TypedefRefWrapper (OrigTypedefRef n (nameUseSites env uTy))

{-------------------------------------------------------------------------------
  Name generation

  NOTE: The name generation for anonymous types is /not/ configurable. This
  makes it possible to use these names also in binding specifications (which
  users can then use to override the names if desired).
-------------------------------------------------------------------------------}

-- | Construct name for anonymous declaration
nameForAnon :: UseOfDecl -> C.Name
nameForAnon = \case
      UsedByNamed (UsedInTypedef ByValue) typedefName ->
        typedefName
      UsedByNamed (UsedInTypedef ByRef) typedefName ->
        typedefName <> "_Deref"
      UsedByNamed (UsedInField _valOrRef fieldName) typeName ->
        typeName <> "_" <> fieldName
      UsedByFieldOfAnon _valOrRef fieldName useOfAnon ->
        nameForAnon useOfAnon <> "_" <> fieldName

      -- Anonymous declarations in functions or globals are unsupported. See
      -- 'functionDecl' and 'varDecl' in "HsBindgen.Frontend.Pass.Parse.Decl".
      UsedByNamed (UsedInVar _valOrRef) _varName ->
        panicPure "unsupported anonymous declaration in global"
      UsedByNamed (UsedInFunction _valOrRef) _functionName ->
        panicPure "unsupported anonymous declaration in signature"
