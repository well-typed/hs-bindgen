
module HsBindgen.Frontend.Pass.NameAnon (
    nameAnon
  ) where

import Data.Either (partitionEithers)

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph, UseOfDecl (..))
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (Usage (..), ValOrRef (..))
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.NameAnon.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

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

findNamedUseOf :: RenameEnv -> C.PrelimDeclId -> Maybe UseOfDecl
findNamedUseOf RenameEnv{envDeclIndex, envDeclUse} qualPrelimDeclId =
    DeclUseGraph.findNamedUseOf envDeclIndex envDeclUse qualPrelimDeclId

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
    case constructDeclId env declId of
      Nothing      -> Left  $ NameAnonSkipped declInfo
      Just declId' -> Right $ C.Decl{
        declInfo = C.DeclInfo{
            declId = declId'
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

-- | Get the declaration identifier
--
-- Returns 'Nothing' for unused anonmyous declarations.
constructDeclId :: RenameEnv -> Id HandleMacros -> Maybe (Id NameAnon)
constructDeclId env declId =
     case declId of
       C.PrelimDeclIdNamed name ->
         Just $ C.DeclId{
             name       = name
           , origDeclId = C.OrigDeclId declId
           , haskellId  = ()
           }
       C.PrelimDeclIdAnon _anonId kind -> do
         useOfAnon <- findNamedUseOf env declId
         Just $ C.DeclId{
             name       = C.DeclName (nameForAnon useOfAnon) kind
           , origDeclId = C.OrigDeclId declId
           , haskellId  = ()
           }

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
  nameUseSites _ (C.CommentRef c hs) = C.CommentRef c hs

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

      -- Actual modifications
      go (C.TypeRef uid)       = C.TypeRef $ nameUseSite env uid
      go (C.TypeTypedef n uTy) = C.TypeTypedef
                                   (nameUseSite env n)
                                   (nameUseSites env uTy)

      -- Recursive cases
      go (C.TypePointer ty)         = C.TypePointer (go ty)
      go (C.TypeFun args res)       = C.TypeFun (map go args) (go res)
      go (C.TypeConstArray n ty)    = C.TypeConstArray n (go ty)
      go (C.TypeIncompleteArray ty) = C.TypeIncompleteArray (go ty)
      go (C.TypeBlock ty)           = C.TypeBlock (go ty)
      go (C.TypeConst ty)           = C.TypeConst (go ty)

      -- Simple cases
      go (C.TypePrim prim)      = C.TypePrim prim
      go (C.TypeVoid)           = C.TypeVoid
      go (C.TypeExtBinding ext) = absurd ext
      go (C.TypeComplex prim)   = C.TypeComplex prim

-- | Rename specific use site
--
-- NOTE: there must be at least one use site, because we are renaming one!
nameUseSite :: RenameEnv -> C.PrelimDeclId -> C.DeclId NameAnon
nameUseSite env prelimDeclId =
    case constructDeclId env prelimDeclId of
      Nothing    -> panicPure "unused anonymous declaration?"
      Just declId -> declId

{-------------------------------------------------------------------------------
  Name generation

  NOTE: The name generation for anonymous types is /not/ configurable. This
  makes it possible to use these names also in binding specifications (which
  users can then use to override the names if desired).
-------------------------------------------------------------------------------}

-- | Construct name for anonymous declaration
nameForAnon :: UseOfDecl -> Text
nameForAnon = \case
      UsedByNamed (UsedInTypedef ByValue) typedefName ->
        typedefName.text
      UsedByNamed (UsedInTypedef ByRef) typedefName ->
        typedefName.text <> "_Deref"
      UsedByNamed (UsedInField _valOrRef fieldName) typeName ->
        typeName.text <> "_" <> fieldName.text
      UsedByFieldOfAnon _valOrRef fieldName useOfAnon ->
        nameForAnon useOfAnon <> "_" <> fieldName.text

      -- Anonymous declarations in functions or globals are unsupported. See
      -- 'functionDecl' and 'varDecl' in "HsBindgen.Frontend.Pass.Parse.Decl".
      UsedByNamed (UsedInVar _valOrRef) _varName ->
        panicPure "unsupported anonymous declaration in global"
      UsedByNamed (UsedInFunction _valOrRef) _functionName ->
        panicPure "unsupported anonymous declaration in signature"
