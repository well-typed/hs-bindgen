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
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.NameAnon.IsPass
import HsBindgen.Frontend.Pass.Parse.Type.PrelimDeclId
import HsBindgen.Frontend.Pass.Sort.IsPass
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

findNamedUseOf :: RenameEnv -> NsPrelimDeclId -> Maybe UseOfDecl
findNamedUseOf RenameEnv{envDeclIndex, envDeclUse} nsid =
    DeclUseGraph.findNamedUseOf envDeclIndex envDeclUse nsid

findAliasesOf :: RenameEnv -> NsPrelimDeclId -> [C.Name]
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
    case mName of
      Nothing             -> Left  $ NameAnonSkipped (coercePass declInfo)
      Just (name, origin) -> Right $ C.Decl{
        declInfo = C.DeclInfo{
            declId      = DeclId name origin
          , declAliases = findAliasesOf env nsid
          , declLoc
          , declHeader
          }
      , declKind = nameUseSites env declKind
      , declAnn
      }
  where
    C.Decl{declInfo, declKind, declAnn} = decl
    C.DeclInfo{declId, declLoc, declHeader} = declInfo

    nsid :: NsPrelimDeclId
    nsid = declNsPrelimDeclId decl

    mName :: Maybe (C.Name, C.NameOrigin)
    mName =
        case declId of
          PrelimDeclIdNamed n ->
            Just (n, C.NameOriginInSource)
          PrelimDeclIdAnon anonId ->
            (, C.NameOriginGenerated anonId) . nameForAnon <$>
              findNamedUseOf env nsid
          PrelimDeclIdBuiltin name ->
            Just (name, C.NameOriginInSource)

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
      C.DeclExtern ty        -> C.DeclExtern (nameUseSites env ty)
      C.DeclConst ty         -> C.DeclConst (nameUseSites env ty)

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

      -- Cases where we actually need to do work
      go (C.TypeStruct uid) = C.TypeStruct . nameUseSite $
        nsPrelimDeclId uid C.TypeNamespaceTag
      go (C.TypeUnion uid) = C.TypeUnion . nameUseSite $
        nsPrelimDeclId uid C.TypeNamespaceTag
      go (C.TypeEnum uid) = C.TypeEnum . nameUseSite $
        nsPrelimDeclId uid C.TypeNamespaceTag
      go (C.TypeMacroTypedef uid) = C.TypeMacroTypedef . nameUseSite $
        nsPrelimDeclId uid C.TypeNamespaceOrdinary

      -- Recursive cases
      go (C.TypePointer ty)         = C.TypePointer (go ty)
      go (C.TypeFun args res)       = C.TypeFun (map go args) (go res)
      go (C.TypeConstArray n ty)    = C.TypeConstArray n (go ty)
      go (C.TypeIncompleteArray ty) = C.TypeIncompleteArray (go ty)
      go (C.TypeBlock ty)           = C.TypeBlock (go ty)

      -- Simple cases
      go (C.TypePrim prim)      = C.TypePrim prim
      go (C.TypeTypedef name)   = C.TypeTypedef name
      go (C.TypeVoid)           = C.TypeVoid
      go (C.TypeExtBinding ext) = absurd ext


      -- Rename specific use site
      --
      -- NOTE: there /must/ be at least one use site, because we are renaming one!
      nameUseSite :: NsPrelimDeclId -> DeclId
      nameUseSite nsid = case nsid of
        NsPrelimDeclIdNamed name _ns -> DeclId name C.NameOriginInSource
        NsPrelimDeclIdBuiltin name   -> DeclId name C.NameOriginBuiltin
        NsPrelimDeclIdAnon anonId    -> case findNamedUseOf env nsid of
          Just useOfAnon ->
            DeclId (nameForAnon useOfAnon) (C.NameOriginGenerated anonId)
          Nothing -> panicPure "impossible"

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
