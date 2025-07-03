module HsBindgen.Frontend.Pass.NameAnon (
    nameAnon
  , NameAnonMsg(..)
  ) where

import Data.Either (partitionEithers)

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
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint qualified as PP

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Assign name to all anonymous declarations
nameAnon ::
      C.TranslationUnit HandleMacros
  -> (C.TranslationUnit NameAnon, [NameAnonMsg])
nameAnon C.TranslationUnit{..} = (
      C.TranslationUnit{
          unitDecls = unitDecls'
        , ..
        }
    , msgs
    )
  where
    msgs       :: [NameAnonMsg]
    unitDecls' :: [C.Decl NameAnon]
    (msgs, unitDecls') = partitionEithers (map (nameDecl env) unitDecls)

    env :: RenameEnv
    env = RenameEnv{
          envDeclIndex = declIndex unitAnn
        , envDeclUse   = DeclUseGraph.fromUseDecl (declUsage unitAnn)
        }

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data NameAnonMsg =
    -- | Skipped unused anonymous declaration entirely
    --
    -- @clang@ will produce a warning for this ("declaration does not declare
    -- anything"); we issue a separate message here in case we skip over
    -- something that we shouldn't.
    NameAnonSkipped (C.DeclInfo Parse)
  deriving stock (Show, Eq)

instance PrettyForTrace NameAnonMsg where
  prettyForTrace = \case
      NameAnonSkipped info -> PP.hsep [
          "Skipped unused anonynous declaration"
        , prettyForTrace info
        ]

instance HasDefaultLogLevel NameAnonMsg where
  getDefaultLogLevel = \case
      NameAnonSkipped{} -> Debug -- clang already warned

{-------------------------------------------------------------------------------
  Internal auxiliary: environment used for renaming
-------------------------------------------------------------------------------}

data RenameEnv = RenameEnv {
      envDeclIndex :: DeclIndex
    , envDeclUse   :: DeclUseGraph
    }

findNamedUseOf :: RenameEnv -> QualDeclId -> Maybe UseOfDecl
findNamedUseOf RenameEnv{envDeclIndex, envDeclUse} qid =
    DeclUseGraph.findNamedUseOf envDeclIndex envDeclUse qid

findAliasesOf :: RenameEnv -> QualDeclId -> [CName]
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
  -> Either NameAnonMsg (C.Decl NameAnon)
nameDecl env decl = do
    case mName of
      Nothing             -> Left  $ NameAnonSkipped (coercePass declInfo)
      Just (name, origin) -> Right $ C.Decl{
        declInfo = C.DeclInfo{
            declId      = name
          , declOrigin  = origin
          , declAliases = findAliasesOf env qid
          , declLoc
          , declHeader
          }
      , declKind = nameUseSites env declKind
      , declAnn
      }
  where
    C.Decl{declInfo, declKind, declAnn} = decl
    C.DeclInfo{declId, declLoc, declHeader} = declInfo

    qid :: QualDeclId
    qid = declQualDeclId decl

    mName :: Maybe (CName, C.NameOrigin)
    mName =
        case declId of
          DeclNamed n ->
            Just (n, C.NameOriginInSource)
          DeclAnon anonId ->
            (, C.NameOriginGenerated anonId) . nameForAnon <$>
              findNamedUseOf env qid

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

      -- Cases where we actually need to do work
      go (C.TypeStruct uid _) =
          let qid = QualDeclId uid C.NameKindStruct
          in uncurry C.TypeStruct (nameUseSite qid)
      go (C.TypeUnion uid _) =
          let qid = QualDeclId uid C.NameKindUnion
          in uncurry C.TypeUnion (nameUseSite qid)
      go (C.TypeEnum uid _) =
          let qid = QualDeclId uid C.NameKindEnum
          in uncurry C.TypeEnum (nameUseSite qid)
      go (C.TypeMacroTypedef uid _) =
          let qid = QualDeclId uid C.NameKindOrdinary
          in uncurry C.TypeMacroTypedef (nameUseSite qid)

      -- Recursive cases
      go (C.TypePointer ty)         = C.TypePointer (go ty)
      go (C.TypeFun args res)       = C.TypeFun (map go args) (go res)
      go (C.TypeConstArray n ty)    = C.TypeConstArray n (go ty)
      go (C.TypeIncompleteArray ty) = C.TypeIncompleteArray (go ty)

      -- Simple cases
      go (C.TypePrim prim)      = C.TypePrim prim
      go (C.TypeTypedef name)   = C.TypeTypedef name
      go (C.TypeVoid)           = C.TypeVoid
      go (C.TypeExtBinding ext) = absurd ext


      -- Rename specific use site
      --
      -- NOTE: there /must/ be at least one use site, because we are renaming one!
      nameUseSite :: QualDeclId -> (CName, C.NameOrigin)
      nameUseSite qid@(QualDeclId uid _nameKind) =
          case uid of
            DeclNamed name   -> (name, C.NameOriginInSource)
            DeclAnon  anonId ->
             case findNamedUseOf env qid of
               Just useOfAnon ->
                 (nameForAnon useOfAnon, C.NameOriginGenerated anonId)
               Nothing -> panicPure "impossible"

{-------------------------------------------------------------------------------
  Name generation

  NOTE: The name generation for anonymous types is /not/ configurable. This
  makes it possible to use these names also in binding specifications (which
  users can then use to override the names if desired).
-------------------------------------------------------------------------------}

-- | Construct name for anonymous declaration
nameForAnon :: UseOfDecl -> CName
nameForAnon = \case
      UsedByNamed (UsedInTypedef ByValue) typedefName ->
        C.qualNameName typedefName
      UsedByNamed (UsedInTypedef ByRef) typedefName ->
        C.qualNameName typedefName <> "_Deref"
      UsedByNamed (UsedInField _valOrRef fieldName) typeName ->
        C.qualNameName typeName <> "_" <> fieldName
      UsedByFieldOfAnon _valOrRef fieldName useOfAnon ->
        nameForAnon useOfAnon <> "_" <> fieldName

      -- Anonymous declarations used by functions cannot happen: we rule these
      -- out in the parser (see 'functionDecl').
      UsedByNamed (UsedInFunction _valOrRef) _functionName ->
        panicPure "nameForAnon: unexpected anonymous declaration in signature"
