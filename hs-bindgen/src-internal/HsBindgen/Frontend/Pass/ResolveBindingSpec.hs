module HsBindgen.Frontend.Pass.ResolveBindingSpec (
    resolveBindingSpec
  ) where

import Control.Monad ((<=<))
import Control.Monad.RWS (MonadReader, MonadState, RWS)
import Control.Monad.RWS qualified as RWS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.HighLevel.Types
import Clang.Paths
import HsBindgen.BindingSpec (ExternalBindingSpec, PrescriptiveBindingSpec)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.NonSelectedDecls (NonSelectedDecls)
import HsBindgen.Frontend.NonSelectedDecls qualified as NonSelectedDecls
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.NameAnon.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass
import HsBindgen.Frontend.Pass.Sort.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell
import HsBindgen.Util.Monad (mapMaybeM)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

resolveBindingSpec ::
     ExternalBindingSpec
  -> PrescriptiveBindingSpec
  -> C.TranslationUnit NameAnon
  -> (C.TranslationUnit ResolveBindingSpec, [Msg ResolveBindingSpec])
resolveBindingSpec
  extSpec
  pSpec
  C.TranslationUnit{unitDecls, unitIncludeGraph, unitAnn} =
    let (decls, MState{..}) =
          runM extSpec pSpec unitIncludeGraph (declNonSelected unitAnn) $
            resolveDecls unitDecls
        notUsedErrs = BindingSpecTypeNotUsed <$> Set.toAscList stateNoPTypes
    in  (reassemble decls, reverse stateErrors ++ notUsedErrs)
  where
    reassemble ::
         [C.Decl ResolveBindingSpec]
      -> C.TranslationUnit ResolveBindingSpec
    reassemble decls' = C.TranslationUnit{
        unitDecls = decls'
      , ..
      }


{-------------------------------------------------------------------------------
  Internal: monad
-------------------------------------------------------------------------------}

newtype M a = WrapM (RWS MEnv () MState a)
  deriving newtype (
      Applicative
    , Functor
    , Monad
    , MonadReader MEnv
    , MonadState MState
    )

runM ::
     ExternalBindingSpec
  -> PrescriptiveBindingSpec
  -> IncludeGraph
  -> NonSelectedDecls
  -> M a
  -> (a, MState)
runM extSpec pSpec includeGraph nonSelectedDecls (WrapM m) =
    let env        = MEnv extSpec pSpec includeGraph nonSelectedDecls
        state0     = initMState pSpec
        (x, s, ()) = RWS.runRWS m env state0
    in  (x, s)

{-------------------------------------------------------------------------------
  Internal: monad reader
-------------------------------------------------------------------------------}

data MEnv = MEnv {
      envExtSpec          :: ExternalBindingSpec
    , envPSpec            :: PrescriptiveBindingSpec
    , envIncludeGraph     :: IncludeGraph
    , envNonSelectedDecls :: NonSelectedDecls
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Internal: monad state
-------------------------------------------------------------------------------}

data MState = MState {
      stateErrors    :: [Msg ResolveBindingSpec] -- ^ Stored in reverse order
    , stateExtTypes  :: Map C.QualName (C.Type ResolveBindingSpec)
    , stateNoPTypes  :: Set C.QualName
    , stateOmitTypes :: Set C.QualName
    }
  deriving (Show)

initMState :: PrescriptiveBindingSpec -> MState
initMState pSpec = MState {
      stateErrors    = []
    , stateExtTypes  = Map.empty
    , stateNoPTypes  = BindingSpec.getTypes pSpec
    , stateOmitTypes = Set.empty
    }

insertError :: Msg ResolveBindingSpec -> MState -> MState
insertError e st = st {
      stateErrors = e : stateErrors st
    }

insertExtType ::
     C.QualName
  -> C.Type ResolveBindingSpec
  -> MState
  -> MState
insertExtType cQualName typ st = st {
      stateExtTypes = Map.insert cQualName typ (stateExtTypes st)
    }

deleteNoPType :: C.QualName -> MState -> MState
deleteNoPType cQualName st = st {
      stateNoPTypes = Set.delete cQualName (stateNoPTypes st)
    }

insertOmittedType :: C.QualName -> MState -> MState
insertOmittedType cQualName st = st {
      stateOmitTypes = Set.insert cQualName (stateOmitTypes st)
    }

{-------------------------------------------------------------------------------
  Internal: implementation
-------------------------------------------------------------------------------}

-- Resolve declarations, in two passes
resolveDecls :: [C.Decl NameAnon] -> M [C.Decl ResolveBindingSpec]
resolveDecls = mapM (uncurry resolveDeep) <=< mapMaybeM resolveTop

-- Pass one: top-level
--
-- If a declaration has an external binding, then the declaration is dropped and
-- the external binding is recorded.
--
-- If a declaration is omitted, then the declaration is dropped and the omission
-- is recorded.
--
-- Otherwise, the declaration is kept and is associated with an type
-- specification when applicable.
resolveTop ::
     C.Decl NameAnon
  -> M (Maybe (C.Decl NameAnon, Maybe BindingSpec.TypeSpec))
resolveTop decl = RWS.ask >>= \MEnv{..} -> do
    let cQualName  = C.declQualName decl
        sourcePath = singleLocPath $ C.declLoc (C.declInfo decl)
        declPaths  = IncludeGraph.reaches envIncludeGraph sourcePath
    isExt <- maybe False (\_ty -> True) <$> resolveExtBinding cQualName declPaths
    if isExt
      then return Nothing
      else case BindingSpec.lookupTypeSpec cQualName declPaths envPSpec of
        Just (BindingSpec.Require typeSpec) -> do
          RWS.modify' $ deleteNoPType cQualName
          return $ Just (decl, Just typeSpec)
        Just BindingSpec.Omit -> do
          RWS.modify' $ deleteNoPType cQualName . insertOmittedType cQualName
          return Nothing
        Nothing -> return $ Just (decl, Nothing)

-- Pass two: deep
--
-- Types within the declaration are resolved, and it is reassembled for the
-- current pass.
resolveDeep ::
     C.Decl NameAnon
  -> Maybe BindingSpec.TypeSpec
  -> M (C.Decl ResolveBindingSpec)
resolveDeep C.Decl{..} mTypeSpec =
    reassemble <$> resolve declKind
  where
    reassemble :: C.DeclKind ResolveBindingSpec -> C.Decl ResolveBindingSpec
    reassemble declKind' = C.Decl {
        declInfo = mkDeclInfo declInfo
      , declKind = declKind'
      , declAnn  = fromMaybe BindingSpec.defaultTypeSpec mTypeSpec
      }

    mkDeclInfo :: C.DeclInfo NameAnon -> C.DeclInfo ResolveBindingSpec
    mkDeclInfo C.DeclInfo{..} = C.DeclInfo{..}

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

class Resolve a where
  resolve :: a NameAnon -> M (a ResolveBindingSpec)

instance Resolve C.DeclKind where
  resolve = \case
      C.DeclStruct struct   -> C.DeclStruct <$> resolve struct
      C.DeclStructOpaque    -> return C.DeclStructOpaque
      C.DeclUnion union     -> C.DeclUnion <$> resolve union
      C.DeclUnionOpaque     -> return C.DeclUnionOpaque
      C.DeclTypedef typedef -> C.DeclTypedef <$> resolve typedef
      C.DeclEnum enum       -> C.DeclEnum <$> resolve enum
      C.DeclEnumOpaque      -> return C.DeclEnumOpaque
      C.DeclMacro macro     -> C.DeclMacro <$> resolve macro
      C.DeclFunction fun    -> C.DeclFunction <$> resolve fun
      C.DeclExtern ty       -> C.DeclExtern <$> resolve ty
      C.DeclConst ty        -> C.DeclConst <$> resolve ty

instance Resolve C.Struct where
  resolve C.Struct{..} =
      reassemble <$> mapM resolve structFields
    where
      reassemble ::
           [C.StructField ResolveBindingSpec]
        -> C.Struct ResolveBindingSpec
      reassemble structFields' = C.Struct {
            structFields = structFields'
          , ..
          }

instance Resolve C.StructField where
  resolve C.StructField{..} =
      reassemble <$> resolve structFieldType
    where
      reassemble ::
           C.Type ResolveBindingSpec
        -> C.StructField ResolveBindingSpec
      reassemble structFieldType' = C.StructField {
            structFieldType = structFieldType'
          , ..
          }

instance Resolve C.Union where
  resolve C.Union{..} =
      reassemble <$> mapM resolve unionFields
    where
      reassemble ::
           [C.UnionField ResolveBindingSpec]
        -> C.Union ResolveBindingSpec
      reassemble unionFields' = C.Union {
            unionFields = unionFields'
          , ..
          }

instance Resolve C.UnionField where
  resolve C.UnionField{..} =
      reassemble <$> resolve unionFieldType
    where
      reassemble :: C.Type ResolveBindingSpec -> C.UnionField ResolveBindingSpec
      reassemble unionFieldType' = C.UnionField {
            unionFieldType = unionFieldType'
          , ..
          }

instance Resolve C.Enum where
  resolve C.Enum{..} =
      reassemble
        <$> resolve enumType
        <*> mapM resolve enumConstants
    where
     reassemble ::
          C.Type ResolveBindingSpec
       -> [C.EnumConstant ResolveBindingSpec]
       -> C.Enum ResolveBindingSpec
     reassemble enumType' enumConstants' = C.Enum{
           enumType      = enumType'
         , enumConstants = enumConstants'
         , ..
         }

instance Resolve C.EnumConstant where
  resolve C.EnumConstant{..} = return C.EnumConstant{..}

instance Resolve C.Typedef where
  resolve C.Typedef{..} =
      reassemble <$> resolve typedefType
    where
      reassemble :: C.Type ResolveBindingSpec -> C.Typedef ResolveBindingSpec
      reassemble typedefType' = C.Typedef {
            typedefType = typedefType'
          , ..
          }

instance Resolve C.Function where
  resolve C.Function{..} = do
      functionArgs' <- mapM resolve functionArgs
      functionRes'  <- resolve functionRes
      return C.Function {
          functionArgs = functionArgs'
        , functionRes  = functionRes'
        , ..
        }

instance Resolve C.CheckedMacro where
  resolve (C.MacroType typ)  = C.MacroType <$> resolve typ
  resolve (C.MacroExpr expr) = return $ C.MacroExpr expr

instance Resolve C.CheckedMacroType where
  resolve C.CheckedMacroType{..} = do
      macroType' <- resolve macroType
      return C.CheckedMacroType{
          macroType = macroType'
        , ..
        }

instance Resolve C.Type where
  resolve = \case
      C.TypeStruct       uid -> auxU C.TypeStruct       uid C.NameKindStruct
      C.TypeUnion        uid -> auxU C.TypeUnion        uid C.NameKindUnion
      C.TypeEnum         uid -> auxU C.TypeEnum         uid C.NameKindEnum
      C.TypeMacroTypedef uid -> auxU C.TypeMacroTypedef uid C.NameKindOrdinary
      C.TypeTypedef      nm  -> auxN C.TypeTypedef      nm  C.NameKindOrdinary

      -- Recursive cases
      C.TypePointer t         -> C.TypePointer <$> resolve t
      C.TypeFun args res      -> C.TypeFun <$> mapM resolve args <*> resolve res
      C.TypeConstArray n t    -> C.TypeConstArray n <$> resolve t
      C.TypeIncompleteArray t -> C.TypeIncompleteArray <$> resolve t
      C.TypeBlock t           -> C.TypeBlock <$> resolve t

      -- Simple cases
      C.TypePrim t         -> return (C.TypePrim t)
      C.TypeVoid           -> return C.TypeVoid
      C.TypeExtBinding ext -> absurd ext
    where
      auxN ::
           (C.Name -> C.Type ResolveBindingSpec)
        -> C.Name
        -> C.NameKind
        -> M (C.Type ResolveBindingSpec)
      auxN mk cName nameKind =
        RWS.ask >>= \MEnv{..} -> RWS.get >>= \MState{..} -> do
          let cQualName = C.QualName cName nameKind
          -- check for type omitted by binding specification
          when (Set.member cQualName stateOmitTypes) $
            RWS.modify' $ insertError (BindingSpecOmittedTypeUse cQualName)
          -- check for selected external binding
          case Map.lookup cQualName stateExtTypes of
            Just ty -> return ty
            Nothing -> do
              -- check for external binding of non-selected type
              case NonSelectedDecls.lookup cQualName envNonSelectedDecls of
                Nothing -> return (mk cName)
                Just sourcePath -> do
                  let declPaths =
                        IncludeGraph.reaches envIncludeGraph sourcePath
                  maybe (mk cName) C.TypeExtBinding <$>
                    resolveExtBinding cQualName declPaths

      auxU ::
           (Id ResolveBindingSpec -> C.Type ResolveBindingSpec)
        -> Id NameAnon
        -> C.NameKind
        -> M (C.Type ResolveBindingSpec)
      auxU mk uid = auxN (const (mk uid)) (declIdName uid)

{-------------------------------------------------------------------------------
  Internal: auxiliary functions
-------------------------------------------------------------------------------}

-- | Lookup qualified name in the 'ExternalResolvedBindingSpec'
resolveExtBinding ::
     C.QualName
  -> Set SourcePath
  -> M (Maybe ResolvedExtBinding)
resolveExtBinding cQualName declPaths  = do
    MEnv{envExtSpec} <- RWS.ask
    case BindingSpec.lookupTypeSpec cQualName declPaths envExtSpec of
      Just (BindingSpec.Require typeSpec) ->
        case getExtHsRef cQualName typeSpec of
          Right ref -> do
            let resolved = ResolvedExtBinding {
                    extCName  = cQualName
                  , extHsRef  = ref
                  , extHsSpec = typeSpec
                  }
            RWS.modify' $ insertExtType cQualName (C.TypeExtBinding resolved)
            return (Just resolved)
          Left e -> do
            RWS.modify' $ insertError e
            return Nothing
      Just BindingSpec.Omit -> do
        RWS.modify' $
          insertError (BindingSpecOmittedTypeUse cQualName)
        return Nothing
      Nothing ->
        return Nothing

getExtHsRef ::
     C.QualName
  -> BindingSpec.TypeSpec
  -> Either (Msg ResolveBindingSpec) ExtHsRef
getExtHsRef cQualName typeSpec = do
    extHsRefModule <-
      maybe (Left (BindingSpecExtHsRefNoModule cQualName)) Right $
        BindingSpec.typeSpecModule typeSpec
    extHsRefIdentifier <-
      maybe (Left (BindingSpecExtHsRefNoIdentifier cQualName)) Right $
        BindingSpec.typeSpecIdentifier typeSpec
    return ExtHsRef{extHsRefModule, extHsRefIdentifier}
