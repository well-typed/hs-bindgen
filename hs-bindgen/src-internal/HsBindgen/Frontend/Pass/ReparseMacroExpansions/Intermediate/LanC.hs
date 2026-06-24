module HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.LanC (
    lanC
  ) where

import Control.Monad.Reader (MonadReader (..), Reader, runReader)
import Control.Monad.State (MonadState, StateT, modify, runStateT)
import Data.Map qualified as Map
import Data.Set qualified as Set

import Clang.CStandard (ClangCStandard)

import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.LanC.IsPass
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass.Msg
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

type CType = C.Type LanC

-- | Reparse declarations that have macro expansions in their type positions,
-- using the known-types environment from 'typecheckMacros'.
lanC ::
     ClangCStandard
  -> Map LanC.CName CType
     -- ^ Known non-macro names and their types (see 'ReparseEnv')
  -> Set LanC.CName
     -- ^ Known macro names (see 'ReparseEnv')
  -> C.TranslationUnit l PrepareReparse
  -> C.TranslationUnit l LanC
lanC cStd knownNonMacroTypes knownMacros unit =
    let (reparsedDecls, reparseState) =
          runM
            cStd
            knownNonMacroTypes
            knownMacros
            (mapM reparseDecl unit.decls)
    in  C.TranslationUnit{
            decls        = reparsedDecls
          , includeGraph = unit.includeGraph
          , meta         = updateMeta reparseState unit.meta
          }

{-------------------------------------------------------------------------------
  Reconstruct 'DeclMeta'

  We do not update 'DeclUseGraph' and 'UseDeclGraph' here — that happens in the
  subsequent 'HsBindgen.Frontend.Pass.Zip' pass.
-------------------------------------------------------------------------------}

updateMeta :: forall l. ReparseState -> DeclMeta l -> DeclMeta l
updateMeta reparseState meta = DeclMeta{
      declIndex    = updatedDeclIndex
    , useDeclGraph = meta.useDeclGraph
    , declUseGraph = meta.declUseGraph
    }
  where
    updatedDeclIndex :: DeclIndex l
    updatedDeclIndex =
      -- We use @foldr@ here to establish the original order of reparse
      -- warnings.
      foldr
        DeclIndex.registerDelayedReparseMacroExpansionsMsg
        meta.declIndex
        reparseState.reparseWarnings

{-------------------------------------------------------------------------------
  Reparse declarations with macro expansions
-------------------------------------------------------------------------------}

reparseDecl ::
     C.Decl l PrepareReparse
  -> M (C.Decl l LanC)
reparseDecl decl = case decl.kind of
    C.DeclMacro macro                    -> processMacro            info' macro
    C.DeclTypedef typedef                -> processTypedef          info' typedef
    C.DeclStruct struct                  -> processStruct           info' struct
    C.DeclUnion union                    -> processUnion            info' union
    C.DeclEnum enum                      -> processEnum             info' enum
    C.DeclAnonEnumConstant anonEnumConst -> processAnonEnumConstant info' anonEnumConst
    C.DeclOpaque mSize                   -> processOpaque           info' (C.DeclOpaque mSize)
    C.DeclFunction fun                   -> processFunction         info' fun
    C.DeclGlobal global                  -> processGlobal           info' global
  where
    info' :: C.DeclInfo LanC
    info' = coercePass decl.info

{-------------------------------------------------------------------------------
  Function for each kind of declaration
-------------------------------------------------------------------------------}

-- | Macros have already been type-checked; just coerce the pass annotation.
processMacro ::
     C.DeclInfo LanC
  -> TypecheckedMacro PrepareReparse l
  -> M (C.Decl l LanC)
processMacro info macro =
    pure C.Decl{
        info = info
      , ann  = NoAnn
      , kind = C.DeclMacro (coercePassParam macro)
      }

processStruct ::
     C.DeclInfo LanC
  -> C.Struct PrepareReparse
  -> M (C.Decl l LanC)
processStruct info struct = do
    mkDecl
      <$> mapM (processStructField info.id) struct.fields
      <*> C.traverseFlamField (processStructField info.id) struct.flam
  where
    mkDecl ::
         [C.StructField LanC]
      -> C.Flam LanC
      -> C.Decl l LanC
    mkDecl fields flam = C.Decl{
          info = info
        , ann  = NoAnn
        , kind = C.DeclStruct C.Struct{
              fields    = fields
            , flam      = flam
            , sizeof    = struct.sizeof
            , alignment = struct.alignment
            , ann       = struct.ann
            }
        }

-- | Reconstruct a 'C.FieldInfo' after reparsing.
--
-- When @mName@ is 'Nothing', the original name is preserved; when it is
-- 'Just', the reparsed name replaces it.
mkFieldInfo ::
     C.FieldInfo PrepareReparse
  -> Maybe Text
  -> C.FieldInfo LanC
mkFieldInfo info mName = C.FieldInfo{
      name    = maybe info.name C.ScopedName mName
    , comment = fmap coercePass info.comment
    , loc     = info.loc
    }

processStructField ::
     C.DeclId
  -> C.StructField PrepareReparse
  -> M (C.StructField LanC)
processStructField declId field =
    reparseWith declId LanC.reparseField field.ann withoutReparse withReparse
  where
    withoutReparse :: C.StructField LanC
    withoutReparse = C.StructField{
          typ    = coercePass field.typ
        , ann    = BeforeReparse field
        , offset = field.offset
        , width  = field.width
        , info   = mkFieldInfo field.info Nothing
        }

    withReparse :: (CType, Text) -> M (C.StructField LanC)
    withReparse (ty, name) = pure C.StructField{
          typ    = ty
        , ann    = BeforeReparse field
        , offset = field.offset
        , width  = field.width
        , info   = mkFieldInfo field.info (Just name)
        }

processUnion ::
     C.DeclInfo LanC
  -> C.Union PrepareReparse
  -> M (C.Decl l LanC)
processUnion info union = do
    combineFields <$> mapM (processUnionField info.id) union.fields
  where
    combineFields ::
         [C.UnionField LanC]
      -> C.Decl l LanC
    combineFields fields = C.Decl{
          info = info
        , ann  = NoAnn
        , kind = C.DeclUnion C.Union{
              fields    = fields
            , sizeof    = union.sizeof
            , alignment = union.alignment
            , ann       = union.ann
            }
        }

processUnionField ::
     C.DeclId
  -> C.UnionField PrepareReparse
  -> M (C.UnionField LanC)
processUnionField declId field =
    reparseWith declId LanC.reparseField field.ann withoutReparse withReparse
  where
    withoutReparse :: C.UnionField LanC
    withoutReparse = C.UnionField{
          typ  = coercePass field.typ
        , ann  = BeforeReparse field
        , info = mkFieldInfo field.info Nothing
        }

    withReparse :: (CType, Text) -> M (C.UnionField LanC)
    withReparse (ty, name) = pure C.UnionField{
          typ  = ty
        , ann  = BeforeReparse field
        , info = mkFieldInfo field.info (Just name)
        }

processOpaque ::
     C.DeclInfo LanC
  -> C.DeclKind l LanC
  -> M (C.Decl l LanC)
processOpaque info kind =
    pure C.Decl{
        info = info
      , kind = kind
      , ann  = NoAnn
      }

processEnum ::
     C.DeclInfo LanC
  -> C.Enum PrepareReparse
  -> M (C.Decl l LanC)
processEnum info enum =
    mkDecl <$> mapM processEnumConstant enum.constants
  where
    mkDecl ::
         [C.EnumConstant LanC]
      -> C.Decl l LanC
    mkDecl enumerators = C.Decl{
          info = info
        , ann  = NoAnn
        , kind = C.DeclEnum C.Enum{
              typ       = coercePass enum.typ
            , constants = enumerators
            , sizeof    = enum.sizeof
            , alignment = enum.alignment
            , ann       = NoAnn
            }
        }

processAnonEnumConstant ::
     C.DeclInfo LanC
  -> C.AnonEnumConstant PrepareReparse
  -> M (C.Decl l LanC)
processAnonEnumConstant info anonEnumConst =
    mkDecl <$> processEnumConstant anonEnumConst.constant
  where
    mkDecl ::
         C.EnumConstant LanC
      -> C.Decl l LanC
    mkDecl constant = C.Decl{
          info = info
        , ann  = NoAnn
        , kind = C.DeclAnonEnumConstant C.AnonEnumConstant{
              typ      = anonEnumConst.typ
            , constant = constant
            }
        }

processEnumConstant ::
     C.EnumConstant PrepareReparse
  -> M (C.EnumConstant LanC)
processEnumConstant constant =
    pure C.EnumConstant{
        value = constant.value
      , info  = C.FieldInfo{
            comment = fmap coercePass constant.info.comment
          , name    = constant.info.name
          , loc     = constant.info.loc
          }
      }

processTypedef ::
     C.DeclInfo LanC
  -> C.Typedef PrepareReparse
  -> M (C.Decl l LanC)
processTypedef info typedef = do
    reparsedType :: CType <-
      reparseWith info.id LanC.reparseTypedef typedef.ann
        (coercePass typedef.typ)
        pure
    pure C.Decl{
        info = info
      , ann  = NoAnn
      , kind = C.DeclTypedef C.Typedef{
                   typ = reparsedType
                 , ann = BeforeReparse typedef
                 }
      }

processFunction ::
     C.DeclInfo LanC
  -> C.Function PrepareReparse
  -> M (C.Decl l LanC)
processFunction info function = do
    function' <- reparseWith info.id LanC.reparseFunDecl function.ann withoutReparse withReparse
    pure C.Decl {
        info = info
      , ann = NoAnn
      , kind = C.DeclFunction function'
      }
  where
    withoutReparse :: C.Function LanC
    withoutReparse = C.Function{
          args  = map coercePass function.args
        , res   = coercePass function.res
        , attrs = function.attrs
        , ann   = BeforeReparse function
        }

    withReparse ::
         (([(Maybe Text, CType)] , CType) , Text)
      -> M (C.Function LanC)
    withReparse ((tys, ty), _name) = pure C.Function{
          args  = map (uncurry mkFunctionArg) tys
        , res   = ty
        , attrs = function.attrs
        , ann   = BeforeReparse function
        }

    mkFunctionArg :: Maybe Text -> CType -> C.FunctionArg LanC
    mkFunctionArg mname typ = C.FunctionArg{
          name = C.ScopedName <$> mname
        , typ  = typ
        , ann  = NoAnn
        }

-- | Globals (externs or constants)
processGlobal ::
     C.DeclInfo LanC
  -> C.Global PrepareReparse
  -> M (C.Decl l LanC)
processGlobal info global = do
    global' <- reparseWith info.id LanC.reparseGlobal global.ann withoutReparse withReparse
    pure C.Decl {
        info = info
      , ann = NoAnn
      , kind = C.DeclGlobal global'
      }
  where
    withoutReparse :: C.Global LanC
    withoutReparse = C.Global{
          typ = coercePass global.typ
        , ann = BeforeReparse global
        }

    withReparse :: CType -> M (C.Global LanC)
    withReparse ty = pure C.Global{
          typ = ty
        , ann = BeforeReparse global
        }

{-------------------------------------------------------------------------------
  Internal: monad used for reparsing
-------------------------------------------------------------------------------}

newtype M a = WrapM { _unwrapM :: StateT ReparseState (Reader ReparseEnv) a }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadReader ReparseEnv
    , MonadState ReparseState
    )

-- | Environment used when reparsing declarations with macro expansions.
data ReparseEnv = ReparseEnv {
      -- | Known non-macro names and their types (i.e., @typedef@s, @struct@s,
      --   @union@s and @enum@s).
      knownTypes :: Map LanC.CName CType
      -- | Known macro names; kept separate so we can restrict the reparse
      --   environment to macros actually /expanded/.
    , knownMacros :: Set LanC.CName
    }

data ReparseState = ReparseState {
      -- | Delayed parse messages collected during reparse
      --
      -- Stored in reverse order.
      reparseWarnings :: [(C.DeclId, DelayedReparseMacroExpansionsMsg)]
    }
  deriving stock (Generic)

runM ::
     ClangCStandard
  -> Map LanC.CName CType
  -> Set LanC.CName
  -> M a
  -> (a, ReparseState)
runM cStd knownTypes knownMacros (WrapM ma) = runReader (runStateT ma s) e
  where
    e :: ReparseEnv
    e = ReparseEnv {
        -- Add the bespoke types as a fallback (note, 'Map.union' is
        -- left-biased).
        knownTypes  = knownTypes `Map.union` LanC.bespokeTypes cStd
      , knownMacros = knownMacros
      }

    s :: ReparseState
    s = ReparseState{
        reparseWarnings  = []
      }

{-------------------------------------------------------------------------------
  Reparsing declarations with macro expansions
-------------------------------------------------------------------------------}

-- | Run reparser if needed; use fallback on failure or if not needed.
--
-- On reparsing failure, records @(declId, ReparseMacroExpansionsLanC e)@ in
-- 'ReparseState.reparseWarnings' and uses the fallback value.
--
-- Reconciliation against the pre-reparse tree happens in the subsequent @Zip@
-- intermediate pass; on parser success we always return the reparsed value.
reparseWith ::
     C.DeclId
  -> LanC.Parser a
  -> ReparseInfo FlatTokens
  -> r
  -> (a -> M r)
  -> M r
reparseWith declId parser reparseInfo fallback onSuccess = case reparseInfo of
    ReparseNotNeeded ->
      pure fallback
    ReparseNeeded tokens macroInvs -> do
      env <- ask
      let
          usedMacros :: Set Text
          usedMacros = invokedMacros macroInvs
          usedKnownMacros :: Set LanC.CName
          usedKnownMacros = Set.intersection env.knownMacros usedMacros
          usedUnknownMacros :: Set LanC.CName
          usedUnknownMacros = Set.difference usedMacros env.knownMacros
      forM_ usedUnknownMacros $ \u ->
          modify $ #reparseWarnings %~
            ((declId, ReparseMacroExpansionUnknownType u) :)

      let reparseEnv :: LanC.ReparseEnv
          reparseEnv = LanC.ReparseEnv env.knownTypes usedKnownMacros
      case parser reparseEnv tokens of
        Right a -> onSuccess a
        Left  e -> do
          modify $ #reparseWarnings %~ ((declId, ReparseMacroExpansionsLanC e) :)
          pure fallback
