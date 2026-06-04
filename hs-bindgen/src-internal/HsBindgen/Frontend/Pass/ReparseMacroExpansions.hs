module HsBindgen.Frontend.Pass.ReparseMacroExpansions (
    reparseMacroExpansions
  ) where

import Control.Monad.Reader (MonadReader (..), Reader, runReader)
import Control.Monad.State (MonadState, StateT, modify, runStateT)
import Data.Map qualified as Map
import Data.Set qualified as Set

import Clang.CStandard (ClangCStandard)

import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.TranslationUnit qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

type CType = C.Type ReparseMacroExpansions

-- | Reparse declarations that have macro expansions in their type positions,
-- using the known-types environment from 'typecheckMacros'.
reparseMacroExpansions ::
     ClangCStandard
  -> Map LanC.CName CType
     -- ^ Known non-macro names and their types (see 'ReparseEnv')
  -> Set LanC.CName
     -- ^ Known macro names (see 'ReparseEnv')
  -> C.TranslationUnit l PrepareReparse
  -> C.TranslationUnit l ReparseMacroExpansions
reparseMacroExpansions cStd knownNonMacroTypes knownMacros unit =
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
        DeclIndex.registerDelayedParseMsg
        meta.declIndex
        reparseState.reparseWarnings

{-------------------------------------------------------------------------------
  Reparse declarations with macro expansions
-------------------------------------------------------------------------------}

reparseDecl ::
     C.Decl l PrepareReparse
  -> M (C.Decl l ReparseMacroExpansions)
reparseDecl decl = case decl.kind of
    C.DeclMacro macro                    -> processMacro            info' macro
    C.DeclTypedef typedef                -> processTypedef          info' typedef
    C.DeclStruct struct                  -> processStruct           info' struct
    C.DeclUnion union                    -> processUnion            info' union
    C.DeclEnum enum                      -> processEnum             info' enum
    C.DeclAnonEnumConstant anonEnumConst -> processAnonEnumConstant info' anonEnumConst
    C.DeclOpaque                         -> processOpaque           info' C.DeclOpaque
    C.DeclFunction fun                   -> processFunction         info' fun
    C.DeclGlobal global                  -> processGlobal           info' global
  where
    info' :: C.DeclInfo ReparseMacroExpansions
    info' = coercePass decl.info

{-------------------------------------------------------------------------------
  Function for each kind of declaration
-------------------------------------------------------------------------------}

-- | Macros have already been type-checked; just coerce the pass annotation.
processMacro ::
     C.DeclInfo ReparseMacroExpansions
  -> TypecheckedMacro PrepareReparse l
  -> M (C.Decl l ReparseMacroExpansions)
processMacro info macro =
    pure C.Decl{
        info = info
      , ann  = NoAnn
      , kind = C.DeclMacro (coercePassParam macro)
      }

processStruct ::
     C.DeclInfo ReparseMacroExpansions
  -> C.Struct PrepareReparse
  -> M (C.Decl l ReparseMacroExpansions)
processStruct info struct = do
    mkDecl
      <$> mapM (processStructField info.id) struct.fields
      <*> mapM (processStructField info.id) struct.flam
  where
    mkDecl ::
         [C.StructField ReparseMacroExpansions]
      -> Maybe (C.StructField ReparseMacroExpansions)
      -> C.Decl l ReparseMacroExpansions
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
  -> C.FieldInfo ReparseMacroExpansions
mkFieldInfo info mName = C.FieldInfo{
      name    = maybe info.name CScopedName mName
    , comment = fmap coercePass info.comment
    , loc     = info.loc
    }

processStructField ::
     DeclId
  -> C.StructField PrepareReparse
  -> M (C.StructField ReparseMacroExpansions)
processStructField declId field =
    reparseWith declId LanC.reparseField field.ann withoutReparse withReparse
  where
    withoutReparse :: C.StructField ReparseMacroExpansions
    withoutReparse = C.StructField{
          typ    = coercePass field.typ
        , ann    = BeforeReparse field
        , offset = field.offset
        , width  = field.width
        , info   = mkFieldInfo field.info Nothing
        }

    withReparse :: (CType, Text) -> M (C.StructField ReparseMacroExpansions)
    withReparse (ty, name) = pure C.StructField{
          typ    = ty
        , ann    = BeforeReparse field
        , offset = field.offset
        , width  = field.width
        , info   = mkFieldInfo field.info (Just name)
        }

processUnion ::
     C.DeclInfo ReparseMacroExpansions
  -> C.Union PrepareReparse
  -> M (C.Decl l ReparseMacroExpansions)
processUnion info union = do
    combineFields <$> mapM (processUnionField info.id) union.fields
  where
    combineFields ::
         [C.UnionField ReparseMacroExpansions]
      -> C.Decl l ReparseMacroExpansions
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
     DeclId
  -> C.UnionField PrepareReparse
  -> M (C.UnionField ReparseMacroExpansions)
processUnionField declId field =
    reparseWith declId LanC.reparseField field.ann withoutReparse withReparse
  where
    withoutReparse :: C.UnionField ReparseMacroExpansions
    withoutReparse = C.UnionField{
          typ  = coercePass field.typ
        , ann  = BeforeReparse field
        , info = mkFieldInfo field.info Nothing
        }

    withReparse :: (CType, Text) -> M (C.UnionField ReparseMacroExpansions)
    withReparse (ty, name) = pure C.UnionField{
          typ  = ty
        , ann  = BeforeReparse field
        , info = mkFieldInfo field.info (Just name)
        }

processOpaque ::
     C.DeclInfo ReparseMacroExpansions
  -> C.DeclKind l ReparseMacroExpansions
  -> M (C.Decl l ReparseMacroExpansions)
processOpaque info kind =
    pure C.Decl{
        info = info
      , kind = kind
      , ann  = NoAnn
      }

processEnum ::
     C.DeclInfo ReparseMacroExpansions
  -> C.Enum PrepareReparse
  -> M (C.Decl l ReparseMacroExpansions)
processEnum info enum =
    mkDecl <$> mapM processEnumConstant enum.constants
  where
    mkDecl ::
         [C.EnumConstant ReparseMacroExpansions]
      -> C.Decl l ReparseMacroExpansions
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
     C.DeclInfo ReparseMacroExpansions
  -> C.AnonEnumConstant PrepareReparse
  -> M (C.Decl l ReparseMacroExpansions)
processAnonEnumConstant info anonEnumConst =
    mkDecl <$> processEnumConstant anonEnumConst.constant
  where
    mkDecl ::
         C.EnumConstant ReparseMacroExpansions
      -> C.Decl l ReparseMacroExpansions
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
  -> M (C.EnumConstant ReparseMacroExpansions)
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
     C.DeclInfo ReparseMacroExpansions
  -> C.Typedef PrepareReparse
  -> M (C.Decl l ReparseMacroExpansions)
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
     C.DeclInfo ReparseMacroExpansions
  -> C.Function PrepareReparse
  -> M (C.Decl l ReparseMacroExpansions)
processFunction info function = do
    function' <- reparseWith info.id LanC.reparseFunDecl function.ann withoutReparse withReparse
    pure C.Decl {
        info = info
      , ann = NoAnn
      , kind = C.DeclFunction function'
      }
  where
    withoutReparse :: C.Function ReparseMacroExpansions
    withoutReparse = C.Function{
          args  = map coercePass function.args
        , res   = coercePass function.res
        , attrs = function.attrs
        , ann   = BeforeReparse function
        }

    withReparse ::
         (([(Maybe Text, CType)] , CType) , Text)
      -> M (C.Function ReparseMacroExpansions)
    withReparse ((tys, ty), _name) = pure C.Function{
          args  = map (uncurry mkFunctionArg) tys
        , res   = ty
        , attrs = function.attrs
        , ann   = BeforeReparse function
        }

    mkFunctionArg :: Maybe Text -> CType -> C.FunctionArg ReparseMacroExpansions
    mkFunctionArg mname typ = C.FunctionArg{
          name   = CScopedName <$> mname
        , argTyp = C.TypeFunArgF typ NoAnn
        }

-- | Globals (externs or constants)
processGlobal ::
     C.DeclInfo ReparseMacroExpansions
  -> C.Global PrepareReparse
  -> M (C.Decl l ReparseMacroExpansions)
processGlobal info global = do
    global' <- reparseWith info.id LanC.reparseGlobal global.ann withoutReparse withReparse
    pure C.Decl {
        info = info
      , ann = NoAnn
      , kind = C.DeclGlobal global'
      }
  where
    withoutReparse :: C.Global ReparseMacroExpansions
    withoutReparse = C.Global{
          typ = coercePass global.typ
        , ann = BeforeReparse global
        }

    withReparse :: CType -> M (C.Global ReparseMacroExpansions)
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
      reparseWarnings :: [(DeclId, DelayedParseMsg)]
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
-- On reparsing failure, records @(declId, ParseMacroErrorReparse e)@ in
-- 'ReparseState.reparseWarnings' and uses the fallback value.
--
-- Reconciliation against the pre-reparse tree happens in the subsequent
-- 'HsBindgen.Frontend.Pass.Zip.Zip' pass; on parser success we always return
-- the reparsed value.
reparseWith ::
     DeclId
  -> LanC.Parser a
  -> ReparseInfo FlatTokens
  -> r
  -> (a -> M r)
  -> M r
reparseWith declId parser reparseInfo fallback onSuccess = case reparseInfo of
    ReparseNotNeeded ->
      pure fallback
    ReparseNeeded tokens usedMacros -> do
      env <- ask
      let usedKnownMacros :: Set LanC.CName
          usedKnownMacros = Set.intersection env.knownMacros usedMacros
          usedUnknownMacros :: Set LanC.CName
          usedUnknownMacros = Set.difference usedMacros env.knownMacros
      forM_ usedUnknownMacros $ \u ->
          modify $ #reparseWarnings %~
            ((declId, ParseMacroReparseUnknownType u) :)

      let reparseEnv :: LanC.ReparseEnv
          reparseEnv = LanC.ReparseEnv env.knownTypes usedKnownMacros
      case parser reparseEnv tokens of
        Right a -> onSuccess a
        Left  e -> do
          modify $ #reparseWarnings %~ ((declId, ParseMacroErrorReparse e) :)
          pure fallback
