module HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.LanC (
    lanC
  ) where

import Control.Monad.Reader (MonadReader (..), Reader, runReader)
import Control.Monad.State (MonadState, StateT, modify, runStateT)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set

import Clang.CStandard (ClangCStandard)

import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.ForgetAnn (ForgetAnn (forgetAnn))
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass (ReparseMacroExpansions)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass.Msg
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.LanC (LanC)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Zip
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Reparse declarations that have macro expansions in their type positions,
-- using the known-types environment from 'typecheckMacros'.
lanC ::
     forall l. Macro.HasTypes l
  => ClangCStandard
  -> Map LanC.CName (C.Type PrepareReparse)
     -- ^ Known non-macro names and their types (see 'ReparseEnv')
  -> Set LanC.CName
     -- ^ Known macro names (see 'ReparseEnv')
  -> Macro.Lang l
  -> C.TranslationUnit l PrepareReparse
  -> C.TranslationUnit l ReparseMacroExpansions
lanC cStd knownNonMacroTypes knownMacros macroLang unit =
    let (reparsedDecls, reparseState) =
          runM
            cStd
            (Map.map coercePass knownNonMacroTypes)
            knownMacros
            (mapM reparseDecl unit.decls)
    in  C.TranslationUnit{
            decls        = reparsedDecls
          , includeGraph = unit.includeGraph
          , meta         = updateMeta macroLang reparseState.reparseWarnings reparsedDecls unit.meta
          }

updateMeta ::
     forall l. Macro.HasTypes l
  => Macro.Lang l
  -> [(C.DeclId, DelayedReparseMacroExpansionsMsg)]
  -> [C.Decl l ReparseMacroExpansions]
  -> DeclMeta l
  -> DeclMeta l
updateMeta macroLang msgs decls meta = DeclMeta{
      declIndex    = updatedDeclIndex
    , useDeclGraph = UseDeclGraph.fromDeclUseGraph updatedDeclUseGraph
    , declUseGraph = updatedDeclUseGraph
    }
  where
    updatedDeclIndex :: DeclIndex l
    updatedDeclIndex =
      -- We use @foldr@ here to establish the original order of reparse
      -- warnings.
      foldr
        DeclIndex.registerDelayedReparseMacroExpansionsMsg
        meta.declIndex
        [ (declId, msg)
        | (declId, msg) <- msgs
        ]

    updatedDeclUseGraph :: DeclUseGraph
    updatedDeclUseGraph =
      Foldable.foldl'
        (flip (updateDeps macroLang))
        meta.declUseGraph
        decls

-- | Dependencies before reparse may point to underlying types. These have to be
-- replaced with their actual dependencies after reparse+zip.
--
-- For example,
--
-- @
-- // c_header.h
-- typedef int A;
-- #define B A
-- void foo(B x);
-- @
--
-- Before reparse, @foo@ directly depends on @A@. After reparse+zip, we know
-- that @foo@ depends on @B@. We have to cut the dependency to @A@ and
-- replace it with the dependency to @B@.
updateDeps ::
     Macro.HasTypes l
  => Macro.Lang l
  -> C.Decl l ReparseMacroExpansions
  -> DeclUseGraph
  -> DeclUseGraph
updateDeps macroLang decl graph =
    DeclUseGraph.insertDepsOfDecl macroLang decl $
      DeclUseGraph.deleteDeps (Set.singleton decl.info.id) graph

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
    C.DeclOpaque mSize                   -> processOpaque           info' (C.DeclOpaque mSize)
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
      <*> C.traverseFlamField (processStructField info.id) struct.flam
  where
    mkDecl ::
         [C.StructField ReparseMacroExpansions]
      -> C.Flam ReparseMacroExpansions
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
  -> C.FieldInfo LanC
mkFieldInfo info mName = C.FieldInfo{
      name    = maybe info.name C.ScopedName mName
    , comment = fmap coercePass info.comment
    , loc     = info.loc
    }

processStructField ::
     C.DeclId
  -> C.StructField PrepareReparse
  -> M (C.StructField ReparseMacroExpansions)
processStructField declId field =
    reparseWith declId LanC.reparseField field.ann withoutReparse withReparse
  where
    withoutReparse :: C.StructField PrepareReparse
    withoutReparse = field

    withReparse :: (C.Type LanC, Text) -> M (C.StructField LanC)
    withReparse (ty, name) = pure C.StructField{
          typ    = ty
        , ann    = NoAnn
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
     C.DeclId
  -> C.UnionField PrepareReparse
  -> M (C.UnionField ReparseMacroExpansions)
processUnionField declId field =
    reparseWith declId LanC.reparseField field.ann withoutReparse withReparse
  where
    withoutReparse :: C.UnionField PrepareReparse
    withoutReparse = field

    withReparse :: (C.Type LanC, Text) -> M (C.UnionField LanC)
    withReparse (ty, name) = pure C.UnionField{
          typ  = ty
        , ann  = NoAnn
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
    typedef' <- reparseWith info.id LanC.reparseTypedef typedef.ann withoutReparse withReparse
    pure C.Decl{
        info = info
      , ann  = NoAnn
      , kind = C.DeclTypedef typedef'
      }
  where
    withoutReparse :: C.Typedef PrepareReparse
    withoutReparse = typedef

    withReparse ::
         C.Type LanC
      -> M (C.Typedef LanC)
    withReparse typ' = pure C.Typedef{
          typ = typ'
        , ann = NoAnn
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
    withoutReparse :: C.Function PrepareReparse
    withoutReparse = function

    withReparse ::
         (([(Maybe Text, C.Type LanC)] , C.Type LanC) , Text)
      -> M (C.Function LanC)
    withReparse ((tys, ty), _name) = pure C.Function{
          args  = map (uncurry mkFunctionArg) tys
        , res   = ty
        , attrs = function.attrs
        , ann   = NoAnn
        }

    mkFunctionArg :: Maybe Text -> C.Type LanC -> C.FunctionArg LanC
    mkFunctionArg mname typ = C.FunctionArg{
          name   = C.ScopedName <$> mname
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
    withoutReparse :: C.Global PrepareReparse
    withoutReparse = global

    withReparse :: C.Type LanC -> M (C.Global LanC)
    withReparse ty = pure C.Global{
          typ = ty
        , ann = NoAnn
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
      knownTypes :: Map LanC.CName (C.Type LanC)
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
  -> Map LanC.CName (C.Type LanC)
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
-- On reparsing success and zip failure, records @(declId,
-- ReparseMacroExpansionsZip e)@ in 'ReparseState.reparseWarnings' and uses the
-- fallback value.
--
reparseWith ::
     forall a r. (ZipReparsed r, ForgetAnn r)
  => C.DeclId
  -> LanC.Parser a
  -> ReparseInfo FlatTokens
  -> r PrepareReparse
  -> (a -> M (r LanC))
  -> M (r ReparseMacroExpansions)
reparseWith declId parser reparseInfo fallback onSuccess = case reparseInfo of
    ReparseNotNeeded ->
      pure fallback'
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
        Right a -> do
            b <- onSuccess a
            case zipEither fallback b of
              Left es -> do
                forM_ es $ \e ->
                  modify $ #reparseWarnings %~ ((declId, e) :)
                pure fallback'
              Right c -> pure c
        Left  e -> do
          modify $ #reparseWarnings %~ ((declId, ReparseMacroExpansionsLanC e) :)
          pure fallback'
  where
    fallback' :: r ReparseMacroExpansions
    fallback' = forgetAnn fallback
