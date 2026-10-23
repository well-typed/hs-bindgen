module HsBindgen.Frontend.Pass.ReparseMacroExpansions (
    reparseMacroExpansions
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

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Reparse declarations that have macro expansions in their type positions,
-- using the known-types environment from 'typecheckMacros'.
reparseMacroExpansions ::
     forall l.
     ClangCStandard
  -> Map LanC.CName (C.Type PrepareReparse)
     -- ^ Known non-macro names and their types (see 'ReparseEnv')
  -> Set LanC.CName
     -- ^ Known macro names (see 'ReparseEnv')
  -> C.TranslationUnit l PrepareReparse
  -> C.TranslationUnit l ReparseMacroExpansions
reparseMacroExpansions cStd knownNonMacroTypes knownMacros unit =
    let (reparsedDecls, reparseState) =
          runM
            cStd
            (Map.map coercePass knownNonMacroTypes)
            knownMacros
            (mapM reparseDecl unit.decls)
    in  C.TranslationUnit{
            decls        = reparsedDecls
          , includeGraph = unit.includeGraph
          , meta         = updateMeta reparseState.reparseWarnings reparsedDecls unit.meta
          }

updateMeta ::
     forall l.
     [(C.DeclId, DelayedReparseMacroExpansionsMsg)]
  -> [C.Decl l ReparseMacroExpansions]
  -> DeclMeta l
  -> DeclMeta l
updateMeta msgs decls meta = DeclMeta{
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
        (flip updateDeps)
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
     C.Decl l ReparseMacroExpansions
  -> DeclUseGraph
  -> DeclUseGraph
updateDeps decl graph =
    DeclUseGraph.insertDepsOfDecl decl $
      DeclUseGraph.deleteDeps (Set.singleton decl.id) graph

{-------------------------------------------------------------------------------
  Reparse declarations with macro expansions
-------------------------------------------------------------------------------}

reparseDecl ::
     C.Decl l PrepareReparse
  -> M (C.Decl l ReparseMacroExpansions)
reparseDecl decl = case decl.kind of
    C.DeclMacro macro                    -> processMacro                decl macro
    C.DeclTypedef typedef                -> processTypedef              decl typedef
    C.DeclStruct struct                  -> processStruct               decl struct
    C.DeclUnion union                    -> processUnion                decl union
    C.DeclEnum enum                      -> processEnum                 decl enum
    C.DeclUntaggedEnumConstant enumConst -> processUntaggedEnumConstant decl enumConst
    C.DeclOpaque mSize                   -> processOpaque               decl (C.DeclOpaque mSize)
    C.DeclFunction fun                   -> processFunction             decl fun
    C.DeclGlobal global                  -> processGlobal               decl global

-- | Assemble the reparsed declaration from the original one and its new kind
reparsedDecl ::
     C.Decl l PrepareReparse
  -> C.DeclKind l ReparseMacroExpansions
  -> C.Decl l ReparseMacroExpansions
reparsedDecl decl kind = C.coercePassDeclWith kind NoAnn decl

{-------------------------------------------------------------------------------
  Function for each kind of declaration
-------------------------------------------------------------------------------}

-- | Macros have already been type-checked; just coerce the pass annotation.
processMacro ::
     C.Decl l PrepareReparse
  -> TypecheckedMacro PrepareReparse l
  -> M (C.Decl l ReparseMacroExpansions)
processMacro decl macro =
    pure $ reparsedDecl decl $ C.DeclMacro (coercePassParam macro)

processStruct ::
     forall l.
     C.Decl l PrepareReparse
  -> C.Struct PrepareReparse
  -> M (C.Decl l ReparseMacroExpansions)
processStruct decl struct = do
    mkDecl
      <$> mapM (processField decl.id) struct.fields
      <*> C.traverseFlamField (processRegularField decl.id) struct.flam
  where
    mkDecl ::
         [C.Field ReparseMacroExpansions]
      -> C.Flam ReparseMacroExpansions
      -> C.Decl l ReparseMacroExpansions
    mkDecl fields flam = reparsedDecl decl $ C.DeclStruct C.Struct{
          fields    = fields
        , flam      = flam
        , sizeof    = struct.sizeof
        , alignment = struct.alignment
        , ann       = struct.ann
        }

processUnion ::
     forall l.
     C.Decl l PrepareReparse
  -> C.Union PrepareReparse
  -> M (C.Decl l ReparseMacroExpansions)
processUnion decl union = do
    combineFields <$> mapM (processField decl.id) union.fields
  where
    combineFields ::
         [C.Field ReparseMacroExpansions]
      -> C.Decl l ReparseMacroExpansions
    combineFields fields = reparsedDecl decl $ C.DeclUnion C.Union{
          fields    = fields
        , sizeof    = union.sizeof
        , alignment = union.alignment
        , ann       = union.ann
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

processField ::
     C.DeclId
  -> C.Field PrepareReparse
  -> M (C.Field ReparseMacroExpansions)
processField declId = \case
    C.FieldRegular  field -> C.FieldRegular  <$> processRegularField declId field
    C.FieldImplicit field -> C.FieldImplicit <$> processImplicitField declId field

processRegularField ::
     C.DeclId
  -> C.RegularField PrepareReparse
  -> M (C.RegularField ReparseMacroExpansions)
processRegularField declId field =
    reparseWith declId LanC.reparseField field.ann withoutReparse withReparse
  where
    withoutReparse :: C.RegularField PrepareReparse
    withoutReparse = field

    withReparse :: (C.Type LanC, Text) -> M (C.RegularField LanC)
    withReparse (ty, name) = pure C.RegularField{
          info   = mkFieldInfo field.info (Just name)
        , typ    = ty
        , offset = field.offset
        , width  = field.width
        , ann    = NoAnn
        }

processImplicitField ::
     C.DeclId
  -> C.ImplicitField PrepareReparse
  -> M (C.ImplicitField ReparseMacroExpansions)
processImplicitField declId field = do
    indirect' <- mapM (processIndirectField declId) field.indirect
    pure C.ImplicitField {
        info     = coercePass field.info
      , typRef   = coercePass field.typRef
      , offset   = field.offset
      , indirect = indirect'
      , ann      = field.ann
      }

processIndirectField ::
     C.DeclId
  -> C.IndirectField PrepareReparse
  -> M (C.IndirectField ReparseMacroExpansions)
processIndirectField declId field =
    reparseWith declId LanC.reparseField field.ann withoutReparse withReparse
  where
    withoutReparse :: C.IndirectField PrepareReparse
    withoutReparse = field

    withReparse :: (C.Type LanC, Text) -> M (C.IndirectField LanC)
    withReparse (ty, name) = pure C.IndirectField{
          info   = mkFieldInfo field.info (Just name)
        , typ    = ty
        , offset = field.offset
        , width  = field.width
        , path   = fmap coercePass field.path
        , ann    = NoAnn
        }

processOpaque ::
     C.Decl l PrepareReparse
  -> C.DeclKind l ReparseMacroExpansions
  -> M (C.Decl l ReparseMacroExpansions)
processOpaque decl kind =
    pure $ reparsedDecl decl kind

processEnum ::
     forall l.
     C.Decl l PrepareReparse
  -> C.Enum PrepareReparse
  -> M (C.Decl l ReparseMacroExpansions)
processEnum decl enum =
    mkDecl <$> mapM processEnumConstant enum.constants
  where
    mkDecl ::
         [C.EnumConstant ReparseMacroExpansions]
      -> C.Decl l ReparseMacroExpansions
    mkDecl enumerators = reparsedDecl decl $ C.DeclEnum C.Enum{
          typ       = coercePass enum.typ
        , constants = enumerators
        , sizeof    = enum.sizeof
        , alignment = enum.alignment
        , ann       = NoAnn
        }

processUntaggedEnumConstant ::
     forall l.
     C.Decl l PrepareReparse
  -> C.UntaggedEnumConstant PrepareReparse
  -> M (C.Decl l ReparseMacroExpansions)
processUntaggedEnumConstant decl enumConst =
    mkDecl <$> processEnumConstant enumConst.constant
  where
    mkDecl ::
         C.EnumConstant ReparseMacroExpansions
      -> C.Decl l ReparseMacroExpansions
    mkDecl constant = reparsedDecl decl $ C.DeclUntaggedEnumConstant C.UntaggedEnumConstant{
          typ      = enumConst.typ
        , constant = constant
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
     C.Decl l PrepareReparse
  -> C.Typedef PrepareReparse
  -> M (C.Decl l ReparseMacroExpansions)
processTypedef decl typedef = do
    typedef' <- reparseWith decl.id LanC.reparseTypedef typedef.ann withoutReparse withReparse
    pure $ reparsedDecl decl $ C.DeclTypedef typedef'
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
     C.Decl l PrepareReparse
  -> C.Function PrepareReparse
  -> M (C.Decl l ReparseMacroExpansions)
processFunction decl function = do
    function' <- reparseWith decl.id LanC.reparseFunDecl function.ann withoutReparse withReparse
    pure $ reparsedDecl decl $ C.DeclFunction function'
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
          name = C.ScopedName <$> mname
        , typ  = typ
        , ann  = NoAnn
        }

-- | Globals (externs or constants)
processGlobal ::
     C.Decl l PrepareReparse
  -> C.Global PrepareReparse
  -> M (C.Decl l ReparseMacroExpansions)
processGlobal decl global = do
    global' <- reparseWith decl.id LanC.reparseGlobal global.ann withoutReparse withReparse
    pure $ reparsedDecl decl $ C.DeclGlobal global'
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
