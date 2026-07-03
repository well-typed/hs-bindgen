-- | Updater for 'ReparseInfo' after preprocessing
--
-- This module is intended to be imported unqualified. It is also intended to
-- only be imported from within the "HsBindgen.Frontend.Pass.PrepareReparse"
-- module hierarchy.
--
-- > import HsBindgen.Frontend.Pass.PrepareReparse.Update
--
module HsBindgen.Frontend.Pass.PrepareReparse.Update (
    UpdateMode (..)
  , update
  ) where

import Prelude hiding (lex, print)

import Control.Monad (forM_)
import Control.Monad.Reader (MonadReader (ask), ReaderT (..))
import Control.Monad.State (MonadState, State, modify, runState)
import Data.Kind
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Lazy qualified as Map

import Clang.HighLevel.Types qualified as Clang

import HsBindgen.Clang.Macros (MacroDefinition, MacroInvocation)
import HsBindgen.Clang.Macros.UniqueExpansion
import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.PrepareReparse.AST
import HsBindgen.Frontend.Pass.PrepareReparse.Flatten
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass.Msg
import HsBindgen.Frontend.Pass.PrepareReparse.Simplifier
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports (Map, mapMaybe)
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Flip
import HsBindgen.Util.Tracer (WithCallStack, withCallStack)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

data UpdateMode a =
    UpdateOnlyFlatten
  | UpdatePreprocessAndFlatten (Map Tag Decl) a

update ::
     forall l.
     UpdateMode [MacroDefinition]
  -> C.TranslationUnit l TypecheckMacros
  -> ( C.TranslationUnit l PrepareReparse
     , [AnnMsg PrepareReparse]
     )
update mode unit =
    ( unit'
    , msgs
    )
  where
    (unitUpdated, delayedMsgs) = runM env $ updateIt () unit

    unit' = unitUpdated {
          C.meta = meta'
        }
    meta' = unitUpdated.meta {
          declIndex = declIndex'
        }

    declIndex' ::  DeclIndex l
    declIndex' =
      -- We use @foldr@ here to establish the original order of messages
      foldr
        DeclIndex.registerDelayedPrepareReparseMsg
        unit.meta.declIndex
        delayedMsgs

    env :: Env
    msgs :: [WithCallStack PrepareReparseMsg]
    (env, msgs) = mkEnv mode

mkCache :: [MacroDefinition] -> (Cache, [WithCallStack PrepareReparseMsg])
mkCache macroDefs = (cache, msgs)
  where
    parseResults = map parseDefinition macroDefs
    cache = precomputeIsExpansionUnique parseResults
    msgs = case NE.nonEmpty $ mapMaybe isFailure parseResults of
        Nothing -> []
        Just failures -> [withCallStack (PrepareReparseMacroDefinitionParseFailures failures)]

mkEnv :: UpdateMode [MacroDefinition] -> (Env, [WithCallStack PrepareReparseMsg])
mkEnv = \case
    UpdateOnlyFlatten ->
      (Env UpdateOnlyFlatten, [])
    UpdatePreprocessAndFlatten preprocessedMap macroDefs ->
      let (cache, msgs) = mkCache macroDefs
      in  ( Env (UpdatePreprocessAndFlatten preprocessedMap cache)
          , msgs
          )

{-------------------------------------------------------------------------------
  Update: class
-------------------------------------------------------------------------------}

class Update a where
  type Ctx a :: Type
  type Ctx a = C.DeclInfo TypecheckMacros
  updateIt :: Ctx a -> a TypecheckMacros -> M (a PrepareReparse)

{-------------------------------------------------------------------------------
  Update: monad
-------------------------------------------------------------------------------}

runM :: Env -> M a -> (a, [(C.DeclId, DelayedPrepareReparseMsg)])
runM m (M k) = fmap (.messages) $ runState (runReaderT k m) (St [])

newtype M a = M (ReaderT Env (State St) a)
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance MonadReader Env M
deriving newtype instance MonadState St M

newtype Env = Env {
    updateMode :: UpdateMode Cache
  }

newtype St = St {
    messages :: [(C.DeclId, DelayedPrepareReparseMsg)]
  }

{-------------------------------------------------------------------------------
  Update: instances
-------------------------------------------------------------------------------}

instance Update (C.TranslationUnit l) where
  type Ctx (C.TranslationUnit l) = ()
  updateIt _ unit = do
      decls' <- mapM (updateIt ()) unit.decls
      pure C.TranslationUnit{
          decls        = decls'
        , includeGraph = unit.includeGraph
        , meta         = unit.meta
        }

instance Update (C.Decl l) where
  type Ctx (C.Decl l) = ()
  updateIt _ decl = do
      kind' <- (updateIt decl.info) decl.kind
      pure C.Decl {
          info = coercePass decl.info
        , kind = kind'
        , ann  = decl.ann
        }

instance Update (C.DeclKind l) where
  updateIt info declKind = case declKind of
      C.DeclStruct struct      -> C.DeclStruct           <$> recurse struct
      C.DeclUnion union        -> C.DeclUnion            <$> recurse union
      C.DeclTypedef typedef    -> C.DeclTypedef          <$> recurse typedef
      C.DeclEnum enum          -> C.DeclEnum             <$> recurse enum
      C.DeclAnonEnumConstant c -> C.DeclAnonEnumConstant <$> recurse c
      C.DeclOpaque mSize       -> pure (C.DeclOpaque mSize)
      C.DeclMacro macro        -> C.DeclMacro            <$> (flipM recurse) macro
      C.DeclFunction function  -> C.DeclFunction         <$> recurse function
      C.DeclGlobal global      -> C.DeclGlobal           <$> recurse global
    where
      recurse :: forall a.
           (Update a, Ctx a ~ C.DeclInfo TypecheckMacros)
        => a TypecheckMacros
        -> M (a PrepareReparse)
      recurse = updateIt info

instance Update C.Struct where
  updateIt info struct = do
      fields' <- mapM (updateIt info) struct.fields
      flam' <- C.traverseFlamField (updateIt info) struct.flam
      pure C.Struct {
          sizeof    = struct.sizeof
        , alignment = struct.alignment
        , fields    = fields'
        , flam      = flam'
        , ann       = struct.ann
        }

instance Update C.StructField where
  updateIt info field = do
      ann' <- updateReparseInfo info (fieldTag info field.info) field.ann
      pure C.StructField {
          info   = coercePass field.info
        , typ    = coercePass field.typ
        , offset = field.offset
        , width  = field.width
        , ann    = ann'
        }

instance Update C.Union where
  updateIt info union = do
      fields' <- mapM (updateIt info) union.fields
      pure C.Union {
          sizeof    = union.sizeof
        , alignment = union.alignment
        , fields    = fields'
        , ann       = union.ann
        }

instance Update C.UnionField where
  updateIt info field = do
      ann' <- updateReparseInfo info (fieldTag info field.info) field.ann
      pure C.UnionField {
          info = coercePass field.info
        , typ  = coercePass field.typ
        , ann  = ann'
        }

instance Update C.Typedef where
  updateIt info typedef = do
      ann' <- updateReparseInfo info (typedefTag info) typedef.ann
      pure C.Typedef {
          typ = coercePass typedef.typ
        , ann = ann'
        }

instance Update C.Enum where
  updateIt _ enum = pure $ coercePass enum

instance Update C.AnonEnumConstant where
  updateIt _ constant = pure $ coercePass constant

instance Update (Flip TypecheckedMacro l) where
  updateIt _info (Flip macro) = pure $ Flip $ coercePassParam macro

instance Update C.Function where
  updateIt info function = do
      ann' <- updateReparseInfo info (functionTag info) function.ann
      pure C.Function {
          args = map coercePass function.args
        , res = coercePass function.res
        , attrs = function.attrs
        , ann = ann'
        }

instance Update C.Global where
  updateIt info global = do
      ann' <- updateReparseInfo info (variableTag info) global.ann
      pure C.Global {
          typ = coercePass global.typ
        , ann = ann'
        }

updateReparseInfo ::
     C.DeclInfo TypecheckMacros
  -> Tag
  -> ReparseInfo Tokens
  -> M (ReparseInfo FlatTokens)
updateReparseInfo info tag@(Tag typ _) reparseInfo = do
    case reparseInfo of
      ReparseNotNeeded -> pure ReparseNotNeeded
      ReparseNeeded tokens macroInvs -> goReparseNeeded tokens macroInvs
  where
    goReparseNeeded :: Tokens -> NonEmpty MacroInvocation -> M (ReparseInfo FlatTokens)
    goReparseNeeded tokens macroInvs = do
        env <- ask
        case env.updateMode of
          UpdateOnlyFlatten -> pure fallback
          UpdatePreprocessAndFlatten preprocessedMap cache -> do
            forM_ failuresMay $ \failures ->
              addMessage info.id (PrepareReparseMacroInvocationParseFailures failures)
            if isExpUniq cache then do
              case Map.lookup tag preprocessedMap of
                Nothing -> do
                  addMessage info.id PrepareReparseNoPreprocessorOutput
                  pure fallback
                Just (Decl preppedTokens) ->
                  pure $ ReparseNeeded (mkFlatTokens preppedTokens) macroInvs
            else do
              addMessage info.id PrepareReparseExpansionNotUnique
              pure fallback
      where
        fallback :: ReparseInfo FlatTokens
        fallback = mkReparseNeeded $ mkFlatTokens $ case typ of
            Function -> flattenFunction tokens
            _        -> flattenDefault tokens

        mkReparseNeeded :: FlatTokens -> ReparseInfo FlatTokens
        mkReparseNeeded flatTokens = ReparseNeeded flatTokens macroInvs

        mkFlatTokens :: String -> FlatTokens
        mkFlatTokens flatten = FlatTokens {
              flatten = flatten
            , locStart = getLocation tokens
            }

        parseResults = fmap parseInvocation macroInvs
        failuresMay = NE.nonEmpty $ mapMaybe isFailure $ NE.toList parseResults
        isExpUniq cache = all (cachedIsExpansionUnique cache) parseResults

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

getLocation :: [Clang.Token a] -> Clang.MultiLoc
getLocation []    = panicPure "Unexpected empty list of tokens"
getLocation (t:_) = Clang.rangeStart $ Clang.tokenExtent t

addMessage :: C.DeclId -> DelayedPrepareReparseMsg -> M ()
addMessage did msg = modify $ \st -> st {
      messages = (did, msg) : st.messages
    }
