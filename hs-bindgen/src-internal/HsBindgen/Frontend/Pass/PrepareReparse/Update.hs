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

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.DelayedMsg
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.Pass.ApplyMacroFallback.IsPass
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
import HsBindgen.Macro.Syntax (MacroInvocation)
import HsBindgen.Macro.UniqueExpansion
import HsBindgen.Macro.UniqueExpansion.Types (Ambiguity, Name)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

data UpdateMode =
    UpdateOnlyFlatten
  | UpdatePreprocessAndFlatten (Map Tag Decl)

update ::
     forall l.
     UpdateMode
  -> C.TranslationUnit l ApplyMacroFallback
  -> C.TranslationUnit l PrepareReparse
update mode unit = unit'
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
        (DeclIndex.registerDelayedMsg . fmap DelayedPrepareReparse)
        unit.meta.declIndex
        delayedMsgs

    env :: Env
    env = Env{
        updateMode  = mode
      , ambiguityOf = (`DeclIndex.lookupAmbiguity` unit.meta.declIndex)
      }

{-------------------------------------------------------------------------------
  Update: class
-------------------------------------------------------------------------------}

class Update a where
  type Ctx a :: Type
  type Ctx a = C.DeclId
  updateIt :: Ctx a -> a ApplyMacroFallback -> M (a PrepareReparse)

{-------------------------------------------------------------------------------
  Update: monad
-------------------------------------------------------------------------------}

runM :: Env -> M a -> (a, [(C.DeclId, DelayedPrepareReparseMsg)])
runM m (M k) = fmap (.messages) $ runState (runReaderT k m) (St [])

newtype M a = M (ReaderT Env (State St) a)
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance MonadReader Env M
deriving newtype instance MonadState St M

data Env = Env {
    updateMode  :: UpdateMode
  , ambiguityOf :: Name -> Ambiguity
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
      kind' <- (updateIt decl.id) decl.kind
      pure $ C.coercePassDeclWith kind' decl.ann decl

instance Update (C.DeclKind l) where
  updateIt declId declKind = case declKind of
      C.DeclStruct struct          -> C.DeclStruct               <$> recurse struct
      C.DeclUnion union            -> C.DeclUnion                <$> recurse union
      C.DeclTypedef typedef        -> C.DeclTypedef              <$> recurse typedef
      C.DeclEnum enum              -> C.DeclEnum                 <$> recurse enum
      C.DeclUntaggedEnumConstant c -> C.DeclUntaggedEnumConstant <$> recurse c
      C.DeclOpaque mSize           -> pure (C.DeclOpaque mSize)
      C.DeclMacro macro            -> C.DeclMacro                <$> (flipM recurse) macro
      C.DeclFunction function      -> C.DeclFunction             <$> recurse function
      C.DeclGlobal global          -> C.DeclGlobal               <$> recurse global
    where
      recurse :: forall a.
           (Update a, Ctx a ~ C.DeclId)
        => a ApplyMacroFallback
        -> M (a PrepareReparse)
      recurse = updateIt declId

instance Update C.Struct where
  updateIt declId struct = do
      fields' <- mapM (updateIt declId) struct.fields
      flam' <- C.traverseFlamField (updateIt declId) struct.flam
      pure C.Struct {
          sizeof    = struct.sizeof
        , alignment = struct.alignment
        , fields    = fields'
        , flam      = flam'
        , ann       = struct.ann
        }


instance Update C.Union where
  updateIt declId union = do
      fields' <- mapM (updateIt declId) union.fields
      pure C.Union {
          sizeof    = union.sizeof
        , alignment = union.alignment
        , fields    = fields'
        , ann       = union.ann
        }

instance Update C.Field where
  updateIt declId = C.mapMField (updateIt declId) (updateIt declId)

instance Update C.RegularField where
  updateIt declId field = do
      ann' <- updateReparseInfo declId (fieldTag declId field.info) field.ann
      pure C.RegularField {
          info   = coercePass field.info
        , typ    = coercePass field.typ
        , offset = field.offset
        , width  = field.width
        , ann    = ann'
        }

instance Update C.ImplicitField where
  updateIt declId field = do
      indirect' <- mapM (updateIt declId) field.indirect
      pure C.ImplicitField {
          info     = coercePass field.info
        , typRef   = coercePass field.typRef
        , offset   = field.offset
        , indirect = indirect'
        , ann      = field.ann
        }

instance Update C.IndirectField where
  updateIt declId field = do
      ann' <- updateReparseInfo declId (fieldTag declId field.info) field.ann
      pure C.IndirectField {
          info   = coercePass field.info
        , typ    = coercePass field.typ
        , offset = field.offset
        , width  = field.width
        , path   = fmap coercePass field.path
        , ann    = ann'
        }

instance Update C.Typedef where
  updateIt declId typedef = do
      ann' <- updateReparseInfo declId (typedefTag declId) typedef.ann
      pure C.Typedef {
          typ = coercePass typedef.typ
        , ann = ann'
        }

instance Update C.Enum where
  updateIt _ enum = pure $ coercePass enum

instance Update C.UntaggedEnumConstant where
  updateIt _ constant = pure $ coercePass constant

instance Update (Flip TypecheckedMacro l) where
  updateIt _declId (Flip macro) = pure $ Flip $ coercePassParam macro

instance Update C.Function where
  updateIt declId function = do
      ann' <- updateReparseInfo declId (functionTag declId) function.ann
      pure C.Function {
          args = map coercePass function.args
        , res = coercePass function.res
        , attrs = function.attrs
        , ann = ann'
        }

instance Update C.Global where
  updateIt declId global = do
      ann' <- updateReparseInfo declId (variableTag declId) global.ann
      pure C.Global {
          typ = coercePass global.typ
        , ann = ann'
        }

updateReparseInfo ::
     C.DeclId
  -> Tag
  -> ReparseInfo Tokens
  -> M (ReparseInfo FlatTokens)
updateReparseInfo declId tag@(Tag typ _) reparseInfo = do
    case reparseInfo of
      ReparseNotNeeded -> pure ReparseNotNeeded
      ReparseNeeded tokens macroInvs -> goReparseNeeded tokens macroInvs
  where
    goReparseNeeded :: Tokens -> NonEmpty MacroInvocation -> M (ReparseInfo FlatTokens)
    goReparseNeeded tokens macroInvs = do
        env <- ask
        case env.updateMode of
          UpdateOnlyFlatten -> pure fallback
          UpdatePreprocessAndFlatten preprocessedMap -> do
            forM_ failuresMay $ \failures ->
              addMessage declId (PrepareReparseMacroInvocationParseFailures failures)
            if all (isExpansionUnique env.ambiguityOf) parseResults then do
              case Map.lookup tag preprocessedMap of
                Nothing -> do
                  addMessage declId PrepareReparseNoPreprocessorOutput
                  pure fallback
                Just (Decl preppedTokens) ->
                  pure $ ReparseNeeded (mkFlatTokens preppedTokens) macroInvs
            else do
              addMessage declId $
                PrepareReparseExpansionNotUnique fallbackTokens.locStart fallbackTokens.flatten
              pure fallback
      where
        fallback :: ReparseInfo FlatTokens
        fallback = mkReparseNeeded fallbackTokens

        fallbackTokens :: FlatTokens
        fallbackTokens = mkFlatTokens $ case typ of
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

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

getLocation :: [Clang.Token Clang.SourcePath a] -> Clang.MultiLoc Clang.SourcePath
getLocation []    = panicPure "Unexpected empty list of tokens"
getLocation (t:_) = Clang.rangeStart $ Clang.tokenExtent t

addMessage :: C.DeclId -> DelayedPrepareReparseMsg -> M ()
addMessage did msg = modify $ \st -> st {
      messages = (did, msg) : st.messages
    }
