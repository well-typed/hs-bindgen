-- | Updater for 'ReparseInfo' after preprocessing
--
-- This module is intended to be imported unqualified. It is also intended to
-- only be imported from within the "HsBindgen.Frontend.Pass.PrepareReparse"
-- module hierarchy.
--
-- > import HsBindgen.Frontend.Pass.PrepareReparse.Update
--
module HsBindgen.Frontend.Pass.PrepareReparse.Update (
    update
  ) where

import Prelude hiding (lex, print)

import Control.Monad (forM_)
import Control.Monad.Reader (MonadReader (ask), ReaderT (..))
import Control.Monad.State (MonadState, State, modify, runState)
import Data.Kind
import Data.List.NonEmpty qualified as NE
import Data.Map.Lazy qualified as Map

import Clang.HighLevel.Types qualified as Clang

import HsBindgen.Clang.Macros (MacroDefinition)
import HsBindgen.Clang.Macros.UniqueExpansion (ParseResult, isExpansionUnique,
                                               isFailure, parseDefinition,
                                               parseInvocation)
import HsBindgen.Clang.Macros.UniqueExpansion.Types (Definition)
import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.TranslationUnit qualified as C
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass (AMsg)
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.PrepareReparse.AST
import HsBindgen.Frontend.Pass.PrepareReparse.Flatten
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass.Msg
import HsBindgen.Frontend.Pass.PrepareReparse.Simplifier
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports (Map, mapMaybe)
import HsBindgen.Macro.Type
import HsBindgen.Util.Tracer (WithCallStack, withCallStack)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

update ::
     forall l.
     Maybe (Map Tag Decl)
  -> [MacroDefinition]
  -> C.TranslationUnit l TypecheckMacros
  -> ( C.TranslationUnit l PrepareReparse
     , [AMsg PrepareReparse]
     )
update mapping macroDefs unit =
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
    (env, msgs) = mkEnv mapping macroDefs

mkEnv :: Maybe (Map Tag Decl) -> [MacroDefinition] -> (Env, [AMsg PrepareReparse])
mkEnv mapping macroDefs =  (Env mapping parseResults, msgs)
  where
    parseResults = map parseDefinition macroDefs
    msgs = case NE.nonEmpty $ mapMaybe isFailure parseResults of
        Nothing -> []
        Just failures -> [withCallStack (PrepareReparseMacroDefinitionParseFailures failures)]

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

runM :: Env -> M a -> (a, [(DeclId, DelayedPrepareReparseMsg)])
runM m (M k) = fmap (.messages) $ runState (runReaderT k m) (St [])

newtype M a = M (ReaderT Env (State St) a)
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance MonadReader Env M
deriving newtype instance MonadState St M

data Env = Env {
    -- 'Nothing' means we could not expand macro invocations (for any of a
    -- variety of reasons). We default to just flattening tokens at this
    -- point.
    map       :: Maybe (Map Tag Decl)
  , macroDefs :: [ParseResult Definition]
  }

newtype St = St {
    messages :: [(DeclId, DelayedPrepareReparseMsg)]
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
      C.DeclOpaque             -> pure C.DeclOpaque
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
      flam' <- mapM (updateIt info) struct.flam
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
    env <- ask
    case reparseInfo of
      ReparseNotNeeded -> pure ReparseNotNeeded
      ReparseNeeded tokens macroInvs -> do
        let parseResults = fmap parseInvocation macroInvs
        let failuresMay = NE.nonEmpty $ mapMaybe isFailure $ NE.toList parseResults
        forM_ failuresMay $ \failures ->
                  addMessage info.id (PrepareReparseMacroInvocationParseFailures failures)
        let uniqueExp = all (isExpansionUnique env.macroDefs) parseResults
        let fallback = case typ of
              Function -> flattenFunction tokens
              _        -> flattenDefault tokens
        flatten <-
          case env.map of
            -- If this is 'Nothing', we have already traced a reason why (see
            -- 'DelayedPrepareReparseMsg').
            Nothing -> pure fallback
            Just mapping
              | uniqueExp
              -> case Map.lookup tag mapping of
                    Nothing -> do
                      addMessage info.id PrepareReparseFailed
                      pure fallback
                    Just (Decl dec) -> pure dec
              | otherwise
              -> do
                  addMessage info.id PrepareReparseExpansionNotUnique
                  pure fallback
        let flatTokens = FlatTokens {
                flatten = flatten
              , locStart = getLocation tokens
              }
        pure $ ReparseNeeded flatTokens macroInvs

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

getLocation :: [Clang.Token a] -> Clang.MultiLoc
getLocation []    = panicPure "Unexpected empty list of tokens"
getLocation (t:_) = Clang.rangeStart $ Clang.tokenExtent t

addMessage :: DeclId -> DelayedPrepareReparseMsg -> M ()
addMessage did msg = modify $ \st -> st {
      messages = (did, msg) : st.messages
    }
