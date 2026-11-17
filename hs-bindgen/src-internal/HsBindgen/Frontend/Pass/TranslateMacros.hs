module HsBindgen.Frontend.Pass.TranslateMacros (
    translateMacros
  ) where

import Control.Monad.Reader (MonadReader, ReaderT (..), asks, withReaderT)
import Control.Monad.State (MonadState, State, gets, modify, runState)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (Proxy))

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.DeclDB (DeclDB)
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.DelayedMsg
import HsBindgen.Frontend.Pass.TranslateMacros.IsPass
import HsBindgen.Frontend.Pass.TranslateMacros.IsPass.Msg
import HsBindgen.Frontend.Pass.TranslateMacros.TranslatedMacro
import HsBindgen.Frontend.Pass.TranslateMacros.UnderlyingType
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Pass
import HsBindgen.IR.Translation
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

type PassIn  = TranslateTypes

translateMacros ::
     forall l. Macro.HasTypes l
  => Macro.Lang l
  -> C.TranslationUnit l PassIn
  -> ( C.TranslationUnit l TranslateMacros
     , [AnnMsg TranslateMacros]
     )
translateMacros macroLang unit = (unit{C.db = db'}, msgs)
  where
    msgs = []

    db' :: DeclDB l TranslateMacros
    db' =
        DeclDB.addDelayedMsgs
          (map (fmap DelayedTranslateMacros) delayedMsgs)
          db

    (db, delayedMsgs) =
      runM globalEnv $
        DeclDB.traverseActive
          (DeclDB.DependencyOrder DeclDB.AllEdges tieBreaker)
          (fmap DeclDB.Advanced . processDecl)
          unit.db

    -- Trivial tie breaker
    tieBreaker = const (0 :: Int)

    globalEnv = GlobalEnv { macroLang = macroLang }

{-------------------------------------------------------------------------------
  Active decls
-------------------------------------------------------------------------------}

processDecl ::
     Macro.HasTypes l
  => C.Decl l PassIn
  -> GM l (C.Decl l TranslateMacros)
processDecl decl = do
    kind' <- withDeclEnv decl.id.cName $ processDeclKind decl.kind
    pure $ C.coercePassDeclWith kind' decl.ann decl

processDeclKind ::
     Macro.HasTypes l
  => C.DeclKind l PassIn
  -> DM l (C.DeclKind l TranslateMacros)
processDeclKind = \case
    C.DeclStruct               struct   -> C.DeclStruct               <$> processStruct           struct
    C.DeclUnion                union    -> C.DeclUnion                <$> processUnion            union
    C.DeclTypedef              typedef  -> C.DeclTypedef              <$> processTypedef          typedef
    C.DeclEnum                 enum     -> C.DeclEnum                 <$> processEnum             enum
    C.DeclUntaggedEnumConstant cnst     -> C.DeclUntaggedEnumConstant <$> processUntaggedEnumConstant cnst
    C.DeclOpaque               mSize    -> pure $ C.DeclOpaque mSize
    C.DeclMacro                macro    -> C.DeclMacro                <$> processMacro            macro
    C.DeclFunction             function -> C.DeclFunction             <$> processFunction         function
    C.DeclGlobal               global   -> C.DeclGlobal               <$> processGlobal           global

processStruct :: C.Struct PassIn -> DM l (C.Struct TranslateMacros)
processStruct struct = do
    registerUnderlyingTypeM NoUnderlyingType
    pure $ coercePass struct

processUnion :: C.Union PassIn -> DM l (C.Union TranslateMacros)
processUnion union = do
    registerUnderlyingTypeM NoUnderlyingType
    pure $ coercePass union

processTypedef :: C.Typedef PassIn -> DM l (C.Typedef TranslateMacros)
processTypedef typedef = do
    registerUnderlyingTypeM (UnderlyingType (coercePassTypes @PassIn @TranslateMacros Proxy typedef.typ).hs)
    pure $ coercePass typedef

processEnum :: C.Enum PassIn -> DM l (C.Enum TranslateMacros)
processEnum enum = do
    registerUnderlyingTypeM (UnderlyingType (coercePassTypes @PassIn @TranslateMacros Proxy enum.typ).hs)
    pure $ coercePass enum

processUntaggedEnumConstant ::
     C.UntaggedEnumConstant PassIn
  -> DM l (C.UntaggedEnumConstant TranslateMacros)
processUntaggedEnumConstant = pure . coercePass

processMacro ::
     Macro.HasTypes l
  => MacroBody PassIn l
  -> DM l (MacroBody TranslateMacros l)
processMacro mac = case mac of
    MacroType  typ -> TMacroType  <$> processMacroType typ
    MacroValue val -> pure $ TMacroValue $ coercePass val
    MacroRaw   raw -> pure $ TMacroRaw raw

processMacroType ::
     Macro.HasTypes l
  => TypecheckedMacroType l PassIn
  -> DM l (TranslatedMacroType TranslateMacros)
processMacroType macroType = do
    body' <- mapM macroVarToHsType macroType.body
    macroLang <- asks (.macroLang)
    let typ' = macroLang.translateType body'
    registerUnderlyingTypeM (UnderlyingType typ')
    pure $ TranslatedMacroType {
        typ = typ'
      , ann = macroType.ann
      }
  where
    macroVarToHsType :: MacroTypeBodyVar PassIn -> DM l Hs.Type
    macroVarToHsType = \case
      MacroTypeExtBinding ext -> do
        pure $ Hs.ExtBinding ext.hsName ext.cSpec ext.hsSpec
      MacroTypeBodyVar pair -> do
        ty <- lookupUnderlyingTypeM pair.cName <&> \case
                MissingUnderlyingType                        -> Nothing
                RegisteredUnderlyingType NoUnderlyingType    -> Nothing
                RegisteredUnderlyingType (UnderlyingType ty) -> Just ty
        pure $ Hs.TypRef (Hs.assertNs (Proxy @Hs.NsTypeConstr) pair.hsName) ty

processFunction :: C.Function PassIn -> DM l (C.Function TranslateMacros)
processFunction = pure . coercePass

processGlobal :: C.Global PassIn -> DM l (C.Global TranslateMacros)
processGlobal = pure . coercePass

{-------------------------------------------------------------------------------
  Monad
-------------------------------------------------------------------------------}

-- | Run the monad
--
-- Delayed trace messages are returned in the order they were recorded
runM :: env -> M env a -> (a, [(C.DeclId, DelayedTranslateMacrosMsg)])
runM m (M k) = fmap (reverse . (.messages)) $ runState (runReaderT k m) initSt

newtype M env a = M (ReaderT env (State St) a)
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance MonadReader env (M env)
deriving newtype instance MonadState St (M env)

-- | Monad 'M' with a global environment
type GM  l = M (GlobalEnv l)
-- | Monad 'M' with a decl-local environment
type DM  l = M (DeclEnv   l)

{-------------------------------------------------------------------------------
  Reader
-------------------------------------------------------------------------------}

withEnv :: (env1 -> env2) -> M env2 a -> M env1 a
withEnv f (M k) = M (withReaderT f k)

withDeclEnv :: C.DeclId -> DM l a -> GM l a
withDeclEnv declId = withEnv $ \ge -> DeclEnv {
      macroLang = ge.macroLang
    , declId    = declId
    }

data GlobalEnv l = GlobalEnv {
    macroLang :: Macro.Lang l
  }

data DeclEnv l = DeclEnv {
    macroLang :: Macro.Lang l
  , declId    :: C.DeclId
  }

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

data St = St {
    -- | Delayed trace messages in reverse order that they were recorded
    messages :: [(C.DeclId, DelayedTranslateMacrosMsg)]
    -- | Underlying types
  , underlyingTypes :: UnderlyingTypes
  }

initSt :: St
initSt = St { messages = [], underlyingTypes = Map.empty }

addMessage ::
     (MonadReader (DeclEnv l) m, MonadState St m)
  => DelayedTranslateMacrosMsg -> m ()
addMessage msg = do
    declId <- asks (.declId)
    modify $ \st -> st {
        messages = (declId, msg) : st.messages
      }

registerUnderlyingTypeM ::
     (MonadReader (DeclEnv l) m, MonadState St m)
  => RegisteredUnderlyingType -> m ()
registerUnderlyingTypeM ty = do
    declId          <- asks (.declId)
    underlyingTypes <- gets (.underlyingTypes)
    case registerUnderlyingType declId ty underlyingTypes of
      Left err               -> do
        addMessage $ TranslateMacrosRegisterUnderlyingTypeDuplicate err
      Right underlyingTypes' -> do
        modify $ \st -> st{ underlyingTypes = underlyingTypes' }
        addMessage $ TranslateMacrosRegisterUnderlyingType ty

lookupUnderlyingTypeM ::
     (MonadReader (DeclEnv l) m, MonadState St m)
  => C.DeclId -> m UnderlyingType
lookupUnderlyingTypeM declId = do
    underlyingTypes <- gets (.underlyingTypes)
    let ty = lookupUnderlyingType declId underlyingTypes
    addMessage $ TranslateMacrosLookupUnderlyingType declId ty
    pure ty
