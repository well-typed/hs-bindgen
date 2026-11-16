module HsBindgen.Frontend.Pass.TranslateMacros (
    translateMacros
  ) where

import Control.Monad.Reader (MonadReader, ReaderT (..), asks, withReaderT)
import Control.Monad.State (MonadState, State, modify, runState)
import Data.Proxy (Proxy (Proxy))

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.DeclDB (DeclDB)
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.DelayedMsg
import HsBindgen.Frontend.Pass.TranslateMacros.IsPass
import HsBindgen.Frontend.Pass.TranslateMacros.IsPass.Msg
import HsBindgen.Frontend.Pass.TranslateMacros.TranslatedMacro
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
        DeclDB.traverseActive DeclDB.AnyOrder
          (fmap DeclDB.Advanced . processDecl)
          unit.db

    globalEnv = GlobalEnv { macroLang = macroLang }

{-------------------------------------------------------------------------------
  Decls
-------------------------------------------------------------------------------}

processDecl ::
     Macro.HasTypes l
  => C.Decl l PassIn
  -> GM l (C.Decl l TranslateMacros)
processDecl decl = do
    kind' <- withDeclEnv $ processDeclKind decl.kind
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
processStruct = pure . coercePass

processUnion :: C.Union PassIn -> DM l (C.Union TranslateMacros)
processUnion = pure . coercePass

processTypedef :: C.Typedef PassIn -> DM l (C.Typedef TranslateMacros)
processTypedef = pure . coercePass

processEnum :: C.Enum PassIn -> DM l (C.Enum TranslateMacros)
processEnum = pure . coercePass

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
    macroLang <- asks (.macroLang)
    pure TranslatedMacroType {
        typ = macroLang.translateType (fmap macroVarToHsType macroType.body)
      , ann = macroType.ann
      }
  where
    -- TODO <https://github.com/well-typed/hs-bindgen/issues/2357>: annotate
    -- macro types with correct underlying types
    macroVarToHsType :: MacroTypeBodyVar PassIn -> Hs.Type
    macroVarToHsType = \case
      MacroTypeExtBinding ext ->
        Hs.ExtBinding ext.hsName ext.cSpec ext.hsSpec
      MacroTypeBodyVar pair ->
        Hs.TypRef (Hs.assertNs (Proxy @Hs.NsTypeConstr) pair.hsName) Nothing

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
runM m (M k) = fmap (reverse . (.messages)) $ runState (runReaderT k m) (St [])

newtype M env a = M (ReaderT env (State St) a)
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance MonadReader env (M env)
deriving newtype instance MonadState St (M env)

withEnv :: (env1 -> env2) -> M env2 a -> M env1 a
withEnv f (M k) = M (withReaderT f k)

-- | Monad 'M' with a global environment
type GM l = M (GlobalEnv l)
-- | Monad 'M' with a decl-local environment
type DM l = M (DeclEnv   l)

withDeclEnv :: DM l a -> GM l a
withDeclEnv = withEnv $ \ge -> DeclEnv { macroLang = ge.macroLang }

data GlobalEnv l = GlobalEnv {
    macroLang :: Macro.Lang l
  }

data DeclEnv l = DeclEnv {
    macroLang :: Macro.Lang l
  }

newtype St = St {
    -- | Delayed trace messages in reverse order that they were recorded
    messages :: [(C.DeclId, DelayedTranslateMacrosMsg)]
  }

_addMessage :: C.DeclId -> DelayedTranslateMacrosMsg -> M env ()
_addMessage did msg = modify $ \st -> st {
      messages = (did, msg) : st.messages
    }
