module HsBindgen.Frontend.Pass.TranslateMacros (
    translateMacros
  ) where

import Control.Monad.Reader (MonadReader, ReaderT (..), withReaderT)
import Control.Monad.State (MonadState, State, modify, runState)

import HsBindgen.Frontend.DeclDB (DeclDB)
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.DelayedMsg
import HsBindgen.Frontend.Pass.TranslateMacros.IsPass
import HsBindgen.Frontend.Pass.TranslateMacros.IsPass.Msg
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

type PassIn  = TranslateTypes

translateMacros ::
     forall l.
     C.TranslationUnit l PassIn
  -> ( C.TranslationUnit l TranslateMacros
     , [AnnMsg TranslateMacros]
     )
translateMacros unit = (unit{C.db = db'}, msgs)
  where
    msgs = []

    db' :: DeclDB l TranslateMacros
    db' =
        DeclDB.addDelayedMsgs
          (map (fmap DelayedTranslateMacros) delayedMsgs)
          db

    (db, delayedMsgs) =
      runM NoEnv $
        DeclDB.traverseActive DeclDB.AnyOrder
          (fmap DeclDB.Advanced . processDecl)
          unit.db

{-------------------------------------------------------------------------------
  Decls
-------------------------------------------------------------------------------}

processDecl :: C.Decl l PassIn -> M NoEnv (C.Decl l TranslateMacros)
processDecl decl = do
    kind' <- withEnv (const (DeclEnv decl.info)) $ processDeclKind decl.kind
    pure $ C.coercePassDeclWith kind' decl.ann decl

processDeclKind :: C.DeclKind l PassIn -> M DeclEnv (C.DeclKind l TranslateMacros)
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

processStruct :: C.Struct PassIn -> M DeclEnv (C.Struct TranslateMacros)
processStruct = pure . coercePass

processUnion :: C.Union PassIn -> M DeclEnv (C.Union TranslateMacros)
processUnion = pure . coercePass

processTypedef :: C.Typedef PassIn -> M DeclEnv (C.Typedef TranslateMacros)
processTypedef = pure . coercePass

processEnum :: C.Enum PassIn -> M DeclEnv (C.Enum TranslateMacros)
processEnum = pure . coercePass

processUntaggedEnumConstant ::
     C.UntaggedEnumConstant PassIn
  -> M DeclEnv (C.UntaggedEnumConstant TranslateMacros)
processUntaggedEnumConstant = pure . coercePass

processMacro :: MacroBody PassIn l -> M DeclEnv (MacroBody TranslateMacros l)
processMacro mac = pure $ case mac of
    MacroType  typ -> MacroType  $ coercePass typ
    MacroValue val -> MacroValue $ coercePass val
    MacroRaw   raw -> MacroRaw raw

processFunction :: C.Function PassIn -> M DeclEnv (C.Function TranslateMacros)
processFunction = pure . coercePass

processGlobal :: C.Global PassIn -> M DeclEnv (C.Global TranslateMacros)
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

data NoEnv = NoEnv

data DeclEnv = DeclEnv {
    _info :: C.DeclInfo
  }

newtype St = St {
    -- | Delayed trace messages in reverse order that they were recorded
    messages :: [(C.DeclId, DelayedTranslateMacrosMsg)]
  }

_addMessage :: C.DeclId -> DelayedTranslateMacrosMsg -> M env ()
_addMessage did msg = modify $ \st -> st {
      messages = (did, msg) : st.messages
    }
