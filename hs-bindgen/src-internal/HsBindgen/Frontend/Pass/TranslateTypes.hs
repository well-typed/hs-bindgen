module HsBindgen.Frontend.Pass.TranslateTypes (
    translateTypes
  ) where

import Control.Monad (zipWithM)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks, withReaderT)
import Control.Monad.State (MonadState, State, gets, modify, runState)
import Control.Monad.Writer.Strict (runWriter)
import Data.Functor ((<&>))

import HsBindgen.Frontend.DeclDB (DeclDB)
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.DelayedMsg
import HsBindgen.Frontend.Pass.MangleNames.IsPass (MangleNames)
import HsBindgen.Frontend.Pass.TranslateTypes.Error
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.Msg
import HsBindgen.Frontend.Pass.TranslateTypes.PassingMethod (Classified,
                                                             classifyArgPassingMethod,
                                                             classifyResPassingMethod)
import HsBindgen.Frontend.Pass.TranslateTypes.Position
import HsBindgen.Frontend.Pass.TranslateTypes.SugarInfo
import HsBindgen.Frontend.Pass.TranslateTypes.Translation qualified as Translation
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.FunSig
import HsBindgen.IR.Pass
import HsBindgen.IR.Translation

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

translateTypes ::
     forall l.
     C.TranslationUnit l MangleNames
  -> C.TranslationUnit l TranslateTypes
translateTypes unit = unit{C.db = db'}
  where
    db' :: DeclDB l TranslateTypes
    db' =
        DeclDB.addDelayedMsgs
          (map (second DelayedTranslateTypes) delayedMsgs)
          db

    (db, delayedMsgs) =
      runM globalEnv $
        DeclDB.traverseActive
          (DeclDB.DependencyOrder DeclDB.AllEdges tieBreaker)
          processDecl
          unit.db

    -- Trivial tie breaker
    tieBreaker = const (0 :: Int)

    globalEnv = GlobalEnv

{-------------------------------------------------------------------------------
  Decls
-------------------------------------------------------------------------------}

processDecl ::
     C.Decl l MangleNames
  -> GM l (DeclDB.PassResult l TranslateTypes)
processDecl decl = do
    eKind' <- withDeclEnv decl.id $ processDeclKind decl.kind
    pure $ case eKind' of
      Left  err   -> DeclDB.Fail $ DeclDB.TranslateTypesFailed err
      Right kind' -> DeclDB.Advanced $ C.coercePassDeclWith kind' decl.ann decl

processDeclKind ::
     C.DeclKind l MangleNames
  -> DM l (Either TranslateTypesError (C.DeclKind l TranslateTypes))
processDeclKind = \case
    C.DeclStruct               struct   -> ok $ C.DeclStruct               <$> processStruct           struct
    C.DeclUnion                union    -> ok $ C.DeclUnion                <$> processUnion            union
    C.DeclTypedef              typedef  -> ok $ C.DeclTypedef              <$> processTypedef          typedef
    C.DeclEnum                 enum     -> ok $ C.DeclEnum                 <$> processEnum             enum
    C.DeclUntaggedEnumConstant cnst     -> ok $ C.DeclUntaggedEnumConstant <$> processUntaggedEnumConstant cnst
    C.DeclOpaque               mSize    -> ok $ pure $ C.DeclOpaque mSize
    C.DeclMacro                macro    -> ok $ C.DeclMacro                <$> processMacro            macro
    C.DeclFunction             function -> second C.DeclFunction           <$> processFunction         function
    C.DeclGlobal               global   -> ok $ C.DeclGlobal               <$> processGlobal           global
  where
    ok :: DM l a -> DM l (Either TranslateTypesError a)
    ok = fmap Right

processStruct :: C.Struct MangleNames -> DM l (C.Struct TranslateTypes)
processStruct struct = do
    registerSugarInfoM NotSugar
    fields' <- mapM processField struct.fields
    flam'   <- C.traverseFlamField processRegularField struct.flam
    pure C.Struct{
        sizeof    = struct.sizeof
      , alignment = struct.alignment
      , fields    = fields'
      , flam      = flam'
      , ann       = struct.ann
      }

processUnion :: C.Union MangleNames -> DM l (C.Union TranslateTypes)
processUnion union = do
    registerSugarInfoM NotSugar
    fields' <- mapM processField union.fields
    pure C.Union{
        sizeof    = union.sizeof
      , alignment = union.alignment
      , fields    = fields'
      , ann       = union.ann
      }

processField :: C.Field MangleNames -> DM l (C.Field TranslateTypes)
processField = \case
    C.FieldRegular  field -> C.FieldRegular  <$> processRegularField field
    C.FieldImplicit field -> C.FieldImplicit <$> processImplicitField field

processRegularField :: C.RegularField MangleNames -> DM l (C.RegularField TranslateTypes)
processRegularField field = pure C.RegularField{
      info   = coercePass field.info
    , typ    = processType Translation.Top field.typ
    , offset = field.offset
    , width  = field.width
    , ann    = field.ann
    }

processImplicitField :: C.ImplicitField MangleNames -> DM l (C.ImplicitField TranslateTypes)
processImplicitField field = do
    indirect' <- mapM processIndirectField field.indirect
    pure C.ImplicitField{
        info     = coercePass field.info
      , typRef   = processType Translation.Top field.typRef
      , offset   = field.offset
      , indirect = indirect'
      , ann      = field.ann
      }

processIndirectField :: C.IndirectField MangleNames -> DM l (C.IndirectField TranslateTypes)
processIndirectField field = pure C.IndirectField{
      info   = coercePass field.info
    , typ    = processType Translation.Top field.typ
    , offset = field.offset
    , width  = field.width
    , path   = map (processType Translation.Top) field.path
    , ann    = coercePassAnn (Proxy @'("IndirectField", MangleNames, TranslateTypes)) field.ann
    }

processTypedef :: C.Typedef MangleNames -> DM l (C.Typedef TranslateTypes)
processTypedef typedef = do
    let typ' = processType Translation.Top typedef.typ
    registerSugarInfoM $ Sugar typ'.hs
    pure C.Typedef{
        typ = typ'
      , ann = typedef.ann
      }

processEnum :: C.Enum MangleNames -> DM l (C.Enum TranslateTypes)
processEnum enum = do
    let typ' = processType Translation.Top enum.typ
    registerSugarInfoM $ Sugar typ'.hs
    pure C.Enum{
        typ       = typ'
      , sizeof    = enum.sizeof
      , alignment = enum.alignment
      , constants = map coercePass enum.constants
      , ann       = enum.ann
      }

processUntaggedEnumConstant ::
     C.UntaggedEnumConstant MangleNames
  -> DM l (C.UntaggedEnumConstant TranslateTypes)
processUntaggedEnumConstant = pure . coercePass

processMacro :: MacroBody MangleNames l -> DM l (MacroBody TranslateTypes l)
processMacro mac = pure $ case mac of
    MacroType  typ -> MacroType  $ coercePass typ
    MacroValue val -> MacroValue $ coercePass val
    MacroRaw   raw -> MacroRaw raw

processFunction ::
     C.Function MangleNames
  -> DM l (Either TranslateTypesError (C.Function TranslateTypes))
processFunction fun = do
    eArgs <- zipWithM processFunctionArg [1..] fun.args
    eRes  <- processFunctionRes fun.res
    pure $ case collectErrors FunSig{args = eArgs, res = eRes} of
      Left errs -> Left $ TranslateTypesFunctionError errs
      Right sig -> Right C.Function{
          args  = sig.args
        , res   = sig.res
        , attrs = fun.attrs
        , ann   = fun.ann
        }

processFunctionArg ::
     Int
  -> C.FunctionArg MangleNames
  -> DM l (Either FunPositionError (C.FunctionArg TranslateTypes))
processFunctionArg i arg = do
    eAnn <- classified pos $ classifyArgPassingMethod pos typ'.c.typ typ'.hs
    pure $ eAnn <&> \ann -> C.FunctionArg{
        name = arg.name
      , typ  = typ'
      , ann  = ann
      }
  where
    pos  = FunPositionArg FunArgPosition{
        index = i
      , name  = (.cName) <$> arg.name
      }
    typ' = processType Translation.FunArg arg.typ

processFunctionRes ::
     C.FunctionRes MangleNames
  -> DM l (Either FunPositionError (C.FunctionRes TranslateTypes))
processFunctionRes res = do
    eAnn <- classified FunPositionRes $ classifyResPassingMethod typ'.c typ'.hs
    pure $ eAnn <&> \ann -> C.FunctionRes{
        typ = typ'
      , ann = ann
      }
  where
    typ' = processType Translation.FunRes res.typ

-- | Report the external bindings without FFI type
classified ::
     FunPosition
  -> Classified a
  -> DM l (Either FunPositionError a)
classified pos x = do
    sugars <- gets (.sugars)
    let (r, fallbacks) = runWriter (runReaderT x sugars)
    forM_ fallbacks $ addMessage . TranslateTypesFFITypes pos
    pure r

processGlobal :: C.Global MangleNames -> DM l (C.Global TranslateTypes)
processGlobal global = pure C.Global{
      typ = processType Translation.Top global.typ
    , ann = global.ann
    }

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

processType ::
     ( CoercePass f MangleNames TranslateTypes
     , Translation.InContext (f MangleNames)
     )
  => Translation.TypeContext
  -> f MangleNames
  -> Translated f TranslateTypes
processType ctx typ = Translated{
      c  = coercePass typ
    , hs = Translation.inContext ctx typ
    }

{-------------------------------------------------------------------------------
  Monad
-------------------------------------------------------------------------------}

-- | Run the monad
--
-- Delayed trace messages are returned in the order they were recorded
runM :: env -> M env a -> (a, [(C.DeclId, DelayedTranslateTypesMsg)])
runM m (M k) = fmap (reverse . (.messages)) $ runState (runReaderT k m) initSt

newtype M env a = M (ReaderT env (State St) a)
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance MonadReader env (M env)
deriving newtype instance MonadState St (M env)

-- | Monad 'M' with a global environment
type GM l = M (GlobalEnv l)
-- | Monad 'M' with a decl-local environment
type DM l = M (DeclEnv   l)

{-------------------------------------------------------------------------------
  Reader
-------------------------------------------------------------------------------}

withEnv :: (env1 -> env2) -> M env2 a -> M env1 a
withEnv f (M k) = M (withReaderT f k)

withDeclEnv :: DeclIdPair -> DM l a -> GM l a
withDeclEnv declIdPair = withEnv $ \_ -> DeclEnv {
      declIdPair = declIdPair
    }

data GlobalEnv l = GlobalEnv

data DeclEnv l = DeclEnv {
    declIdPair :: DeclIdPair
  }

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

data St = St {
    -- | Delayed trace messages in reverse order that they were recorded
    messages :: [(C.DeclId, DelayedTranslateTypesMsg)]
    -- | Sugar info
  , sugars   :: SugarInfos
  }

initSt :: St
initSt = St { messages = [], sugars = emptySugarInfos }

addMessage ::
     (MonadReader (DeclEnv l) m, MonadState St m)
  => DelayedTranslateTypesMsg -> m ()
addMessage msg = do
    declIdPair <- asks (.declIdPair)
    modify $ \st -> st {
        messages = (declIdPair.cName, msg) : st.messages
      }

registerSugarInfoM ::
     (MonadReader (DeclEnv l) m, MonadState St m)
  => RegSugarInfo -> m ()
registerSugarInfoM ty = do
    declIdPair <- asks (.declIdPair)
    sugars <- gets (.sugars)
    case registerSugarInfo declIdPair ty sugars of
      Left err               -> do
        addMessage $ TranslateTypesRegisterSugarInfoDuplicate err
      Right sugars' -> do
        modify $ \st -> st { sugars = sugars' }
        addMessage $ TranslateTypesRegisterSugarInfo ty
