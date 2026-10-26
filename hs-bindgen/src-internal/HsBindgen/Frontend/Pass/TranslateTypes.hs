module HsBindgen.Frontend.Pass.TranslateTypes (
    translateTypes
  ) where

import Control.Monad.Reader (MonadReader, ReaderT (..), withReaderT)
import Control.Monad.State (MonadState, State, modify, runState)

import HsBindgen.Frontend.DeclDB (DeclDB)
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.DelayedMsg
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.Msg
import HsBindgen.Frontend.Pass.TranslateTypes.PassBy (classifyArgPassingMethod,
                                                      classifyResPassingMethod)
import HsBindgen.Frontend.Pass.TranslateTypes.Translation qualified as Translation
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.IR.Translation

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

translateTypes ::
     forall l.
     C.TranslationUnit l AdjustTypes
  -> ( C.TranslationUnit l TranslateTypes
     , [AnnMsg TranslateTypes]
     )
translateTypes unit = (C.fromDeclDB unit.includeGraph db', msgs)
  where
    msgs = []

    db' :: DeclDB l TranslateTypes
    db' =
        -- Collected in reverse order
        DeclDB.addDelayedMsgs
          (reverse $ map (fmap DelayedTranslateTypes) delayedMsgs)
          db

    (db, delayedMsgs) =
      runM NoEnv $
        DeclDB.traverseActive DeclDB.AnyOrder
          (fmap DeclDB.Advanced . processDecl)
          unit.db

{-------------------------------------------------------------------------------
  Decls
-------------------------------------------------------------------------------}

processDecl :: C.Decl l AdjustTypes -> M NoEnv (C.Decl l TranslateTypes)
processDecl decl = do
    kind' <- withEnv (const (DeclEnv decl.info)) $ processDeclKind decl.kind
    pure $ C.coercePassDeclWith kind' decl.ann decl

processDeclKind :: C.DeclKind l AdjustTypes -> M DeclEnv (C.DeclKind l TranslateTypes)
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

processStruct :: C.Struct AdjustTypes -> M DeclEnv (C.Struct TranslateTypes)
processStruct struct = do
    fields' <- mapM processField struct.fields
    flam' <- C.traverseFlamField processRegularField struct.flam
    pure C.Struct{
        sizeof    = struct.sizeof
      , alignment = struct.alignment
      , fields    = fields'
      , flam      = flam'
      , ann       = struct.ann
      }

processUnion :: C.Union AdjustTypes -> M DeclEnv (C.Union TranslateTypes)
processUnion union = do
    fields' <- mapM processField union.fields
    pure C.Union{
        sizeof    = union.sizeof
      , alignment = union.alignment
      , fields    = fields'
      , ann       = union.ann
      }

processField :: C.Field AdjustTypes -> M DeclEnv (C.Field TranslateTypes)
processField = \case
    C.FieldRegular  field -> C.FieldRegular  <$> processRegularField field
    C.FieldImplicit field -> C.FieldImplicit <$> processImplicitField field

processRegularField :: C.RegularField AdjustTypes -> M DeclEnv (C.RegularField TranslateTypes)
processRegularField field = pure C.RegularField{
      info   = coercePass field.info
    , typ    = processType Translation.Top field.typ
    , offset = field.offset
    , width  = field.width
    , ann    = field.ann
    }

processImplicitField :: C.ImplicitField AdjustTypes -> M DeclEnv (C.ImplicitField TranslateTypes)
processImplicitField field = do
    indirect' <- mapM processIndirectField field.indirect
    pure C.ImplicitField{
        info     = coercePass field.info
      , typRef   = processAnonRef Translation.Top field.typRef
      , offset   = field.offset
      , indirect = indirect'
      , ann      = field.ann
      }

processIndirectField :: C.IndirectField AdjustTypes -> M DeclEnv (C.IndirectField TranslateTypes)
processIndirectField field = pure C.IndirectField{
      info   = coercePass field.info
    , typ    = processType Translation.Top field.typ
    , offset = field.offset
    , width  = field.width
    , path   = map (processAnonRef Translation.Top) field.path
    , ann    = coercePassAnn (Proxy @'("IndirectField", AdjustTypes, TranslateTypes)) field.ann
    }

processTypedef :: C.Typedef AdjustTypes -> M DeclEnv (C.Typedef TranslateTypes)
processTypedef typedef = pure C.Typedef{
      typ = processType Translation.Top typedef.typ
    , ann = typedef.ann
    }

processEnum :: C.Enum AdjustTypes -> M DeclEnv (C.Enum TranslateTypes)
processEnum enum = pure C.Enum{
      typ       = processType Translation.Top enum.typ
    , sizeof    = enum.sizeof
    , alignment = enum.alignment
    , constants = map coercePass enum.constants
    , ann       = enum.ann
    }

processUntaggedEnumConstant ::
     C.UntaggedEnumConstant AdjustTypes
  -> M DeclEnv (C.UntaggedEnumConstant TranslateTypes)
processUntaggedEnumConstant = pure . coercePass

processMacro :: MacroBody AdjustTypes l -> M DeclEnv (MacroBody TranslateTypes l)
processMacro mac = pure $ case mac of
    MacroType  typ -> MacroType  $ coercePass typ
    MacroValue val -> MacroValue $ coercePass val
    MacroRaw   raw -> MacroRaw raw

processFunction :: C.Function AdjustTypes -> M DeclEnv (C.Function TranslateTypes)
processFunction fun = do
    args' <- mapM processFunctionArg fun.args
    res' <- processFunctionRes fun.res
    pure C.Function{
        args  = args'
      , res   = res'
      , attrs = fun.attrs
      , ann   = fun.ann
      }

processFunctionArg ::
     C.FunctionArg AdjustTypes
  -> M DeclEnv (C.FunctionArg TranslateTypes)
processFunctionArg arg = do
    pure C.FunctionArg{
        name = arg.name
      , typ  = processTypeFunArg Translation.FunArg arg.typ
      , ann  = classifyArgPassingMethod arg
      }

processFunctionRes :: C.FunctionRes AdjustTypes -> M DeclEnv (C.FunctionRes TranslateTypes)
processFunctionRes res = do
    pure C.FunctionRes{
        typ = TranslatedTypes{
            c  = coercePass res.typ
          , hs = Translation.inContext Translation.FunRes res
          }
      , ann = classifyResPassingMethod res
      }

processGlobal :: C.Global AdjustTypes -> M DeclEnv (C.Global TranslateTypes)
processGlobal global = pure C.Global{
      typ = processType Translation.Top global.typ
    , ann = global.ann
    }

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

processType ::
     Translation.TypeContext
  -> C.Type AdjustTypes
  -> TranslatedTypes TranslateTypes
processType ctx typ = TranslatedTypes{
      c  = coercePass typ
    , hs = Translation.inContext ctx typ
    }

processTypeFunArg ::
     Translation.TypeContext
  -> C.TypeFunArg AdjustTypes
  -> TranslatedTypeFunArg TranslateTypes
processTypeFunArg ctx typ = TranslatedTypeFunArg{
      c  = coercePass typ
    , hs = Translation.inContext ctx typ
    }

processAnonRef ::
     Translation.TypeContext
  -> C.AnonRef AdjustTypes
  -> TranslatedAnonRef TranslateTypes
processAnonRef ctx typ = TranslatedAnonRef{
      c = coercePass typ
    , hs = Translation.inContext ctx (C.anonRefType typ)
    }

{-------------------------------------------------------------------------------
  Monad
-------------------------------------------------------------------------------}

-- | Run the monad
--
-- Delayed trace messages are returned in reverse order that they were recorded
runM :: env -> M env a -> (a, [(C.DeclId, DelayedTranslateTypesMsg)])
runM m (M k) = fmap (.messages) $ runState (runReaderT k m) (St [])

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
    messages :: [(C.DeclId, DelayedTranslateTypesMsg)]
  }

_addMessage :: C.DeclId -> DelayedTranslateTypesMsg -> M env ()
_addMessage did msg = modify $ \st -> st {
      messages = (did, msg) : st.messages
    }
