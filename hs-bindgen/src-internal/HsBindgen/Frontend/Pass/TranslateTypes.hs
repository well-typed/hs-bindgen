module HsBindgen.Frontend.Pass.TranslateTypes (
    translateTypes
  ) where

import Control.Monad (zipWithM)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks, withReaderT)
import Control.Monad.State (MonadState, State, modify, runState)
import Control.Monad.Writer.Strict (runWriter)
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NonEmpty

import HsBindgen.Frontend.DeclDB (DeclDB)
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.DelayedMsg
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.Error
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.Msg
import HsBindgen.Frontend.Pass.TranslateTypes.PassBy (Classified,
                                                      classifyArgPassingMethod,
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
  -> C.TranslationUnit l TranslateTypes
translateTypes unit = unit{C.db = db'}
  where
    db' :: DeclDB l TranslateTypes
    db' =
        DeclDB.addDelayedMsgs
          (map (second DelayedTranslateTypes) delayedMsgs)
          db

    (db, delayedMsgs) =
      runM NoEnv $
        DeclDB.traverseActive DeclDB.AnyOrder processDecl unit.db

{-------------------------------------------------------------------------------
  Decls
-------------------------------------------------------------------------------}

processDecl ::
     C.Decl l AdjustTypes
  -> M NoEnv (DeclDB.PassResult l TranslateTypes)
processDecl decl = do
    eKind' <- withEnv (const (DeclEnv decl.id.cName)) $ processDeclKind decl.kind
    pure $ case eKind' of
      Left  err   -> DeclDB.Fail $ DeclDB.TranslateTypesFailed err
      Right kind' -> DeclDB.Advanced $ C.coercePassDeclWith kind' decl.ann decl

processDeclKind ::
     C.DeclKind l AdjustTypes
  -> M DeclEnv (Either TranslateTypesError (C.DeclKind l TranslateTypes))
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
    ok :: M DeclEnv a -> M DeclEnv (Either TranslateTypesError a)
    ok = fmap Right

processStruct :: C.Struct AdjustTypes -> M DeclEnv (C.Struct TranslateTypes)
processStruct struct = do
    fields' <- mapM processField struct.fields
    flam'   <- C.traverseFlamField processRegularField struct.flam
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

processFunction ::
     C.Function AdjustTypes
  -> M DeclEnv (Either TranslateTypesError (C.Function TranslateTypes))
processFunction fun = do
    (argErrs, args') <- partitionEithers <$>
      zipWithM processFunctionArg [1..] fun.args
    eRes' <- processFunctionRes fun.res
    pure $ first TranslateTypesNoFFIType $
      case (NonEmpty.nonEmpty argErrs, eRes') of
        (Nothing  , Right res') -> Right C.Function{
            args  = args'
          , res   = res'
          , attrs = fun.attrs
          , ann   = fun.ann
          }
        (Nothing  , Left resErr) -> Left $ resErr :| []
        (Just errs, Right _    ) -> Left errs
        (Just errs, Left resErr) -> Left $ errs <> (resErr :| [])

processFunctionArg ::
     Int
  -> C.FunctionArg AdjustTypes
  -> M DeclEnv (Either NoFFIType (C.FunctionArg TranslateTypes))
processFunctionArg i arg = do
    eAnn <- classified pos $ classifyArgPassingMethod pos arg typ'.hs
    pure $ eAnn <&> \ann -> C.FunctionArg{
        name = arg.name
      , typ  = typ'
      , ann  = ann
      }
  where
    pos  = FunPositionArg i ((.cName) <$> arg.name)
    typ' = processTypeFunArg Translation.FunArg arg.typ

processFunctionRes ::
     C.FunctionRes AdjustTypes
  -> M DeclEnv (Either NoFFIType (C.FunctionRes TranslateTypes))
processFunctionRes res = do
    eAnn <- classified FunPositionRes $ classifyResPassingMethod res typ'.hs
    pure $ eAnn <&> \ann -> C.FunctionRes{
        typ = typ'
      , ann = ann
      }
  where
    typ' = processType Translation.FunRes res.typ

-- | Report the external bindings without FFI type
classified :: FunPosition -> Classified a -> M DeclEnv (Either NoFFIType a)
classified pos x = do
    let (r, fallbacks) = runWriter x
    forM_ fallbacks $ addMessage . TranslateTypesExtBindingFallback pos
    pure r

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
-- Delayed trace messages are returned in the order they were recorded
runM :: env -> M env a -> (a, [(C.DeclId, DelayedTranslateTypesMsg)])
runM m (M k) = second (reverse . (.messages)) $ runState (runReaderT k m) (St [])

newtype M env a = M (ReaderT env (State St) a)
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance MonadReader env (M env)
deriving newtype instance MonadState St (M env)

withEnv :: (env1 -> env2) -> M env2 a -> M env1 a
withEnv f (M k) = M (withReaderT f k)

data NoEnv = NoEnv

newtype DeclEnv = DeclEnv {
    declId :: C.DeclId
  }

newtype St = St {
    -- | Delayed trace messages in reverse order that they were recorded
    messages :: [(C.DeclId, DelayedTranslateTypesMsg)]
  }

addMessage :: DelayedTranslateTypesMsg -> M DeclEnv ()
addMessage msg = do
    did <- asks (.declId)
    modify $ \st -> st {
        messages = (did, msg) : st.messages
      }
