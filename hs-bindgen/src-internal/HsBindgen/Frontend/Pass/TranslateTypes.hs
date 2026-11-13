module HsBindgen.Frontend.Pass.TranslateTypes (
    translateTypes
  ) where

import Control.Monad (zipWithM)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks, withReaderT)
import Control.Monad.State (MonadState, State, modify, runState)
import Control.Monad.Writer.Strict (runWriter)
import Data.Either (partitionEithers)
import Data.List.NonEmpty qualified as NonEmpty

import HsBindgen.Frontend.DeclDB (DeclDB)
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.DelayedMsg
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.Error
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.Msg
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.PassingMethod (PassingMethod,
                                                                    ResPassingMethod,
                                                                    isResSerialized,
                                                                    isSerialized)
import HsBindgen.Frontend.Pass.TranslateTypes.PassingMethod (Classified,
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
processRegularField field = do
    typ' <- processType (useSiteField field.info) Translation.Top field.typ
    pure C.RegularField{
        info   = coercePass field.info
      , typ    = typ'
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
processIndirectField field = do
    typ' <- processType (useSiteField field.info) Translation.Top field.typ
    pure C.IndirectField{
        info   = coercePass field.info
      , typ    = typ'
      , offset = field.offset
      , width  = field.width
      , path   = map (processAnonRef Translation.Top) field.path
      , ann    = coercePassAnn (Proxy @'("IndirectField", AdjustTypes, TranslateTypes)) field.ann
      }

useSiteField :: C.FieldInfo AdjustTypes -> FunTypeLocation
useSiteField info = FunTypeLocation{
      site = UseSiteField info.name.cName
    , path = []
    }

processTypedef :: C.Typedef AdjustTypes -> M DeclEnv (C.Typedef TranslateTypes)
processTypedef typedef = do
    typ' <- processType loc Translation.Top typedef.typ
    pure C.Typedef{
        typ = typ'
      , ann = typedef.ann
      }
  where
    loc = FunTypeLocation{site = UseSiteTypedef, path = []}

processEnum :: C.Enum AdjustTypes -> M DeclEnv (C.Enum TranslateTypes)
processEnum enum = pure C.Enum{
      typ       = TranslatedTypes{
          c       = coercePass enum.typ
        , hs      = Translation.inContext Translation.Top enum.typ
          -- The type of an enum is integral
        , funType = Nothing
        }
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
    eSig <- processSignature declarationSignature fun.args fun.res
    pure $ case eSig of
      Left (errs, _) -> Left $ TranslateTypesFunctionError errs
      Right sig      -> Right C.Function{
          args  = sig.args
        , res   = sig.res
        , attrs = fun.attrs
        , ann   = fun.ann
        }

-- | Translate and classify the arguments and the result of a function type
--
-- The position errors are in signature order.
processSignature ::
     FunTypeLocation
  -> [C.FunctionArg AdjustTypes]
  -> C.FunctionRes AdjustTypes
  -> M DeclEnv (
       Either
         (NonEmpty FunPositionError, UnclassifiedFunType TranslateTypes)
         (ClassifiedFunType TranslateTypes)
     )
processSignature loc args res = do
    args' <- zipWithM (processFunctionArg loc) [1..] args
    res'  <- processFunctionRes loc res
    let unclassified = UnclassifiedFunType{
            args = map fst args'
          , res  = fst res'
          }
    pure $ case (partitionEithers (map snd args'), snd res') of
      (([], argsOk), Right resOk) -> Right ClassifiedFunType{
          args = argsOk
        , res  = resOk
        }
      ((err : errs, _), Right _) ->
        Left (err :| errs, unclassified)
      ((errs, _), Left resErr) ->
        Left (NonEmpty.prependList errs (resErr :| []), unclassified)

-- | Translate and classify a function argument
--
-- Returns the translated type also on a position error.
processFunctionArg ::
     FunTypeLocation
  -> Int
  -> C.FunctionArg AdjustTypes
  -> M DeclEnv (
       TranslatedTypeFunArg TranslateTypes
     , Either FunPositionError (C.FunctionArg TranslateTypes)
     )
processFunctionArg loc i arg = do
    typ' <- processTypeFunArg (enterPosition loc pos) Translation.FunArg arg.typ
    eAnn <- classified loc pos $ classifyArgPassingMethod pos arg typ'.hs
    pure (typ', mk typ' <$> eAnn)
  where
    pos  = FunPositionArg FunArgPosition{
        index = i
      , name  = (.cName) <$> arg.name
      }

    mk ::
         TranslatedTypeFunArg TranslateTypes
      -> PassingMethod TranslateTypes
      -> C.FunctionArg TranslateTypes
    mk typ' ann = C.FunctionArg{
        name = arg.name
      , typ  = typ'
      , ann  = ann
      }

-- | Translate and classify a function result
--
-- Returns the translated type also on a position error.
processFunctionRes ::
     FunTypeLocation
  -> C.FunctionRes AdjustTypes
  -> M DeclEnv (
       TranslatedTypes TranslateTypes
     , Either FunPositionError (C.FunctionRes TranslateTypes)
     )
processFunctionRes loc res = do
    typ' <- processType (enterPosition loc pos) Translation.FunRes res.typ
    eAnn <- classified loc pos $ classifyResPassingMethod res typ'.hs
    pure (typ', mk typ' <$> eAnn)
  where
    pos = FunPositionRes

    mk ::
         TranslatedTypes TranslateTypes
      -> ResPassingMethod TranslateTypes
      -> C.FunctionRes TranslateTypes
    mk typ' ann = C.FunctionRes{
        typ = typ'
      , ann = ann
      }

-- | Report the external bindings without FFI type
classified ::
     FunTypeLocation
  -> FunPosition
  -> Classified a
  -> M DeclEnv (Either FunPositionError a)
classified loc pos x = do
    let (r, fallbacks) = runWriter x
    forM_ fallbacks $ addMessage . TranslateTypesExtBindingFallback loc pos
    pure r

processGlobal :: C.Global AdjustTypes -> M DeclEnv (C.Global TranslateTypes)
processGlobal global = do
    typ' <- processType loc Translation.Top global.typ
    pure C.Global{
        typ = typ'
      , ann = global.ann
      }
  where
    loc = FunTypeLocation{site = UseSiteGlobal, path = []}

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Translate a use-site type
processType ::
     FunTypeLocation -- ^ Location of the outermost function type in the type
  -> Translation.TypeContext
  -> C.Type AdjustTypes
  -> M DeclEnv (TranslatedTypes TranslateTypes)
processType loc ctx typ = do
    funType <- processOutermostFunType loc typ
    pure TranslatedTypes{
        c       = coercePass typ
      , hs      = Translation.inContext ctx typ
      , funType = funType
      }

-- | Translate a use-site function argument type
processTypeFunArg ::
     FunTypeLocation -- ^ Location of the outermost function type in the type
  -> Translation.TypeContext
  -> C.TypeFunArg AdjustTypes
  -> M DeclEnv (TranslatedTypeFunArg TranslateTypes)
processTypeFunArg loc ctx typ = do
    funType <- processOutermostFunType loc typ.typ
    pure TranslatedTypeFunArg{
        c       = coercePass typ
      , hs      = Translation.inContext ctx typ
      , funType = funType
      }

-- | Classify the outermost function type in a type
--
-- A function type that gets no @ToFunPtr@ and @FromFunPtr@ instances gets a
-- message: one with a position we cannot pass, or with a serialised position.
processOutermostFunType ::
     FunTypeLocation
  -> C.Type AdjustTypes
  -> M DeclEnv (Maybe (FunType TranslateTypes))
processOutermostFunType loc typ =
    forM (C.getOutermostFunType typ) $ \(args, res) -> do
      eSig <- processSignature loc (map mkArg args) (mkRes res)
      case eSig of
        Left (errs, sig) -> do
          addMessage $ TranslateTypesFunTypeUnclassified loc errs
          pure $ FunTypeUnclassified sig
        Right sig -> do
          forM_ (NonEmpty.nonEmpty $ serializedPositions sig) $
            addMessage . TranslateTypesFunTypeSerialized loc
          pure $ FunTypeClassified sig
  where
    mkArg :: C.TypeFunArg AdjustTypes -> C.FunctionArg AdjustTypes
    mkArg typ' = C.FunctionArg{
        name = Nothing
      , typ  = typ'
      , ann  = NoAnn
      }

    mkRes :: C.Type AdjustTypes -> C.FunctionRes AdjustTypes
    mkRes typ' = C.FunctionRes{
        typ = typ'
      , ann = NoAnn
      }

    serializedPositions :: ClassifiedFunType TranslateTypes -> [FunPosition]
    serializedPositions sig = concat [
          [ FunPositionArg FunArgPosition{index = i, name = Nothing}
          | (i, arg) <- zip [1..] sig.args
          , isSerialized arg.ann
          ]
        , [ FunPositionRes
          | isResSerialized sig.res.ann
          ]
        ]

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
