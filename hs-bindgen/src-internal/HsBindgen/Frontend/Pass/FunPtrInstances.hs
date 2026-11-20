-- | Decide which function types get @ToFunPtr@ and @FromFunPtr@ instances
--
-- A function type gets the instances iff it passes every argument and its
-- result directly, and
--
-- * it is the function type of a typedef (@typedef void F(T)@, or the @_Aux@
--   newtype of @typedef void (*F)(T)@): the instances are for the newtype; or
-- * it is nested in a use-site type, and one of its types is defined in this
--   module, which avoids orphan instances: the instances are for the
--   function type itself.
--
-- The signature of a function declaration is not a nested function type; the
-- function types in its arguments and result are.
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Pass.FunPtrInstances qualified as FunPtrInstances
module HsBindgen.Frontend.Pass.FunPtrInstances (
    Result(..)
  , funPtrInstances
  ) where

import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.State (State, gets, modify, runState)
import Control.Monad.Writer.Strict (runWriter)
import Data.Either (partitionEithers)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.DeclDB (DeclDB)
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.Pass.FunPtrInstances.IsPass
import HsBindgen.Frontend.Pass.FunPtrInstances.IsPass.Msg
import HsBindgen.Frontend.Pass.MangleNames.IsPass (TypedefNames (..))
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.Error (FunPositionError)
import HsBindgen.Frontend.Pass.TranslateTypes.FFIType qualified as FFIType
import HsBindgen.Frontend.Pass.TranslateTypes.PassingMethod (Classified,
                                                             classifyArgPassingMethod,
                                                             classifyResPassingMethod)
import HsBindgen.Frontend.Pass.TranslateTypes.Position
import HsBindgen.Frontend.Pass.TranslateTypes.Translation qualified as Translation
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.FunPtrInstance
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Pass
import HsBindgen.IR.PassingMethod
import HsBindgen.IR.Translation
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer (withCallStack)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

data Result l = Result {
      -- | The input, with its declarations unchanged
      unit      :: C.TranslationUnit l FunPtrInstances
      -- | The instances to generate, in output order
    , instances :: [FunPtrInstance]
    }

funPtrInstances ::
     C.TranslationUnit l Select
  -> (Result l, [AnnMsg FunPtrInstances])
funPtrInstances unit = (
      Result{
          unit      = unit{C.db = db'}
        , instances = reverse st.instances
        }
    , reverse st.messages
    )
  where
    -- The order of the generated bindings: the instances are in output order,
    -- and each function type is reported at its first occurrence in it
    (db', st) =
      flip runState initSt $
        DeclDB.traverseActive
          (DeclUseGraph.outputOrder unit.includeGraph)
          (processDecl unit.db)
          unit.db

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

processDecl ::
     forall l.
     DeclDB l Select
  -> C.Decl l Select
  -> State St (DeclDB.PassResult l FunPtrInstances)
processDecl db decl = do
    runReaderT (processDeclKind decl) env
    pure $ DeclDB.Advanced $ coercePass decl
  where
    env :: Env l
    env = Env{
        db  = db
      , loc = C.declIdLocationInfo decl.id.cName [decl.info.loc]
      }

processDeclKind :: C.Decl l Select -> M l ()
processDeclKind decl = case decl.kind of
    C.DeclStruct struct -> do
      mapM_ processField struct.fields
      mapM_ processRegularField (C.flamStructField struct.flam)
    C.DeclUnion union ->
      mapM_ processField union.fields
    C.DeclTypedef typedef ->
      processType (TypedefOwn (typedefHead decl typedef))
        (atUseSite UseSiteTypedef) typedef.typ.c
    C.DeclFunction fun -> do
      forM_ (zip [1..] fun.args) $ \(i, arg) ->
        processType Nested
          (enterPosition declarationSignature $ FunPositionArg FunArgPosition{
              index = i
            , name  = (.cName) <$> arg.name
            })
          arg.typ.c.typ
      processType Nested
        (enterPosition declarationSignature FunPositionRes)
        fun.res.typ.c
    C.DeclGlobal global ->
      processType Nested (atUseSite UseSiteGlobal) global.typ.c
    -- Integral
    C.DeclEnum{}                 -> pure ()
    C.DeclUntaggedEnumConstant{} -> pure ()
    C.DeclOpaque{}               -> pure ()
    C.DeclMacro{}                -> pure ()

processField :: C.Field Select -> M l ()
processField = C.elimField processRegularField processImplicitField

processRegularField :: C.RegularField Select -> M l ()
processRegularField field =
    processType Nested (atUseSite (UseSiteField field.info.name.cName)) field.typ.c

processImplicitField :: C.ImplicitField Select -> M l ()
processImplicitField field =
    forM_ field.indirect $ \indirect ->
      processType Nested
        (atUseSite (UseSiteField indirect.info.name.cName))
        indirect.typ.c

-- | The head of the instances for the function type of a typedef
--
-- Evaluated only if the type of the typedef has a function type.
typedefHead :: C.Decl l Select -> C.Typedef Select -> FunPtrHead
typedefHead decl typedef = case typedef.typ.c of
    C.TypeFun{} ->
      HeadNewtype
        (Hs.assertNs (Proxy @Hs.NsTypeConstr) decl.id.hsName)
        typedef.ann.orig
    _otherwise -> case typedef.ann.aux of
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/2369>
      Nothing          -> panicPure "name of auxiliary declaration unavailable"
      Just (name, nms) -> HeadNewtype name nms

{-------------------------------------------------------------------------------
  Function types
-------------------------------------------------------------------------------}

-- | Where a function type occurs
data Occurrence =
    -- | The function type of a typedef: it gets instances for the newtype
    TypedefOwn FunPtrHead
    -- | Any other function type: it gets instances for itself, if one of its
    -- types is defined in this module
  | Nested

-- | Process the outermost function type in a type, and the function types
-- nested in its arguments and result
processType :: Occurrence -> FunTypeLocation -> C.Type Select -> M l ()
processType occurrence loc ty =
    forM_ (C.getOutermostFunType ty) $ \(args, res) -> do
      processFunType occurrence loc args res
      forM_ (zip [1..] args) $ \(i, arg) ->
        processType Nested (enterPosition loc (argPosition i)) arg.typ
      processType Nested (enterPosition loc FunPositionRes) res

processFunType ::
     Occurrence
  -> FunTypeLocation
  -> [C.TypeFunArg Select]
  -> C.Type Select
  -> M l ()
processFunType occurrence loc args res = do
    mHead <- case occurrence of
      TypedefOwn hd -> pure $ Just hd
      Nested        -> do
        db <- asks (.db)
        pure $ HeadFunType <$ guard (isDefinedInCurrentModule db funC)
    forM_ mHead $ \hd -> do
      mSig <- classifyCached loc args res
      forM_ mSig $ \(args', res') ->
        modify $ \st ->
          let key = (hd, eraseUnderlying funHs)
          in  if key `Set.member` st.seen then st else st{
                  instances = FunPtrInstance hd args' res' : st.instances
                , seen      = Set.insert key st.seen
                }
  where
    funHs :: Hs.Type
    funHs = Translation.topLevel funC

    funC :: C.Type Select
    funC = C.TypeFun args res

-- | Is one of the types defined in the current module?
--
-- After @Select@, the active declarations are the selected ones.
isDefinedInCurrentModule :: DeclDB l Select -> C.Type Select -> Bool
isDefinedInCurrentModule db =
    any (isDefined . (.cName) . fst) . C.depsOfType
  where
    isDefined :: C.DeclId -> Bool
    isDefined declId = case DeclDB.lookupEntry declId db of
      Just DeclDB.Active{} -> True
      -- Uses of a squashed typedef refer to the declaration of its target
      Just (DeclDB.Squashed _ target) -> isJust $ DeclDB.lookup target.c db
      _otherwise -> False

-- | Classify a function type, reporting why it gets no instances
--
-- Classifies and reports each distinct Haskell function type once.
classifyCached ::
     FunTypeLocation
  -> [C.TypeFunArg Select]
  -> C.Type Select
  -> M l (Maybe ([FunPtrArg], FunPtrRes))
classifyCached loc args res = do
    cached <- gets (Map.lookup key . (.cache))
    case cached of
      Just mSig -> pure mSig
      Nothing   -> do
        mSig <- classify loc args res
        modify $ \st -> st{cache = Map.insert key mSig st.cache}
        pure mSig
  where
    key :: Hs.Type
    key = eraseUnderlying $ Translation.topLevel (C.TypeFun args res)

classify ::
     FunTypeLocation
  -> [C.TypeFunArg Select]
  -> C.Type Select
  -> M l (Maybe ([FunPtrArg], FunPtrRes))
classify loc args res =
    case (partitionEithers (map fst args'), fst res') of
      (([], argsOk), Right resOk) ->
        case (traverse direct argsOk, directRes resOk) of
          (Just argsDirect, Just resDirect) -> do
            forM_ fallbacks $ \(pos, fallback) ->
              addMessage $ FunPtrInstancesExtBindingFallback loc pos fallback
            pure $ Just (argsDirect, resDirect)
          _otherwise -> do
            forM_ (NonEmpty.nonEmpty serialized) $
              addMessage . FunPtrInstancesSerialized loc
            pure Nothing
        where
          serialized :: [FunPosition]
          serialized = concat [
                [ argPosition i
                | (i, arg) <- zip [1..] argsOk
                , isSerialized arg.passing
                ]
              , [ FunPositionRes | isResSerialized resOk.passing ]
              ]
      ((errs, _), eRes) -> do
        let allErrs = errs ++ either pure (const []) eRes
        forM_ (NonEmpty.nonEmpty allErrs) $
          addMessage . FunPtrInstancesPositionErrors loc
        pure Nothing
  where
    args' :: [(Either FunPositionError (ClassifiedPosition (PassingMethod Select)), [Fallback])]
    args' = [
        withFallbacks pos $
          fmap (ClassifiedPosition hs) <$> classifyArgPassingMethod pos arg.typ hs
      | (i, arg) <- zip [1..] args
      , let pos = argPosition i
            hs  = Translation.inContext Translation.FunArg arg
      ]

    res' :: (Either FunPositionError (ClassifiedPosition (ResPassingMethod Select)), [Fallback])
    res' =
        withFallbacks FunPositionRes $
          fmap (ClassifiedPosition hs) <$> classifyResPassingMethod res hs
      where
        hs = Translation.inContext Translation.FunRes res

    fallbacks :: [Fallback]
    fallbacks = concatMap snd args' ++ snd res'

    direct :: ClassifiedPosition (PassingMethod Select) -> Maybe FunPtrArg
    direct arg = case arg.passing of
      PassDirect ffi   -> Just FunPtrArg{hs = arg.hs, ffi = ffi}
      PassSerialized _ -> Nothing

    directRes :: ClassifiedPosition (ResPassingMethod Select) -> Maybe FunPtrRes
    directRes r = case r.passing of
      PassResVoid                   -> Just FunPtrRes{hs = r.hs, ffi = Hs.FFIResIOUnit}
      PassResNonVoid (PassDirect t) -> Just FunPtrRes{hs = r.hs, ffi = Hs.FFIResIO t}
      PassResNonVoid PassSerialized{} -> Nothing

-- | A classified argument or result, with its Haskell type
data ClassifiedPosition a = ClassifiedPosition {
      hs      :: Hs.Type
    , passing :: a
    }

type Fallback = (FunPosition, FFIType.ExtBindingFallback)

withFallbacks ::
     FunPosition
  -> Classified a
  -> (Either FunPositionError a, [Fallback])
withFallbacks pos x = second (map (pos,)) $ runWriter x

-- | Erase the underlying types of references
--
-- The generated code renders a reference by its name only, so two types that
-- differ only in the underlying types of their references give the same
-- instance. For example, with @typedef struct S S;@ squashed, a use of the
-- typedef translates to @TypRef "S" (Just (TypRef "S" Nothing))@, and a use
-- of @struct S@ to @TypRef "S" Nothing@.
eraseUnderlying :: Hs.Type -> Hs.Type
eraseUnderlying = \case
    Hs.PrimType t             -> Hs.PrimType t
    Hs.TypRef name _          -> Hs.TypRef name Nothing
    Hs.ConstArray n t         -> Hs.ConstArray n (eraseUnderlying t)
    Hs.IncompleteArray t      -> Hs.IncompleteArray (eraseUnderlying t)
    Hs.PtrArrayElem t         -> Hs.PtrArrayElem (eraseUnderlying t)
    Hs.PtrConstArrayElem t    -> Hs.PtrConstArrayElem (eraseUnderlying t)
    Hs.Ptr t                  -> Hs.Ptr (eraseUnderlying t)
    Hs.FunPtr t               -> Hs.FunPtr (eraseUnderlying t)
    Hs.PtrConst t             -> Hs.PtrConst (eraseUnderlying t)
    Hs.IO t                   -> Hs.IO (eraseUnderlying t)
    Hs.Fun a b                -> Hs.Fun (eraseUnderlying a) (eraseUnderlying b)
    -- Not optional: erase the references in it
    Hs.ExtBinding ref c hs t  -> Hs.ExtBinding ref c hs (eraseUnderlying t)
    Hs.ByteArray              -> Hs.ByteArray
    Hs.SizedByteArray n m     -> Hs.SizedByteArray n m
    Hs.Block t                -> Hs.Block (eraseUnderlying t)
    Hs.ComplexType t          -> Hs.ComplexType t
    Hs.StrLit str             -> Hs.StrLit str
    Hs.WithFlam a b           -> Hs.WithFlam (eraseUnderlying a) (eraseUnderlying b)
    Hs.EquivStorable t        -> Hs.EquivStorable (eraseUnderlying t)
    Hs.IsStructViaReadRaw t   -> Hs.IsStructViaReadRaw (eraseUnderlying t)

-- | An argument of a function type, which has no name
argPosition :: Int -> FunPosition
argPosition i = FunPositionArg FunArgPosition{index = i, name = Nothing}

{-------------------------------------------------------------------------------
  Monad
-------------------------------------------------------------------------------}

type M l = ReaderT (Env l) (State St)

data Env l = Env {
      -- | The declarations after @Select@
      db  :: DeclDB l Select
      -- | Location of the current declaration
    , loc :: C.LocationInfo
    }

data St = St {
      -- | In reverse order
      instances :: [FunPtrInstance]
      -- | The instances so far, by head and Haskell function type (with
      -- 'eraseUnderlying')
    , seen      :: Set (FunPtrHead, Hs.Type)
      -- | Signatures of the Haskell function types classified so far (with
      -- 'eraseUnderlying'), or 'Nothing' if they get no instances
    , cache     :: Map Hs.Type (Maybe ([FunPtrArg], FunPtrRes))
      -- | In reverse order
    , messages  :: [AnnMsg FunPtrInstances]
    }

initSt :: St
initSt = St{
      instances = []
    , seen      = Set.empty
    , cache     = Map.empty
    , messages  = []
    }

addMessage :: FunPtrInstancesMsg -> M l ()
addMessage msg = do
    loc <- asks (.loc)
    modify $ \st -> st{
        messages = withCallStack C.WithLocationInfo{loc = loc, msg = msg}
                 : st.messages
      }
