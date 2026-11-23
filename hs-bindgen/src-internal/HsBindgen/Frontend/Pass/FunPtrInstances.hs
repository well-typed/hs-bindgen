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

import Control.Monad (zipWithM)
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.State (State, gets, modify, runState)
import Control.Monad.Writer.Strict (runWriter)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.DeclDB (DeclDB)
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.Pass.FunPtrInstances.IsPass
import HsBindgen.Frontend.Pass.FunPtrInstances.IsPass.Msg
import HsBindgen.Frontend.Pass.MangleNames.IsPass (AuxNames (..))
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.Error (FunPositionError)
import HsBindgen.Frontend.Pass.TranslateTypes.FFIType qualified as FFIType
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.Msg (FFITypesMsg)
import HsBindgen.Frontend.Pass.TranslateTypes.PassingMethod (Classified,
                                                             classifyArgPassingMethod,
                                                             classifyResPassingMethod)
import HsBindgen.Frontend.Pass.TranslateTypes.Position
import HsBindgen.Frontend.Pass.TranslateTypes.SugarInfo (SugarInfos)
import HsBindgen.Frontend.Pass.TranslateTypes.Translation qualified as Translation
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.FunPtrInstance
import HsBindgen.IR.FunSig
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
     SugarInfos
  -> C.TranslationUnit l Select
  -> (Result l, [AnnMsg FunPtrInstances])
funPtrInstances sugars unit = (
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
          (processDecl sugars unit.db)
          unit.db

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

processDecl ::
     forall l.
     SugarInfos
  -> DeclDB l Select
  -> C.Decl l Select
  -> State St (DeclDB.PassResult l FunPtrInstances)
processDecl sugars db decl = do
    runReaderT (processDeclKind decl) env
    pure $ DeclDB.Advanced $ coercePass decl
  where
    env :: Env l
    env = Env{
        db     = db
      , loc    = C.declIdLocationInfo decl.id.cName [decl.info.loc]
      , sugars = sugars
      }

processDeclKind :: C.Decl l Select -> M l ()
processDeclKind decl = case decl.kind of
    C.DeclStruct struct -> do
      mapM_ processField struct.fields
      mapM_ processRegularField (C.flamStructField struct.flam)
    C.DeclUnion union ->
      mapM_ processField union.fields
    C.DeclTypedef typedef ->
      processTypedef decl typedef
    C.DeclFunction fun -> do
      forM_ (zip [1..] fun.sig.args) $ \(i, arg) ->
        processType HeadFunType
          (enterPosition declarationSignature $ FunPositionArg FunArgPosition{
              index = i
            , name  = (.cName) <$> arg.name
            })
          arg.typ.c.typ
      processType HeadFunType
        (enterPosition declarationSignature FunPositionRes)
        fun.sig.res.typ.c
    C.DeclGlobal global ->
      processType HeadFunType (atUseSite UseSiteGlobal) global.typ.c
    -- Integral
    C.DeclEnum{}                 -> pure ()
    C.DeclUntaggedEnumConstant{} -> pure ()
    C.DeclOpaque{}               -> pure ()
    C.DeclMacro{}                -> pure ()

processField :: C.Field Select -> M l ()
processField = C.elimField processRegularField processImplicitField

processRegularField :: C.RegularField Select -> M l ()
processRegularField field =
    processType HeadFunType (atUseSite (UseSiteField field.info.name.cName)) field.typ.c

processImplicitField :: C.ImplicitField Select -> M l ()
processImplicitField field =
    forM_ field.indirect $ \indirect ->
      processType HeadFunType
        (atUseSite (UseSiteField indirect.info.name.cName))
        indirect.typ.c

-- | Process the function types of a typedef
--
-- The function type of the typedef itself gets instances for a newtype: the
-- typedef's own, or its auxiliary newtype.
processTypedef :: C.Decl l Select -> C.Typedef Select -> M l ()
processTypedef decl typedef = case typedef.typ of
    C.TypedefFun sig _hs ->
      processOwn
        (HeadNewtype
          (Hs.assertNs (Proxy @Hs.NsTypeConstr) decl.id.hsName)
          typedef.ann)
        sig
    C.TypedefFunIndirect _indirections sig auxNames ->
      processOwn (HeadNewtype auxNames.typeConstr auxNames.newtypeNames) sig
    -- Function types occur only behind references
    C.TypedefOther{} ->
      pure ()
  where
    processOwn ::
         FunPtrHead
      -> FunSig (C.TypeFunArg Select) (C.Type Select)
      -> M l ()
    processOwn hd = processSig hd (atUseSite UseSiteTypedef)

{-------------------------------------------------------------------------------
  Function types
-------------------------------------------------------------------------------}

-- | Process the outermost function type in a type, and the function types
-- nested in its arguments and result
processType ::
     FunPtrHead -- ^ The head of the instances for the outermost function type
  -> FunTypeLocation
  -> C.Type Select
  -> M l ()
processType hd loc ty =
    forM_ (C.getOutermostFunType ty) $ processSig hd loc

-- | Process a function type, and the function types nested in its arguments
-- and result
processSig ::
     FunPtrHead -- ^ The head of the instances for the function type
  -> FunTypeLocation
  -> FunSig (C.TypeFunArg Select) (C.Type Select)
  -> M l ()
processSig hd loc sig = do
    processFunType hd loc sig
    forM_ (zip [1..] sig.args) $ \(i, arg) ->
      processType HeadFunType (enterPosition loc (argPosition i)) arg.typ
    processType HeadFunType (enterPosition loc FunPositionRes) sig.res

processFunType ::
     FunPtrHead
  -> FunTypeLocation
  -> FunSig (C.TypeFunArg Select) (C.Type Select)
  -> M l ()
processFunType hd loc sig = do
    db <- asks (.db)
    when (isLocalInstance db hd (C.TypeFun sig)) $ do
      mSigOk <- classifyCached loc key funType
      forM_ mSigOk $ \sigOk ->
        modify $ \st ->
          if (hd, key) `Set.member` st.seen then st else st{
              instances = FunPtrInstance{head = hd, sig = sigOk} : st.instances
            , seen      = Set.insert (hd, key) st.seen
            }
  where
    funType :: FunType
    funType = translateFunType sig

    key :: Hs.Type
    key = funTypeHs funType

-- | May this module define the instances with this head for the function type?
--
-- A newtype is defined in this module. An instance for the function type
-- itself needs one of its types defined in this module: it is no orphan, and
-- no other module defines it.
isLocalInstance :: DeclDB l Select -> FunPtrHead -> C.Type Select -> Bool
isLocalInstance db = \case
    HeadNewtype{} -> const True
    HeadFunType   -> isDefinedInCurrentModule db

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

-- | A function type, with the Haskell types of its arguments and result
--
-- The arguments are translated in context @FunArg@, the result in context
-- @FunRes@.
type FunType = FunSig (TranslatedTypeFunArg Select) (TranslatedTypes Select)

translateFunType :: FunSig (C.TypeFunArg Select) (C.Type Select) -> FunType
translateFunType = bimap
    (\arg -> Translated{c = arg, hs = Translation.inContext Translation.FunArg arg})
    (\res -> Translated{c = res, hs = Translation.inContext Translation.FunRes res})

-- | The Haskell type of the function type, as in the instance head
funTypeHs :: FunType -> Hs.Type
funTypeHs = Translation.funType . bimap (.hs) (.hs)

-- | Classify a function type, reporting why it gets no instances
--
-- Classifies and reports each distinct Haskell function type once.
classifyCached ::
     FunTypeLocation
  -> Hs.Type -- ^ The Haskell function type, with 'eraseUnderlying'
  -> FunType
  -> M l (Maybe (FunSig FunPtrArg FunPtrRes))
classifyCached loc key funType = do
    cached <- gets (Map.lookup key . (.cache))
    case cached of
      Just mSig -> pure mSig
      Nothing   -> do
        mSig <- classify loc funType
        modify $ \st -> st{cache = Map.insert key mSig st.cache}
        pure mSig

classify :: FunTypeLocation -> FunType -> M l (Maybe (FunSig FunPtrArg FunPtrRes))
classify loc funType = do
    args <- zipWithM classifyArg [1..] funType.args
    res  <- classifyRes funType.res
    case collectLefts FunSig{args = args, res = res} of
      Left errs -> do
        addMessage $ FunPtrInstancesPositionErrors loc errs
        pure Nothing
      Right sig -> case collectLefts sig of
        Left serialized -> do
          addMessage $ FunPtrInstancesSerialized loc serialized
          pure Nothing
        Right sigDirect ->
          pure $ Just sigDirect
  where
    classifyArg ::
         Int
      -> TranslatedTypeFunArg Select
      -> M l (Either FunPositionError (Either FunPosition FunPtrArg))
    classifyArg i arg =
        fmap direct <$>
          classified loc pos (classifyArgPassingMethod pos arg.c.typ arg.hs)
      where
        pos :: FunPosition
        pos = argPosition i

        direct :: PassingMethod Select -> Either FunPosition FunPtrArg
        direct = \case
          PassDirect ffi   -> Right FunPtrArg{hs = arg.hs, ffi = ffi}
          PassSerialized _ -> Left pos

    classifyRes ::
         TranslatedTypes Select
      -> M l (Either FunPositionError (Either FunPosition FunPtrRes))
    classifyRes res =
        fmap direct <$>
          classified loc FunPositionRes (classifyResPassingMethod res.c res.hs)
      where
        direct :: ResPassingMethod Select -> Either FunPosition FunPtrRes
        direct = \case
          PassResVoid                     -> Right FunPtrRes{hs = res.hs, ffi = Hs.FFIResIOUnit}
          PassResNonVoid (PassDirect t)   -> Right FunPtrRes{hs = res.hs, ffi = Hs.FFIResIO t}
          PassResNonVoid PassSerialized{} -> Left FunPositionRes

-- | Run a classification, reporting its messages
classified ::
     FunTypeLocation
  -> FunPosition
  -> Classified a
  -> M l (Either FunPositionError a)
classified loc pos = runFFITypeM (FunPtrInstancesFFITypes loc pos)

-- | An argument of a function type, which has no name
argPosition :: Int -> FunPosition
argPosition i = FunPositionArg FunArgPosition{index = i, name = Nothing}

{-------------------------------------------------------------------------------
  FFI types
-------------------------------------------------------------------------------}

runFFITypeM ::
     (FFITypesMsg -> FunPtrInstancesMsg)
  -> FFIType.M a
  -> M l a
runFFITypeM mkMsg action = do
    sugars <- asks (.sugars)
    let (r, msgs) = runWriter (runReaderT action sugars)
    forM_ msgs $ addMessage . mkMsg
    pure r

{-------------------------------------------------------------------------------
  Monad
-------------------------------------------------------------------------------}

type M l = ReaderT (Env l) (State St)

data Env l = Env {
      -- | The declarations after @Select@
      db     :: DeclDB l Select
      -- | Location of the current declaration
    , loc    :: C.LocationInfo
    , sugars :: SugarInfos
    }

data St = St {
      -- | In reverse order
      instances :: [FunPtrInstance]
      -- | The instances so far, by head and Haskell function type (with
      -- 'eraseUnderlying')
    , seen      :: Set (FunPtrHead, Hs.Type)
      -- | Signatures of the Haskell function types classified so far (with
      -- 'eraseUnderlying'), or 'Nothing' if they get no instances
    , cache     :: Map Hs.Type (Maybe (FunSig FunPtrArg FunPtrRes))
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
