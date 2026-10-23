module HsBindgen.Backend.Hs.Translation.Function (
    functionDecs
  ) where

import Control.Monad.Reader qualified as Reader
import Data.Kind (Type)
import Data.Text qualified as T
import DeBruijn (Ctx, Env (..), Idx (..), sizeEnv, tabulateEnv, zipWithEnv)

import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Haddock.Translation
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.ForeignImport qualified as Hs.ForeignImport
import HsBindgen.Backend.Hs.Translation.ForeignImport qualified as HsFI
import HsBindgen.Backend.Hs.Translation.Monad (HsM)
import HsBindgen.Backend.Hs.Translation.Monad qualified as HsM
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.SHs.Translation.Common (reverseEnv)
import HsBindgen.Backend.SHs.Translation.Common qualified as SHs
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Config.MangleCandidate (mangleCandidateDefaultFallback)
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.PassBy
import HsBindgen.Frontend.PrettyC qualified as PC
import HsBindgen.Imports hiding (def)
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Translation
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint (NameHint (..))

-- | Bind to a C function
--
-- We seek to always generate and expose a Haskell function with the same name
-- as the C function, and with the same signature. However, we can not always
-- directly call the original C function from Haskell. Only primitive types
-- (e.g, pointers, integers, characters) can be passed directly by value between
-- Haskell and C.
--
-- We say that functions which use only primitive types have a "primitive
-- signature". Functions with a non-primitive signature (for example, functions
-- that accept or return structs by value) cannot be called directly from
-- Haskell and require additional handling. We therefore have multiple objects
-- on both the C and the Haskell sides, and have to be meticulous with names:
--
-- On the C side, we have:
--
-- * @originalC@: The original C function.
--
--   For example:
--
--   > int foo (struct S x);
--
-- On the C side, we generate:
--
-- * @wrapperC@: A C wrapper around @orignalC@ that passes arguments and results
--   by address if necessary. The C wrapper gets a globally unique name. The C
--   wrapper will always have a primitive signature, so that we can import it it
--   in Haskell. If the original C function also has a primitive signature, the
--   C wrapper will have the same signature as the original C function.
--
--   For example:
--
--   > int hs_bindgen_b66ead8229086e95 (struct S * x) {
--   >   return foo(*x);
--   > };
--
-- On the Haskell side, we generate:
--
-- * @wrapperImport@: The foreign import of @wrapperC@. We consider the foreign
--   import an internal, auxiliary function. we give it the same globally unique
--   name as the C wrapper. the foreign import does _not_ have Haddock
--   documentation.
--
--   For example:
--
--   > foreign import ccall safe "hs_bindgen_b66ead8229086e95" hs_bindgen_b66ead8229086e95 ::
--   >   Ptr S -> IO CInt
--
--   Note: in practice, the foreign import is implemented with multiple Haskell
--   declarations to account for FFI types. See 'HsFI.foreignImportDec'.
--
-- * @restoreFun@: A function declaration that /restores/ the name (modulo name
--   mangling) and signature of the original C function. It writes arguments to
--   pointers (where necessary), calls @wrapperImport@, and reads the result
--   from a pointer if necessary.
--
--   For example:
--
--   > foo :: S -> IO CInt
--   > foo x = with x $ \ptr -> hs_bindgen_b66ead8229086e95  ptr
--
functionDecs :: forall l.
     SHs.Safety
  -> C.Decl l Final
  -> C.Function Final
  -> PrescriptiveDeclSpec
  -> HsM [Hs.Decl l]
functionDecs safety decl originalC _spec = do
    env <- Reader.ask
    pure $ aux env
  where
    aux :: HsM.Env -> [Hs.Decl l]
    aux env = concat [
          -- @wrapperC@ is included in @wrapperImport@, so it is not a separate
          -- declaration
          wrapperImport
        , [restoreFun]
        ]
      where
        wrapperImport = getWrapperImportDecl decl originalC safety wrapperCName runInIO
        restoreFun = getRestoreFunDecl decl originalC wrapperCName runInIO

        wrapperCName :: UniqueSymbol
        wrapperCName =
            globallyUnique env.uniqueId env.baseModuleName $
              concat [
                  show safety
                , "_"
                , T.unpack origCName
                ]

        origCName :: Text
        origCName = decl.id.cName.name.text

        runInIO :: RunInIO
        runInIO = functionShouldRunInIO originalC.attrs.purity originalC.res originalC.args

{-------------------------------------------------------------------------------
  Common getters
-------------------------------------------------------------------------------}

-- | Get whether the argument/result should be passed by address
class GetByAddress a where
  byAddress :: a -> Bool

instance GetByAddress (C.FunctionArg Final) where
  byAddress arg = arg.ann.byAddress

instance GetByAddress (C.FunctionRes Final) where
  byAddress res = res.ann.byAddress

-- | Get the C type of an argument/result as it is used in @wrapperC@
class GetWrapperCType a where
  wrapperCType :: a -> C.Type Final

instance GetWrapperCType (C.FunctionRes Final) where
  wrapperCType res = res.ann.wrapperType.c

instance GetWrapperCType (C.FunctionArg Final) where
  wrapperCType arg = arg.ann.wrapperType.c.typ

-- | Get the Haskell type of an argument/result as it is used in @wrapperImport@
class GetImportWrapperType a where
  importWrapperType :: a -> Hs.Type

instance GetImportWrapperType (C.FunctionRes Final) where
  importWrapperType res = res.ann.wrapperType.hs

instance GetImportWrapperType (C.FunctionArg Final) where
  importWrapperType arg = arg.ann.wrapperType.hs

-- | Get the Haskell type of an argument/result as it is used in @restoreFun@
class GetRestoreFunType a where
  restoreFunType :: a -> Hs.Type

instance GetRestoreFunType (C.FunctionRes Final) where
  restoreFunType res = res.typ.hs

instance GetRestoreFunType (C.FunctionArg Final) where
  restoreFunType arg = arg.typ.hs

{-------------------------------------------------------------------------------
  Purity
-------------------------------------------------------------------------------}

-- | Decide whether function has to run in 'IO'.
--
-- Only Haskell-pure functions are allowed to run without 'IO'. See the
-- documentation on 'C.FunctionPurity'.
--
-- But, if any of the function arguments or the function result is passed by
-- address, then the function has to run in 'IO' regardless of the function
-- purity, because pointer manipulation requires 'IO'.
--
functionShouldRunInIO ::
     C.FunctionPurity      -- ^ C function purity (function attribute)
  -> C.FunctionRes Final   -- ^ C result type
  -> [C.FunctionArg Final] -- ^ C parameter types
  -> RunInIO
functionShouldRunInIO purity resType argTypes
  | byAddress resType || any byAddress argTypes
  = RunInIO
  | otherwise
  = case purity of
      C.HaskellPureFunction -> RunPure
      C.CPureFunction       -> RunInIO
      C.ImpureFunction      -> RunInIO

data RunInIO = RunInIO | RunPure

-- | Add @IO@ to a type if necessary
hsIO :: RunInIO -> Hs.Type -> Hs.Type
hsIO = \case
    RunInIO -> Hs.IO
    RunPure -> id

{-------------------------------------------------------------------------------
  @wrapperC@
-------------------------------------------------------------------------------}

-- | Construct a @wrapperC@ declaration
--
-- See als 'functionDecs'.
getWrapperCDecl ::
     C.Decl l Final
  -> C.Function Final
  -> UniqueSymbol
  -> PC.FunDefn
getWrapperCDecl decl originalC wrapperCName = wrapperC
  where
    wrapperC = getWrapperCFunDefn wrapperCName.unique
                  originalC.args originalC.res (T.unpack originalCName)

    originalCName :: Text
    originalCName = decl.id.cName.name.text

getWrapperCFunDefn ::
     String                -- ^ C wrapper name
  -> [C.FunctionArg Final] -- ^ C types of function parameters
  -> C.FunctionRes Final   -- ^ C result type
  -> String                -- ^ original C name
  -> PC.FunDefn
getWrapperCFunDefn  wrapperName args res origName
    | C.isVoid (wrapperCType res)
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName C.TypeVoid C.ImpureFunction (wrapperCType <$> args') $
        PC.CSList $
          PC.CSStatement
            (PC.ExpressionStatement $ PC.Call origName (callArgs args' (PC.argsToIdx args')))
            PC.CSNil

    | byAddress res
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName C.TypeVoid C.ImpureFunction (fmap wrapperCType args' :> wrapperCType res) $
        PC.CSList $
          PC.CSStatement
            (PC.ExpressionStatement $ PC.Assign (PC.LDeRef (PC.LVar IZ)) $
              PC.Call origName (callArgs args' (IS <$> PC.argsToIdx args')))
            PC.CSNil

    | otherwise
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName (wrapperCType res) C.ImpureFunction (wrapperCType <$> args') $
        PC.CSList $
          PC.CSStatement
            (PC.ExpressionStatement $ PC.Return $
              PC.Call origName (callArgs args' (PC.argsToIdx args')))
            PC.CSNil
  where
    callArgs :: Env ctx' (C.FunctionArg Final) -> Env ctx' (Idx ctx) -> [PC.Expr ctx]
    callArgs tys ids = toList (zipWithEnv f tys ids)
      where f ty idx = if byAddress ty then PC.DeRef (PC.Var idx) else PC.Var idx

{-------------------------------------------------------------------------------
  @wrapperImport@
-------------------------------------------------------------------------------}

-- | Construct a @wrapperImport@ declaration
--
-- See als 'functionDecs'.
getWrapperImportDecl :: forall l.
     C.Decl l Final
  -> C.Function Final
  -> SHs.Safety
  -> UniqueSymbol
  -> RunInIO
  -> [Hs.Decl l]
getWrapperImportDecl decl originalC safety wrapperCName runInIO = wrapperImport
  where
    wrapperImport :: [Hs.Decl l]
    wrapperImport =
        HsFI.foreignImportDec
          (Hs.ForeignImport.FunName wrapperCName)
          params
          result
          (uniqueCDeclName wrapperCName)
          (CallConvUserlandCapi wrapperCPretty)
          (Origin.Function originalC)
          safety

    wrapperCPretty :: CWrapper
    wrapperCPretty = CWrapper {
          definition = PC.prettyFunDefn wrapperC ""
        }

    wrapperC :: PC.FunDefn
    wrapperC = getWrapperCDecl decl originalC wrapperCName

    params :: [Hs.ForeignImport.FunParam]
    params = [
          Hs.ForeignImport.FunParam {
              hsParam =
                Hs.FunctionParameter{
                  typ     = importWrapperType arg
                , comment = Nothing
                }
            }
        | arg <- originalC.args
        ] ++ [
          -- Result types that have to be passed by address become parameters.
          Hs.ForeignImport.FunParam {
              hsParam =
                Hs.FunctionParameter{
                  typ     = importWrapperType originalC.res
                , comment = Nothing
                }
            }
        | byAddress originalC.res
        ]

    -- Result types that have to be passed by address become parameters.
    result :: Hs.ForeignImport.FunRes
    result
        -- A type that is not supported by the Haskell FFI as a function result.
        -- We pass it as a function parameter instead.
        | byAddress originalC.res
        = mkFunRes voidTyp
        -- A "normal" result type that is supported by the Haskell FFI.
        | otherwise
        = mkFunRes $ importWrapperType originalC.res
      where
        voidTyp = Hs.PrimType Hs.PrimUnit
        mkFunRes :: Hs.Type -> Hs.ForeignImport.FunRes
        mkFunRes ty = Hs.ForeignImport.FunRes {
              hsType   = hsIO runInIO ty
            }

{-------------------------------------------------------------------------------
  @restoreFun@
-------------------------------------------------------------------------------}

-- | Construct a @restoreFun@ declaration
--
-- See als 'functionDecs'.
getRestoreFunDecl :: forall l.
     C.Decl l Final
  -> C.Function Final
  -> UniqueSymbol
  -> RunInIO
  -> Hs.Decl l
getRestoreFunDecl decl originalC wrapperCName runInIO = restoreFun
  where
    restoreFun :: Hs.Decl l
    restoreFun =
        Hs.DeclFunction $ Hs.FunctionDecl{
            name       = restoreFunName
          , parameters = restoreFunParams
          , result     = hsIO runInIO $ restoreFunType originalC.res
          , body       = getRestoreFunBody originalC (Hs.InternalName wrapperCName)
          , origin     = Origin.Function originalC
          , pragmas    = []
          , comment    = restoreFunComment <> mbIoComment
          }

    restoreFunName = Hs.ExportedName $ Hs.assertNs (Proxy @Hs.NsVar) decl.id.hsName

    restoreFunComment :: Maybe HsDoc.Comment
    restoreFunParams :: [Hs.FunctionParameter Hs.Type]
    (restoreFunComment, restoreFunParams) =
      let params :: [(Maybe Text, Hs.FunctionParameter Hs.Type)]
          params = [ ( fmap (.cName.text) arg.name
                    , Hs.FunctionParameter{
                        typ     = restoreFunType arg
                      , comment = Nothing
                      })
                    | arg <- originalC.args
                    ]
      in  mkHaddocksDecorateParams decl params

    mbIoComment :: Maybe HsDoc.Comment
    mbIoComment = ioComment originalC.attrs.purity

-- | A haddock comment related to function purity
ioComment ::
     C.FunctionPurity     -- ^ C function purity (function attribute)
  -> Maybe HsDoc.Comment
ioComment purity =
    case purity of
      C.HaskellPureFunction -> Just constComment
      C.CPureFunction       -> Just pureComment
      C.ImpureFunction      -> Nothing
  where
    -- "Marked @__attribute((const))__@"
    --
    -- Haskell-pure functions can be safely encapsulated using
    -- 'unsafePerformIO' to create a Haskell-pure function. We include a
    -- comment in the generated bindings to this effect.
    constComment :: HsDoc.Comment
    constComment = HsDoc.paragraph [
        HsDoc.TextContent "Marked"
      , HsDoc.Monospace
        [ HsDoc.Bold
          [ HsDoc.TextContent "attribute((const))" ]
        ]
      ]

    -- "Marked @__attribute((pure))__@"
    --
    -- C-pure functions can be safely encapsulated using 'unsafePerformIO' to
    -- create a Haskell-pure functions. We include a comment in the generated
    -- bindings to this effect.
    pureComment :: HsDoc.Comment
    pureComment = HsDoc.paragraph [
        HsDoc.TextContent "Marked"
      , HsDoc.Monospace
        [ HsDoc.Bold
          [ HsDoc.TextContent "attribute((pure))" ]
        ]
      ]

getRestoreFunBody ::
     C.Function Final
  -> Hs.TermName
  -> SHs.ClosedExpr
getRestoreFunBody originalC wrapperImportName =
    if byAddress originalC.res || any byAddress originalC.args
    then bodyExpr
    else simpleBodyExpr
  where
    -- | The body of the function if no function arguments or results have to be
    -- passed by address.
    --
    -- For example, simply:
    --
    -- > foo
    --
    simpleBodyExpr :: SHs.ClosedExpr
    simpleBodyExpr = SHs.EFree wrapperImportName

    -- | The body of the function if one or more function arguments or results
    -- have to be passed by address.
    --
    -- For example:
    --
    -- >  \x0 -> \x1 -> \x2 -> \x3 ->
    -- >    with x0 $ \y4 -> with x2 $ \y5 ->
    -- >      allocaAndPeek $ \z6 ->
    -- >        foo y4 x1 (unsafeFromPtr y5) x3 z6
    --
    bodyExpr :: SHs.ClosedExpr
    bodyExpr =
      -- construct lambdas for all function arguments
      lambdas EmptyEnv originalC.args $ \env ->
        -- pass function arguments by address if necessary
        passArgsByAddressIfNecessary env $ \args ->
          -- pass the function result by address if necessary
          passResultByAddressIfNecessary args $ \args' ->
            -- call the foreign import
            callForeignImport args'

    -- | Construct a string of lambdas
    --
    -- For example:
    --
    -- > ... \x0 -> \x1 -> \x2 -> \x3 -> ...
    --
    lambdas ::
         forall ctx.
         -- | Context of in-scope variables
         Env ctx (C.FunctionArg Final)
         -- | Function arguments to create lambdas for
      -> [C.FunctionArg Final]
         -- | Construct the body of the string of lambdas
      -> (forall ctx'. Env ctx' (C.FunctionArg Final) -> SHs.SExpr ctx')
      -> SHs.SExpr ctx
    lambdas env0 xs0 kont = go env0 xs0
      where
        -- | Run down the context of function arguments, and include a lambda
        -- for each.
        go ::
             forall ctx'.
             Env ctx' (C.FunctionArg Final)
          -> [C.FunctionArg Final]
          -> SHs.SExpr ctx'
        go env []     = kont env
        go env (x:xs) = SHs.ELam (mkNameHint x) $ go (env :> x) xs

    -- | Construct expressions to pass function arguments by address, if
    -- necessary
    --
    -- For example :
    --
    -- > ... with x0 $ \y4 -> with x2 $ \y5 -> ...
    --
    -- We do this only if the function argument requires restoration (see
    -- 'requiresRestore')
    --
    -- If the function argument is @const@-qualified, then we also record this
    -- information for later in the 'Var' type.
    --
    passArgsByAddressIfNecessary ::
         -- | Context of in-scope variables
         Env ctx (C.FunctionArg Final)
         -- | Construct the body of the string of 'Foreign.Marshal.Utils.with's
      -> (forall ctx'. [(Var ctx')] -> SHs.SExpr ctx')
      -> SHs.SExpr ctx
    passArgsByAddressIfNecessary env0 kont =
        -- NOTE: The environment is in reverse order with respect to the
        -- function arguments, hence the need to reverse here.
        let envTypes = reverseEnv env0
            envIdxs = reverseEnv (tabulateEnv (sizeEnv env0) id)
        in go envTypes envIdxs []
      where
        --  | Run down the context of in-scope variables, and include a 'Foreign.Marshal.Utils.with' if
        -- if the variable is a 'PassByAddress'.
        go ::
             Env ctx' (C.FunctionArg Final)
          -> Env ctx' (Idx ctx)
          -> [(Var ctx)]
          -> SHs.SExpr ctx
        go EmptyEnv EmptyEnv zs = kont (reverse zs) -- reverse again!
        go (xs :> x) (ys :> y) zs
          | x.ann.byAddress
          = SHs.eAppMany (SHs.eBindgenGlobal Capi_with) $
              let wrapPtrConst = C.isErasedTypeConstQualified x.typ.c.typ in
              [ SHs.EBound y
              , SHs.ELam (mkNameHint x) $
                  go xs (IS <$> ys) (Var IZ wrapPtrConst : fmap succVar zs)
              ]
          | otherwise
          = go xs ys (Var y False : zs)

    -- | Construct an expression to pass the function result by address, if
    -- necessary
    --
    -- For example:
    --
    -- > ... allocaAndPeek $ \z6 -> ...
    --
    -- We do this only if the function result requires restoration (see
    -- 'requiresRestore')
    --
    passResultByAddressIfNecessary ::
         -- | Context of in-scope variables to be passed to the foreign import
         [(Var ctx)]
         -- | Construct the body of the lambda
      -> (forall ctx'. [Var ctx'] -> SHs.SExpr ctx')
      -> SHs.SExpr ctx
    passResultByAddressIfNecessary zs kont
      | byAddress originalC.res
      = let zs' = fmap succVar zs ++ [Var IZ False] in
        SHs.EApp
          (SHs.eBindgenGlobal Capi_allocaAndPeek)
          (SHs.ELam (NameHint "res") $ kont zs')
      | otherwise
      = kont zs

    -- | Construct a call to the foreign import
    --
    -- For example (@foo@ is the name of the foreign import):
    --
    -- > ... foo y4 x1 (unsafeFromPtr y5) x3 z6
    --
    callForeignImport ::
         -- | Context of in-scope variables to be passed to the foreign import
         [Var ctx]
      -> SHs.SExpr ctx
    callForeignImport args = SHs.eAppMany (SHs.EFree wrapperImportName) (map exprVar args)

{-------------------------------------------------------------------------------
  Variables
-------------------------------------------------------------------------------}

-- | A variable with some additional information
type Var :: Ctx -> Type
data Var ctx = Var {
    -- | The name (i.e., DeBruijn index) of the variable
    name :: Idx ctx
    -- | Whether to wrap the variable in a 'HsBindgen.Runtime.PtrConst.PtrConst'
    --
    -- 'Foreign.Marshal.Utils.with' and 'HsBindgen.Runtime.IncompleteArray.allocaAndPeek' always use 'Foreign.Ptr.Ptr' rather than 'HsBindgen.Runtime.PtrConst.PtrConst', hence
    -- the need to convert 'Foreign.Ptr.Ptr' to 'HsBindgen.Runtime.PtrConst.PtrConst' manually.
  , wrapPtrConst :: Bool
  }

-- | Shift the variable
succVar :: Var ctx -> Var (S ctx)
succVar var = Var {
      name = IS var.name
    , wrapPtrConst = var.wrapPtrConst
    }

-- | Turn the variable into an expression
--
-- For example:
--
-- > unsafeFromPtr y5
--
exprVar :: Var ctx -> SHs.SExpr ctx
exprVar var
  | var.wrapPtrConst
  = SHs.EApp (SHs.eBindgenGlobal PtrConst_unsafeFromPtr) (SHs.EBound var.name)
  | otherwise
  = SHs.EBound var.name

{-------------------------------------------------------------------------------
  Name hints
-------------------------------------------------------------------------------}

mkNameHint :: C.FunctionArg Final -> NameHint
mkNameHint arg =
    toHint $ case fmap (.cName.text) arg.name of
      Nothing -> fallback
      Just x  -> mangleCandidateDefaultFallback fallback x
  where
    fallback :: Hs.Name Hs.NsVar
    fallback = Hs.UnsafeName $ if byAddress arg then "ptr" else "x"

    toHint :: Hs.Name Hs.NsVar -> NameHint
    toHint n = NameHint $ T.unpack n.text

