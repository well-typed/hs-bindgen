module HsBindgen.Backend.Hs.Translation.Function (
    functionDecs
  , getMainHashIncludeArg
  ) where

import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as T
import Data.Type.Equality ((:~:) (Refl))
import DeBruijn (Ctx, Env (..), Idx (..), sizeEnv, tabulateEnv, zipWithEnv)
import DeBruijn.Add (Add, lzeroAdd, swapAdd, unrzeroAdd)
import GHC.Records

import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Config
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Haddock.Translation
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.ForeignImport qualified as Hs.ForeignImport
import HsBindgen.Backend.Hs.Translation.ForeignImport qualified as HsFI
import HsBindgen.Backend.Hs.Translation.Type qualified as Type
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.SHs.Translation.Common qualified as SHs
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Config.Prelims
import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass (AdjustedFrom (..))
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports hiding (def)
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint (NameHint (..))
import HsBindgen.PrettyC qualified as PC

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
-- Haskell and require additional handling.
--
-- We therefore have multiple objects on both the C and the Haskell sides, and
-- have to be meticulous with names:
--
-- On the C side, we have:
--
-- - The original C function;
--
-- On the C side, we generate:
--
-- - A C wrapper (beware, we use the word "wrapper" only in this case); the C
--   wrapper is C code, has a globally unique name, and we bind to it using the
--   "ccall" calling convention (the combination of the C wrapper and the
--   "ccall" foreign import is what we refer to as userland CAPI).
--
--   The C wrapper will always have a primitive signature, so that we can call
--   it from Haskell. If the original C function also has a primitive signature,
--   the C wrapper will have the same signature as the original C function.
--
-- On the Haskell side, we generate:
--
-- - The foreign import of the C wrapper. We consider the foreign import an
--   internal, auxiliary function; we give it the same globally unique name as
--   the C wrapper; the foreign import does _not_ have Haddock documentation.
--
-- - We generate a function declaration which we refer to as
--   "restoreOrigSignature", and which has the same signature and name (modulo
--   name mangling) as the original C function.
functionDecs ::
     HasCallStack
  => SHs.Safety
  -> UniqueId
  -> HaddockConfig
  -> BaseModuleName
  -> C.Sizeofs
  -> C.DeclInfo Final
  -> C.Function Final
  -> PrescriptiveDeclSpec
  -> [Hs.Decl]
functionDecs safety uniqueId haddockConfig moduleName sizeofs info origCFun _spec =
    concat [
        foreignImport
      , [restoreOrigSignature]
      ]
  where
    origName :: Text
    origName = info.id.cName.name.text

    mangledOrigId :: Hs.Identifier
    mangledOrigId = info.id.unsafeHsName

    mangledOrigName :: Hs.Name Hs.NsVar
    mangledOrigName = Hs.unsafeHsIdHsName mangledOrigId

    cWrapperName :: UniqueSymbol
    cWrapperName =
        globallyUnique uniqueId moduleName $
          concat [
              show safety
            , "_"
            , T.unpack origName
            ]

    primResult :: PassResBy
    primResult = classifyResPassingMethod origCFun.res

    primParams :: [PassArgBy]
    primParams = map (\arg -> classifyArgPassingMethod (arg.argTyp)) origCFun.args

    foreignImport :: [Hs.Decl]
    foreignImport =
        HsFI.foreignImportDec
          sizeofs
          (Hs.ForeignImport.FunName cWrapperName)
          foreignImportParams
          foreignImportResult
          (uniqueCDeclName cWrapperName)
          (CallConvUserlandCapi cWrapper)
          (Origin.Function origCFun)
          safety
      where
        cWrapperDecl :: PC.FunDefn
        cWrapperDecl =
            getCWrapperDecl
              (T.unpack origName)
              cWrapperName.unique
              primResult
              primParams

        cWrapper :: CWrapper
        cWrapper = CWrapper {
              definition     = PC.prettyFunDefn cWrapperDecl ""
            , hashIncludeArg = getMainHashIncludeArg info
            }

    foreignImportParams :: [Hs.ForeignImport.FunParam]
    foreignImportParams = [
          Hs.ForeignImport.FunParam {
              hsParam =
                Hs.FunctionParameter{
                  typ     = toPrimitiveType Type.FunArg arg
                , comment = Nothing
                }
            }
        | arg <- primParams
        ] ++ [
          Hs.ForeignImport.FunParam {
              hsParam =
                Hs.FunctionParameter{
                  typ     = toPrimitiveType Type.FunArg primResult
                , comment = Nothing
                }
            }
        | case primResult of
            -- A type that is not supported by the Haskell FFI as a function result.
            -- We pass it as a function parameter instead.
            PassByAddress {} -> True
            -- A "normal" result type that is supported by the Haskell FFI.
            PassByValue {} -> False
        ]

    -- When translating a 'C.Type' there are C types which we cannot pass
    -- directly using C FFI. We need to distinguish these.
    --
    -- Function arguments and result have to be passed either by value or by
    -- address. Result types that have to be passed by address become
    -- parameters.
    foreignImportResult :: Hs.ForeignImport.FunRes
    foreignImportResult = case primResult of
        -- A type that is not supported by the Haskell FFI as a function result.
        -- We pass it as a function parameter instead.
        PassByAddress {} -> mkFunRes (PassByValue C.TypeVoid)
        -- A "normal" result type that is supported by the Haskell FFI.
        PassByValue {} -> mkFunRes primResult
      where
        mkFunRes :: PassResBy -> Hs.ForeignImport.FunRes
        mkFunRes passBy = Hs.ForeignImport.FunRes {
              hsType   = mbHsIO $ toPrimitiveType Type.FunRes passBy
            }

    restoreOrigSignature :: Hs.Decl
    restoreOrigSignature =
        getRestoreOrigSignatureDecl
          mangledOrigName
          (Hs.InternalName cWrapperName)
          primResult
          primParams
          (mbHsIO $ toOrigType Type.FunRes primResult)
          restoreOrigSignatureParams
          origCFun
          (mbRestoreOrigSignatureComment <> mbIoComment)

    mbRestoreOrigSignatureComment :: Maybe HsDoc.Comment
    restoreOrigSignatureParams :: [Hs.FunctionParameter]
    (mbRestoreOrigSignatureComment, restoreOrigSignatureParams) =
      let params :: [(Maybe Text, Hs.FunctionParameter)]
          params = [ ( fmap (.cName.text) arg.name
                     , Hs.FunctionParameter{
                         typ     = toOrigType Type.FunArg (classifyArgPassingMethod arg.argTyp)
                       , comment = Nothing
                       })
                     | arg <- origCFun.args
                     ]
      in  mkHaddocksDecorateParams haddockConfig info mangledOrigName params

    runsInIO :: Bool
    runsInIO = functionShouldRunInIO origCFun.attrs.purity primResult primParams

    mbHsIO :: HsType -> HsType
    mbHsIO | runsInIO  = HsIO
           | otherwise = id

    mbIoComment :: Maybe HsDoc.Comment
    mbIoComment = ioComment origCFun.attrs.purity

getMainHashIncludeArg :: C.DeclInfo Final -> HashIncludeArg
getMainHashIncludeArg info = NonEmpty.head info.headerInfo.mainHeaders

{-------------------------------------------------------------------------------
  Purity
-------------------------------------------------------------------------------}

-- | Decide whether a function has to run in 'IO'.
--
-- Only Haskell-pure functions are allowed to run without 'IO'. See the
-- documentation on 'C.FunctionPurity'.
--
-- But, if any of the function arguments require restoration (see
-- 'requiresRestore'), then the function has to run in 'IO' regardless of the
-- function purity, because pointer manipulation requires 'IO'.
--
functionShouldRunInIO ::
     C.FunctionPurity -- ^ C function purity (function attribute)
  -> PassResBy        -- ^ C result type
  -> [PassArgBy]      -- ^ C parameter types
  -> Bool
functionShouldRunInIO purity resType argTypes
  | requiresRestore resType || any requiresRestore argTypes
  = True
  | otherwise
  = case purity of
      C.HaskellPureFunction -> False
      C.CPureFunction       -> True
      C.ImpureFunction      -> True

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

{-------------------------------------------------------------------------------
  Argument passing method
-------------------------------------------------------------------------------}

-- | Classification of the type of a function argument: it is either passed by
-- value or by address.
type PassArgBy = PassBy (C.TypeFunArg Final) (C.Type Final)

-- | Classification of the type of a function result: it is either passed by
-- value or by address.
type PassResBy = PassBy (C.Type Final) (C.Type Final)

-- | Classification of the type of a function argument\/result: it is either
-- passed by value or by address.
data PassBy byValue byAddress =
      -- | Types passed by value.
      --
      -- Ordinary, "primitive" types which can be handled by Haskell FFI
      -- directly.
      PassByValue byValue
      -- | Types passed by address: union, struct, and complex
      --
      -- These have to be passed by address, because they can not be handled by
      -- the Haskell FFI directly.
    | PassByAddress byAddress
  deriving Show

isPassByAddress :: PassBy byValue byAddress -> Bool
isPassByAddress = \case
    PassByValue {} -> False
    PassByAddress {} -> True

-- | Classify how a function result is passed from C to Haskell
classifyResPassingMethod :: HasCallStack => C.Type Final -> PassResBy
classifyResPassingMethod res
  -- Heap types
  | C.isCanonicalTypeStruct res ||
    C.isCanonicalTypeUnion res ||
    C.isCanonicalTypeComplex res
  = PassByAddress res

  | C.isCanonicalTypeArray res
  = panicPure "classifyResPassingMethod: an array can not be the result type of a function"

  | C.isCanonicalTypeFunction res
  = panicPure "classifyResPassingMethod: a function can not be the result type of a function"

  -- Other types
  | otherwise
  = PassByValue res

-- | Classify how a function argument is passed from Haskell to C
classifyArgPassingMethod :: HasCallStack => C.TypeFunArg Final -> PassArgBy
classifyArgPassingMethod arg
  -- Heap types
  | C.isCanonicalTypeStruct arg.typ ||
    C.isCanonicalTypeUnion arg.typ ||
    C.isCanonicalTypeComplex arg.typ
  = if arg.ann == NotAdjusted
    then PassByAddress arg.typ
    else panicPure
          "classifyArgPassingMethod: found a function argument/result type that is \
          \a struct/union/complex with an unexpected annotation. \
          \Is there a bug in the AdjustTypes frontend pass?"

  | C.isCanonicalTypeArray arg.typ
  = panicPure
      "classifyArgPassingMethod: found a function argument/result type that is an array, \
      \which is unexpected because it should have been adjusted to a pointer. \
      \Is there a bug in the AdjustTypes frontend pass?"

  | C.isCanonicalTypeFunction arg.typ
  = panicPure
      "classifyArgPassingMethod: found a function argument/result type that is a function, \
      \which is unexpected because it should have been adjusted to a pointer. \
      \Is there a bug in the AdjustTypes frontend pass?"

  -- Other types
  | otherwise
  = PassByValue arg

class ToWrapperType a where
-- | Recover type used in the C wrapper
  toWrapperType :: a -> C.Type Final

instance ToWrapperType PassArgBy where
  toWrapperType = \case
      PassByValue argTy -> argTy.typ
      PassByAddress ty -> C.TypePointers 1 ty

instance ToWrapperType PassResBy where
  toWrapperType = \case
    PassByValue ty -> ty
    PassByAddress ty -> C.TypePointers 1 ty

class ToPrimitiveType a where
  -- | Recover type used in the foreign import
  toPrimitiveType :: Type.TypeContext -> a -> HsType

instance ToPrimitiveType PassArgBy where
  toPrimitiveType ctx = \case
      PassByValue argTy -> Type.inContext ctx argTy
      PassByAddress ty -> Type.inContext ctx $ C.TypePointers 1 ty

instance ToPrimitiveType PassResBy where
  toPrimitiveType ctx = \case
      PassByValue ty -> Type.inContext ctx ty
      PassByAddress ty -> Type.inContext ctx $ C.TypePointers 1 ty

class ToOrigType a where
  -- | Recover type used in "restoreOrigSignature"
  toOrigType :: Type.TypeContext -> a -> HsType

instance ToOrigType PassArgBy where
  toOrigType ctx = \case
      PassByValue argTy -> Type.inContext ctx argTy
      PassByAddress ty -> Type.inContext ctx ty

instance ToOrigType PassResBy where
  toOrigType ctx = \case
      PassByValue ty -> Type.inContext ctx ty
      PassByAddress ty -> Type.inContext ctx ty

-- | Check whether a type needs to be restored
--
-- If a type is passed by address through the Haskell FFI when the C function
-- takes it by value, then we restore the original type in the Haskell binding
-- by peeking/poking the value from/to the address. This requires performing
-- 'IO'.
--
requiresRestore :: PassBy byValue byAddress -> Bool
requiresRestore = \case
    PassByValue{} -> False
    PassByAddress{} -> True

{-------------------------------------------------------------------------------
  Userland-API C wrapper
-------------------------------------------------------------------------------}

-- | Userland-API C wrapper
getCWrapperDecl ::
     String      -- ^ original C name
  -> String      -- ^ C wrapper name
  -> PassResBy   -- ^ C result type
  -> [PassArgBy] -- ^ C types of function parameters
  -> PC.FunDefn
getCWrapperDecl origName wrapperName res args
    | C.isVoid (toWrapperType res)
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName C.TypeVoid C.ImpureFunction (toWrapperType <$> args') $
        PC.CSList $
          PC.CSStatement
            (PC.ExpressionStatement $ PC.Call origName (callArgs args' (PC.argsToIdx args')))
            PC.CSNil

    | isPassByAddress res
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName C.TypeVoid C.ImpureFunction (fmap toWrapperType args' :> toWrapperType res) $
        PC.CSList $
          PC.CSStatement
            (PC.ExpressionStatement $ PC.Assign (PC.LDeRef (PC.LVar IZ)) $ PC.Call origName (callArgs args' (IS <$> PC.argsToIdx args')))
            PC.CSNil

    | otherwise
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName (toWrapperType res) C.ImpureFunction (toWrapperType <$> args') $
        PC.CSList $
          PC.CSStatement
            (PC.ExpressionStatement $ PC.Return $ PC.Call origName (callArgs args' (PC.argsToIdx args')))
            PC.CSNil
  where
    callArgs :: Env ctx' PassArgBy -> Env ctx' (Idx ctx) -> [PC.Expr ctx]
    callArgs tys ids = toList (zipWithEnv f tys ids)
      where f ty idx = if isPassByAddress ty then PC.DeRef (PC.Var idx) else PC.Var idx

{-------------------------------------------------------------------------------
  RestoreOrigSignature
-------------------------------------------------------------------------------}

-- | Generate a function declaration restoring the signature of the original C
-- function.
getRestoreOrigSignatureDecl ::
     Hs.Name Hs.NsVar       -- ^ name of new function
  -> Hs.Name Hs.NsVar       -- ^ name of foreign import
  -> PassResBy              -- ^ C result type
  -> [PassArgBy]            -- ^ C types of function parameters
  -> HsType                 -- ^ Haskell result type
  -> [Hs.FunctionParameter] -- ^ Haskell function parameters
  -> C.Function Final       -- ^ original C function
  -> Maybe HsDoc.Comment    -- ^ function comment
  -> Hs.Decl
getRestoreOrigSignatureDecl hiName loName primResult primParams hsResult hsParams cFunc mbComment =
    Hs.DeclFunction $ Hs.FunctionDecl{
        name       = hiName
      , parameters = hsParams
        -- NOTE: the result type only includes 'IO' if 'functionShouldRunInIO'
        -- said to include it.
      , result     = hsResult
      , body       =
          if requiresRestore primResult || any requiresRestore primParams
          then bodyExpr
          else noRecoveryBodyExpr
      , origin     = Origin.Function cFunc
      , pragmas    = []
      , comment    = mbComment
      }
  where
    -- | The body of the function if no function arguments or results require
    -- restoration (see 'requiresRestore').
    --
    -- For example, simply:
    --
    -- > foo
    --
    noRecoveryBodyExpr :: SHs.ClosedExpr
    noRecoveryBodyExpr = SHs.EFree loName

    -- | The body of the function if one or more function arguments or results
    -- require restoration (see 'requiresRestore').
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
      lambdas EmptyEnv (zipWith3 mkFunArg cParamNames primParams hsParams) $ \env ->
        -- pass function arguments by address if necessary
        passArgsByAddressIfNecessary env $ \args ->
          -- pass the function result by address if necessary
          passResultByAddressIfNecessary args $ \args' ->
            -- call the foreign import
            callForeignImport args'
      where
        cParamNames :: [Maybe Text]
        cParamNames = [ fmap (.cName.text) arg.name
                      | arg <- cFunc.args
                      ]

        mkFunArg :: Maybe Text -> PassArgBy -> Hs.FunctionParameter -> FunArg
        mkFunArg mbCName passBy param = FunArg{
            typ        = passBy
          , cParamName = mbCName
          , funParam   = param
          }

    -- | Construct a string of lambdas
    --
    -- For example:
    --
    -- > ... \x0 -> \x1 -> \x2 -> \x3 -> ...
    --
    lambdas ::
         forall ctx.
         -- | Context of in-scope variables
         Env ctx VarInfo
         -- | Function arguments to create lambdas for
      -> [FunArg]
         -- | Construct the body of the string of lambdas
      -> (forall ctx'. Env ctx' VarInfo -> SHs.SExpr ctx')
      -> SHs.SExpr ctx
    lambdas env0 xs0 kont = go env0 xs0
      where
        -- | Run down the context of function arguments, and include a lambda
        -- for each.
        go ::
             forall ctx'.
             Env ctx'  VarInfo
          -> [FunArg]
          -> SHs.SExpr ctx'
        go env []     = kont env
        go env (x:xs) =
            let varInfo = funArgToVarInfo x
            in  SHs.ELam varInfo.nameHint $ go (env :> varInfo) xs

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
         Env ctx VarInfo
         -- | Construct the body of the string of 'with's
      -> (forall ctx'. [(Var ctx')] -> SHs.SExpr ctx')
      -> SHs.SExpr ctx
    passArgsByAddressIfNecessary env0 kont =
        -- NOTE: The environment is in reverse order with respect to the
        -- function arguments, hence the need to reverse here.
        let envTypes = reverseEnv env0
            envIdxs = reverseEnv (tabulateEnv (sizeEnv env0) id)
        in go envTypes envIdxs []
      where
        --  | Run down the context of in-scope variables, and include a 'with' if
        -- if the variable is a 'PassByAddress'.
        go ::
             Env ctx' VarInfo
          -> Env ctx' (Idx ctx)
          -> [(Var ctx)]
          -> SHs.SExpr ctx
        go EmptyEnv EmptyEnv zs = kont (reverse zs) -- reverse again!
        go (xs :> x) (ys :> y) zs = case x.typ of
            PassByAddress ty -> SHs.eAppMany (SHs.eBindgenGlobal Capi_with) $
              let wrapPtrConst = C.isErasedTypeConstQualified ty in
              [ SHs.EBound y
              , SHs.ELam x.ptrNameHint $
                  go xs (IS <$> ys) (Var IZ wrapPtrConst : fmap succVar zs)
              ]

            PassByValue{} -> go xs ys (Var y False : zs)

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
      | requiresRestore primResult
      = let zs' = fmap succVar zs ++ [Var IZ False] in
        SHs.EApp
          (SHs.eBindgenGlobal Capi_allocaAndPeek)
          (SHs.ELam "res" $ kont zs')
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
    callForeignImport args = SHs.eAppMany (SHs.EFree loName) (map exprVar args)

{-------------------------------------------------------------------------------
  Function arguments
-------------------------------------------------------------------------------}

-- | Information about a function argument
data FunArg = FunArg {
    typ        :: PassArgBy
  , cParamName :: Maybe Text
  , funParam   :: Hs.FunctionParameter
  }

funArgToVarInfo :: FunArg -> VarInfo
funArgToVarInfo arg = VarInfo {
      typ = arg.typ
    , optNameHint = fmap (fromString . T.unpack) arg.cParamName
    }

{-------------------------------------------------------------------------------
  Variables
-------------------------------------------------------------------------------}

-- | A variable with some additional information
type Var :: Ctx -> Type
data Var ctx = Var {
    -- | The name (i.e., DeBruijn index) of the variable
    name :: Idx ctx
    -- | Whether to wrap the variable in a 'PtrConst'
    --
    -- 'with' and 'allocaAndPeek' always use 'Ptr' rather than 'PtrConst', hence
    -- the need to convert 'Ptr' to 'PtrConst' manually.
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
  Variable info
-------------------------------------------------------------------------------}

-- | Information for variables corresponding to function arguments
data VarInfo = VarInfo {
    typ         :: PassArgBy
  , optNameHint :: Maybe NameHint
  }

instance HasField "nameHint" VarInfo NameHint where
  getField vinfo = fromMaybe "x" vinfo.optNameHint

instance HasField "ptrNameHint" VarInfo NameHint where
  getField vinfo = fromMaybe "ptr" vinfo.optNameHint

{-------------------------------------------------------------------------------
  Environment
-------------------------------------------------------------------------------}

-- | Reverse an 'Env'
reverseEnv :: forall ctx a. Env ctx a -> Env ctx a
reverseEnv = \env -> go (lzeroAdd (sizeEnv env)) EmptyEnv env
  where
    go ::
         forall ctx1 ctx2 ctx3.
         -- | Proof that the output environment 's size is the same as the the
         -- sum of the two input environment's sizes
         Add ctx1 ctx2 ctx3
         -- | Accumulator
      -> Env ctx1 a
         -- | Input to reverse
      -> Env ctx2 a
      -> Env ctx3 a
    go proof acc EmptyEnv = case unrzeroAdd proof of
        Refl -> acc
    go proof acc (xs :> x) = go (swapAdd proof) (acc :> x) xs
