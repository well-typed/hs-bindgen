module HsBindgen.Backend.Hs.Translation.Function (
    functionDecs
  , getMainHashIncludeArg
  ) where

import Control.Monad.Reader qualified as Reader
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as T
import Data.Type.Equality ((:~:) (Refl))
import DeBruijn (Add, Ctx, Env (..), Idx (..), lzeroAdd, sizeEnv, swapAdd,
                 tabulateEnv, unrzeroAdd, zipWithEnv)

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
import HsBindgen.Backend.SHs.Translation.Common qualified as SHs
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Config.MangleCandidate (mangleCandidateDefaultFallback)
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.Translation qualified as Translation
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
  -> C.DeclInfo Final
  -> C.Function Final
  -> PrescriptiveDeclSpec
  -> HsM [Hs.Decl l]
functionDecs safety info origCFun _spec = do
    env <- Reader.ask
    pure $ aux env
  where
    aux :: HsM.Env -> [Hs.Decl l]
    aux env = concat [
          foreignImport
        , [restoreOrigSignature]
        ]
      where
        origCName :: Text
        origCName = info.id.cName.name.text

        origHsName :: Hs.TermName
        origHsName = Hs.ExportedName $ Hs.assertNs (Proxy @Hs.NsVar) info.id.hsName

        cWrapperName :: UniqueSymbol
        cWrapperName =
            globallyUnique env.uniqueId env.baseModuleName $
              concat [
                  show safety
                , "_"
                , T.unpack origCName
                ]

        primResult :: Translation.PassResBy Final
        primResult = Translation.passResBy origCFun.res.c

        primParams :: [Translation.PassArgBy Final]
        primParams = map Translation.passArgBy origCFun.args

        foreignImport :: [Hs.Decl l]
        foreignImport =
            HsFI.foreignImportDec
              env.sizeofs
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
                  (T.unpack origCName)
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
                      typ     = Translation.toPrimitiveType Translation.FunArg arg
                    , comment = Nothing
                    }
                }
            | arg <- primParams
            ] ++ [
              Hs.ForeignImport.FunParam {
                  hsParam =
                    Hs.FunctionParameter{
                      typ     = Translation.toPrimitiveType Translation.FunArg primResult
                    , comment = Nothing
                    }
                }
            | case primResult of
                -- A type that is not supported by the Haskell FFI as a function result.
                -- We pass it as a function parameter instead.
                Translation.PassByAddress {} -> True
                -- A "normal" result type that is supported by the Haskell FFI.
                Translation.PassByValue {} -> False
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
            Translation.PassByAddress {} -> mkFunRes (Translation.PassByValue C.TypeVoid)
            -- A "normal" result type that is supported by the Haskell FFI.
            Translation.PassByValue {} -> mkFunRes primResult
          where
            mkFunRes :: Translation.PassResBy Final -> Hs.ForeignImport.FunRes
            mkFunRes passBy = Hs.ForeignImport.FunRes {
                  hsType   = mbHsIO $ Translation.toPrimitiveType Translation.FunRes passBy
                }

        restoreOrigSignature :: Hs.Decl l
        restoreOrigSignature =
            getRestoreOrigSignatureDecl
              origHsName
              (Hs.InternalName cWrapperName)
              primResult
              primParams
              (mbHsIO $ Translation.toOrigType Translation.FunRes primResult)
              restoreOrigSignatureParams
              origCFun
              (mbRestoreOrigSignatureComment <> mbIoComment)

        mbRestoreOrigSignatureComment :: Maybe HsDoc.Comment
        restoreOrigSignatureParams :: [Hs.FunctionParameter]
        (mbRestoreOrigSignatureComment, restoreOrigSignatureParams) =
          let params :: [(Maybe Text, Hs.FunctionParameter)]
              params = [ ( fmap (.cName.text) arg.name
                        , Hs.FunctionParameter{
                            typ     = Translation.toOrigType Translation.FunArg (Translation.passArgBy arg)
                          , comment = Nothing
                          })
                        | arg <- origCFun.args
                        ]
          in  mkHaddocksDecorateParams env.haddockConfig info params

        runsInIO :: Bool
        runsInIO = functionShouldRunInIO origCFun.attrs.purity primResult primParams

        mbHsIO :: Hs.Type -> Hs.Type
        mbHsIO
          | runsInIO  = Hs.IO
          | otherwise = id

        mbIoComment :: Maybe HsDoc.Comment
        mbIoComment = ioComment origCFun.attrs.purity

getMainHashIncludeArg :: C.DeclInfo Final -> C.HashIncludeArg
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
     C.FunctionPurity              -- ^ C function purity (function attribute)
  -> Translation.PassResBy Final   -- ^ C result type
  -> [Translation.PassArgBy Final] -- ^ C parameter types
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

-- | Check whether a type needs to be restored
--
-- If a type is passed by address through the Haskell FFI when the C function
-- takes it by value, then we restore the original type in the Haskell binding
-- by peeking/poking the value from/to the address. This requires performing
-- 'IO'.
--
requiresRestore :: Translation.PassBy byValue byAddress -> Bool
requiresRestore = Translation.isPassByAddress

{-------------------------------------------------------------------------------
  Userland-API C wrapper
-------------------------------------------------------------------------------}

-- | Userland-API C wrapper
getCWrapperDecl ::
     String                        -- ^ original C name
  -> String                        -- ^ C wrapper name
  -> Translation.PassResBy Final   -- ^ C result type
  -> [Translation.PassArgBy Final] -- ^ C types of function parameters
  -> PC.FunDefn
getCWrapperDecl origName wrapperName res args
    | C.isVoid resWrapper = PC.withArgs args $ \args' ->
        let expr = PC.ExpressionStatement $
              PC.Call origName (callArgs args' (PC.argsToIdx args'))
        in  PC.FunDefn
              wrapperName
              C.TypeVoid
              C.ImpureFunction
              (Translation.toWrapperType <$> args')
              (PC.CSList $ PC.CSStatement expr PC.CSNil)
    | Translation.isPassByAddress res = PC.withArgs args $ \args' ->
        let expr = PC.ExpressionStatement $
              PC.Assign
                (PC.LDeRef (PC.LVar IZ))
                (PC.Call origName (callArgs args' (IS <$> PC.argsToIdx args')))
        in  PC.FunDefn
              wrapperName
              C.TypeVoid
              C.ImpureFunction
              (fmap Translation.toWrapperType args' :> Translation.toWrapperType res)
              (PC.CSList $ PC.CSStatement expr PC.CSNil)
    | otherwise = PC.withArgs args $ \args' ->
        let expr = PC.ExpressionStatement $
              PC.Return (PC.Call origName (callArgs args' (PC.argsToIdx args')))
        in  PC.FunDefn
              wrapperName
              resWrapper
              C.ImpureFunction
              (Translation.toWrapperType <$> args')
              (PC.CSList $ PC.CSStatement expr PC.CSNil)

  where
    resWrapper :: C.Type Final
    resWrapper = Translation.toWrapperType res

    callArgs ::
         Env ctx' (Translation.PassArgBy Final)
      -> Env ctx' (Idx ctx)
      -> [PC.Expr ctx]
    callArgs tys ids = toList (zipWithEnv f tys ids)
      where
        f ty idx
          | Translation.isPassByAddress ty = PC.DeRef (PC.Var idx)
          | otherwise                      = PC.Var idx

{-------------------------------------------------------------------------------
  RestoreOrigSignature
-------------------------------------------------------------------------------}

-- | Generate a function declaration restoring the signature of the original C
-- function.
getRestoreOrigSignatureDecl ::
     Hs.TermName                   -- ^ name of new function
  -> Hs.TermName                   -- ^ name of foreign import
  -> Translation.PassResBy Final   -- ^ C result type
  -> [Translation.PassArgBy Final] -- ^ C types of function parameters
  -> Hs.Type                       -- ^ Haskell result type
  -> [Hs.FunctionParameter]        -- ^ Haskell function parameters
  -> C.Function Final              -- ^ original C function
  -> Maybe HsDoc.Comment           -- ^ function comment
  -> Hs.Decl l
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

        mkFunArg ::
             Maybe Text
          -> Translation.PassArgBy Final
          -> Hs.FunctionParameter
          -> FunArg
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
             Env ctx' VarInfo
          -> Env ctx' (Idx ctx)
          -> [(Var ctx)]
          -> SHs.SExpr ctx
        go EmptyEnv EmptyEnv zs = kont (reverse zs) -- reverse again!
        go (xs :> x) (ys :> y) zs = case x.typ of
            Translation.PassByAddress ty -> SHs.eAppMany (SHs.eBindgenGlobal Capi_with) $
              let wrapPtrConst = C.isErasedTypeConstQualified ty in
              [ SHs.EBound y
              , SHs.ELam x.nameHint $
                  go xs (IS <$> ys) (Var IZ wrapPtrConst : fmap succVar zs)
              ]

            Translation.PassByValue{} -> go xs ys (Var y False : zs)

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
    callForeignImport args = SHs.eAppMany (SHs.EFree loName) (map exprVar args)

{-------------------------------------------------------------------------------
  Function arguments
-------------------------------------------------------------------------------}

-- | Information about a function argument
data FunArg = FunArg {
    typ        :: Translation.PassArgBy Final
  , cParamName :: Maybe Text
  , funParam   :: Hs.FunctionParameter
  }

funArgToVarInfo :: FunArg -> VarInfo
funArgToVarInfo arg = VarInfo {
      typ = arg.typ
    , nameHint = toHint $ case arg.cParamName of
        Nothing -> fallback
        Just x  -> mangleCandidateDefaultFallback fallback x
    }
  where
    fallback :: Hs.Name Hs.NsVar
    fallback = Hs.UnsafeName $ case arg.typ of
      Translation.PassByValue{}   -> "x"
      Translation.PassByAddress{} -> "ptr"

    toHint :: Hs.Name Hs.NsVar -> NameHint
    toHint n = NameHint $ T.unpack n.text

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
  Variable info
-------------------------------------------------------------------------------}

-- | Information for variables corresponding to function arguments
data VarInfo = VarInfo {
    typ      :: Translation.PassArgBy Final
  , nameHint :: NameHint
  }

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
