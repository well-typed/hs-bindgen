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

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Config
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Haddock.Translation
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Config
import HsBindgen.Backend.Hs.Translation.ForeignImport qualified as Hs.ForeignImport
import HsBindgen.Backend.Hs.Translation.ForeignImport qualified as HsFI
import HsBindgen.Backend.Hs.Translation.State (TranslationState)
import HsBindgen.Backend.Hs.Translation.Type qualified as Type
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.SHs.Translation.Common qualified as SHs
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Config.Prelims
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming
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
  -> TranslationConfig
  -> HaddockConfig
  -> BaseModuleName
  -> TranslationState
  -> C.DeclInfo Final
  -> C.Function Final
  -> PrescriptiveDeclSpec
  -> [Hs.Decl]
functionDecs safety opts haddockConfig moduleName transState info origCFun _spec =
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
        globallyUnique opts.uniqueId moduleName $
          concat [
              show safety
            , "_"
            , T.unpack origName
            ]

    primResult :: PassBy
    primResult = classifyArgPassingMethod origCFun.res

    primParams :: [PassBy]
    primParams = map (classifyArgPassingMethod . snd) origCFun.args

    foreignImport :: [Hs.Decl]
    foreignImport =
        HsFI.foreignImportDec
          transState
          (Hs.ForeignImport.FunName cWrapperName)
          foreignImportParams
          foreignImportResult
          (uniqueCDeclName cWrapperName)
          (CallConvUserlandCAPI cWrapper)
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
                    name    = n
                  , typ     = hsType
                  , comment = Nothing
                  }
            }
        | (mbName, ty) <- origCFun.args ++ toList ((Nothing,) <$> foreignImportOptParam)
        , let n = fmap (Hs.unsafeHsIdHsName . (.hsName)) mbName
              hsType = Type.inContext Type.FunArg cType
              cType = toPrimitiveType passMethod
              passMethod = classifyArgPassingMethod ty
        ]

    foreignImportOptParam :: Maybe (C.Type Final)
    foreignImportOptParam = case primResult of
        -- A type that is not supported by the Haskell FFI as a function result.
        -- We pass it as a function parameter instead.
        PassByAddress {} -> Just $ toPrimitiveType primResult
        -- A "normal" result type that is supported by the Haskell FFI.
        PassByValue {} -> Nothing

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
        PassByAddress {} -> mkFunRes C.TypeVoid
        -- A "normal" result type that is supported by the Haskell FFI.
        PassByValue {} -> mkFunRes $ toPrimitiveType primResult
      where
        mkFunRes :: C.Type Final -> Hs.ForeignImport.FunRes
        mkFunRes cType = Hs.ForeignImport.FunRes {
              hsType   = mbHsIO $ Type.inContext Type.FunRes $ cType
            }

    restoreOrigSignature :: Hs.Decl
    restoreOrigSignature =
        getRestoreOrigSignatureDecl
          mangledOrigName
          (Hs.InternalName cWrapperName)
          primResult
          primParams
          (mbHsIO $ Type.inContext Type.FunRes $ toOrigType primResult)
          restoreOrigSignatureParams
          origCFun
          (mbRestoreOrigSignatureComment <> mbIoComment)

    mbRestoreOrigSignatureComment :: Maybe HsDoc.Comment
    restoreOrigSignatureParams :: [Hs.FunctionParameter]
    (mbRestoreOrigSignatureComment, restoreOrigSignatureParams) =
      let params :: [Hs.FunctionParameter]
          params = [
               Hs.FunctionParameter{
                 name    = fmap (Hs.unsafeHsIdHsName . (.hsName)) mbName
               , typ     = Type.inContext Type.FunArg (toOrigType (classifyArgPassingMethod ty))
               , comment = Nothing
               }
            | (mbName, ty) <- origCFun.args
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
-- But, if any of the function arguments are passed by address, then the
-- function has to run in 'IO' regardless of the function purity, because
-- pointer manipulation requires 'IO'.
--
functionShouldRunInIO ::
     C.FunctionPurity -- ^ C function purity (function attribute)
  -> PassBy           -- ^ C result type
  -> [PassBy]         -- ^ C parameter types
  -> Bool
functionShouldRunInIO purity resType argTypes
  | all isPassByValue (resType : argTypes)
  = case purity of
      C.HaskellPureFunction -> False
      C.CPureFunction       -> True
      C.ImpureFunction      -> True
  | otherwise
  = True

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

-- | Classification of the type of a function argument\/result: it is either
-- passed by value or by address.
data PassBy =
      -- | Types passed by value.
      --
      -- Ordinary, "primitive" types which can be handled by Haskell FFI
      -- directly.
      PassByValue (C.Type Final)
      -- | Types passed by address
      --
      -- Structs and unions have to be passed by address, because they can not
      -- be handled by the Haskell FFI directly.
    | PassByAddress (C.Type Final)
  deriving Show

isPassByValue :: PassBy -> Bool
isPassByValue = \case
    PassByValue{}     -> True
    PassByAddress{} -> False

isPassByAddress :: PassBy -> Bool
isPassByAddress = \case
    PassByValue {} -> False
    PassByAddress {} -> True

isVoidType :: PassBy -> Bool
isVoidType = C.isVoid . toPrimitiveType

-- | Classify how a function argument\/result is passed from Haskell to C
classifyArgPassingMethod :: C.Type Final -> PassBy
classifyArgPassingMethod ty
  -- Heap types
  | C.isCanonicalTypeStruct ty ||
    C.isCanonicalTypeUnion ty ||
    C.isCanonicalTypeComplex ty
  = PassByAddress ty

  -- Array types
  | Just aTy <- C.isCanonicalTypeArray ty
  = PassByValue $ firstElemPtr ty (C.getArrayElementType aTy)

  -- Other types
  | otherwise
  = PassByValue ty
  where
    -- NOTE: if an array type is const-qualified, then its array element type is
    -- also const-qualified, and vice versa.
    firstElemPtr :: C.Type Final -> C.Type Final -> C.Type Final
    firstElemPtr aTy eTy
      -- The array element type has a const qualifier.
      | C.isErasedTypeConstQualified eTy
      = C.TypePointers 1 eTy
      -- The array type has a const qualifier, but the array element type does
      -- not.
      | C.isErasedTypeConstQualified aTy
      = C.TypePointers 1 $ C.TypeQual C.QualConst eTy
      -- No const qualifiers on either the array type or the array element type.
      | otherwise
      = C.TypePointers 1 eTy

-- | Recover type used in the foreign import
toPrimitiveType :: PassBy -> C.Type Final
toPrimitiveType = \case
    PassByValue ty -> ty
    PassByAddress ty -> C.TypePointers 1 ty

-- | Recover type used in "restoreOrigSignature"
toOrigType :: PassBy -> C.Type Final
toOrigType = \case
    PassByValue ty -> ty
    PassByAddress ty -> ty

{-------------------------------------------------------------------------------
  Userland-API C wrapper
-------------------------------------------------------------------------------}

-- | Userland-API C wrapper
getCWrapperDecl ::
     String   -- ^ original C name
  -> String   -- ^ C wrapper name
  -> PassBy   -- ^ C result type
  -> [PassBy] -- ^ C parameter types
  -> PC.FunDefn
getCWrapperDecl origName wrapperName res args
    | isVoidType res
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName C.TypeVoid C.ImpureFunction (toPrimitiveType <$> args') $
        PC.CSList $
          PC.CSStatement
            (PC.ExpressionStatement $ PC.Call origName (callArgs args' (PC.argsToIdx args')))
            PC.CSNil

    | isPassByAddress res
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName C.TypeVoid C.ImpureFunction (toPrimitiveType <$> (args' :> res)) $
        PC.CSList $
          PC.CSStatement
            (PC.ExpressionStatement $ PC.Assign (PC.LDeRef (PC.LVar IZ)) $ PC.Call origName (callArgs args' (IS <$> PC.argsToIdx args')))
            PC.CSNil

    | otherwise
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName (toPrimitiveType res) C.ImpureFunction (toPrimitiveType <$> args') $
        PC.CSList $
          PC.CSStatement
            (PC.ExpressionStatement $ PC.Return $ PC.Call origName (callArgs args' (PC.argsToIdx args')))
            PC.CSNil
  where
    callArgs :: Env ctx' PassBy -> Env ctx' (Idx ctx) -> [PC.Expr ctx]
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
  -> PassBy                 -- ^ C result type
  -> [PassBy]               -- ^ C types of function parameters
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
          if all isPassByValue (primResult : primParams)
          then byValueBodyExpr
          else bodyExpr
      , origin     = Origin.Function cFunc
      , pragmas    = []
      , comment    = mbComment
      }
  where
    -- | The body of the function if no function arguments or results have to be
    -- passed by address.
    --
    -- For example, simply:
    --
    -- > foo
    --
    byValueBodyExpr :: SHs.ClosedExpr
    byValueBodyExpr = SHs.EFree loName

    -- | The body of the function if some function arguments or results have to
    -- be passed by address.
    --
    -- For example:
    --
    -- >  \x0 -> \x1 -> \x2 -> \x3 ->
    -- >    with x0 $ \y4 -> with x2 $ \y5 ->
    -- >      allocaAndPeek $ \z6 ->
    -- >        foo y4 x1 (ConstPtr y5) x3 z6
    --
    bodyExpr :: SHs.ClosedExpr
    bodyExpr =
      -- construct lambdas for all function arguments
      lambdas EmptyEnv (zipWith FunArg primParams hsParams) $ \env ->
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
    -- We do this for all function arguments that are 'PassByAddress', not if they
    -- are 'PassByValue'.
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
            PassByAddress ty' ->  SHs.eAppMany (SHs.EGlobal SHs.CAPI_with) $
              let wrapConstPtr = C.isErasedTypeConstQualified ty' in
              [ SHs.EBound y
              , SHs.ELam x.ptrNameHint $
                  go xs (IS <$> ys) (Var IZ wrapConstPtr : fmap succVar zs)
              ]

            PassByValue{} -> go xs ys (Var y False : zs)

    -- | Construct an expression to pass the function result by address, if
    -- necessary
    --
    -- For example:
    --
    -- > ... allocaAndPeek $ \z6 -> ...
    --
    -- We do this only if the function result is a 'PassByAddress', not if it is
    -- 'PassByValue'.
    --
    passResultByAddressIfNecessary ::
         -- | Context of in-scope variables to be passed to the foreign import
         [(Var ctx)]
         -- | Construct the body of the lambda
      -> (forall ctx'. [Var ctx'] -> SHs.SExpr ctx')
      -> SHs.SExpr ctx
    passResultByAddressIfNecessary zs kont
      | isPassByAddress primResult
      = let zs' = fmap succVar zs ++ [Var IZ False] in
        SHs.EApp
          (SHs.EGlobal SHs.CAPI_allocaAndPeek)
          (SHs.ELam "res" $ kont zs')
      | otherwise
      = kont zs

    -- | Construct a call to the foreign import
    --
    -- For example (@foo@ is the name of the foreign import):
    --
    -- > ... foo y4 x1 (ConstPtr y5) x3 z6
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
    typ :: PassBy
  , funParam :: Hs.FunctionParameter
  }

funArgToVarInfo :: FunArg -> VarInfo
funArgToVarInfo arg = VarInfo {
      typ = arg.typ
    , optNameHint = fmap (NameHint . T.unpack . Hs.getName) arg.funParam.name
    }

{-------------------------------------------------------------------------------
  Variables
-------------------------------------------------------------------------------}

-- | A variable with some additional information
type Var :: Ctx -> Type
data Var ctx = Var {
    -- | The name (i.e., DeBruijn index) of the variable
    name :: Idx ctx
    -- | Whether to wrap the variable in a 'ConstPtr' constructor
    --
    -- 'with' and 'allocaAndPeek' always use 'Ptr' rather than 'ConstPtr', hence
    -- the need to add the 'ConstPtr' constructor manually.
  , wrapConstPtr :: Bool
  }

-- | Shift the variable
succVar :: Var ctx -> Var (S ctx)
succVar var = Var {
      name = IS var.name
    , wrapConstPtr = var.wrapConstPtr
    }

-- | Turn the variable into an expression
--
-- For example:
--
-- > ConstPtr y5
--
exprVar :: Var ctx -> SHs.SExpr ctx
exprVar var
  | var.wrapConstPtr
  = SHs.EApp (SHs.EGlobal SHs.ConstPtr_constructor) (SHs.EBound var.name)
  | otherwise
  = SHs.EBound var.name

{-------------------------------------------------------------------------------
  Variable info
-------------------------------------------------------------------------------}

-- | Information for variables in an environment
data VarInfo = VarInfo {
    typ :: PassBy
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
