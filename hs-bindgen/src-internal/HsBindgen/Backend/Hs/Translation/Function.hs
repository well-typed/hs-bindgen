module HsBindgen.Backend.Hs.Translation.Function (
    functionDecs
  , getMainHashIncludeArg
  ) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as T

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Config
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Haddock.Translation
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Config
import HsBindgen.Backend.Hs.Translation.ForeignImport qualified as HsFI
import HsBindgen.Backend.Hs.Translation.Type qualified as Type
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Config.Prelims
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.PrettyC qualified as PC

import DeBruijn (Env (..), Idx (..), sizeEnv, tabulateEnv, zipWithEnv)

-- | Bind to a C function
--
-- We seek to always generate and expose a Haskell function with the same name
-- as the C function, and with the same signature. However, we can not always
-- directly call the original C function from Haskell. Only primitive types
-- (e.g, pointers, integers, characters) can be passed directly between Haskell
-- and C.
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
-- - A C wrapper (beware, we use the word `wrapper` only in this case); the C
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
-- - An alias to the foreign import of the C wrapper. If the original C function
--   has a primitive signature, we internally refer to this alias `aliasOrig`
--   and assign it the same name as the original C function (modulo name
--   mangling). Otherwise, the refer to the alias as `aliasCWrapper`, and give
--   it a `_wrapper` suffix.
--
--   TODO https://github.com/well-typed/hs-bindgen/issues/1401: This alias may
--   later be moved into a separate module so we avoid the suffix altogether.
--
-- - If the original C function has a non-primitive signature, we generate a
--   function declaration which we refer to as `restoreOrigSignature`, and which
--   has the same signature and name (module name mangling) as the original C
--   function.
functionDecs ::
     HasCallStack
  => SHs.Safety
  -> TranslationConfig
  -> HaddockConfig
  -> BaseModuleName
  -> C.DeclInfo Final
  -> C.Function Final
  -> PrescriptiveDeclSpec
  -> [Hs.Decl]
functionDecs safety opts haddockConfig moduleName info origCFun _spec =
    foreignImport :
      if hasPrimitiveSignature then
        [ aliasOrig ]
      else
        [ aliasCWrapper, restoreOrigSignature ]
  where
    origName :: Text
    origName = info.declId.cName.name.text

    mangledOrigId :: Hs.Identifier
    mangledOrigId = info.declId.hsName

    mangledOrigName :: Hs.Name Hs.NsVar
    mangledOrigName = Hs.unsafeHsIdHsName mangledOrigId

    -- TODO: Should the name mangler take care of the "_wrapper" suffix?
    mangledOrigNameWrapper :: Hs.Name Hs.NsVar
    mangledOrigNameWrapper = Hs.unsafeHsIdHsName $ mangledOrigId <> "_wrapper"

    cWrapperName :: UniqueSymbol
    cWrapperName =
        globallyUnique opts.translationUniqueId moduleName $
          concat [
              show safety
            , "_"
            , T.unpack origName
            ]

    -- TODO https://github.com/well-typed/hs-bindgen/issues/569.
    hasPrimitiveSignature :: Bool
    hasPrimitiveSignature = all isPrimitive (primResult : primParams)

    primResult :: IsPrimitiveType
    primResult = toIsPrimitiveType $ C.functionRes origCFun

    primParams :: [IsPrimitiveType]
    primParams = map (toIsPrimitiveType . snd) origCFun.functionArgs

    foreignImport :: Hs.Decl
    foreignImport =
        HsFI.foreignImportDec
          (Hs.InternalName cWrapperName)
          resultType
          foreignImportParams
          (uniqueCDeclName cWrapperName)
          (CallConvUserlandCAPI cWrapper)
          (Origin.Function origCFun)
          safety
      where
        cWrapperDecl :: PC.Decl
        cWrapperDecl =
            getCWrapperDecl
              (T.unpack origName)
              cWrapperName.unique
              primResult
              primParams

        cWrapper :: CWrapper
        cWrapper = CWrapper {
              cWrapperDefinition = PC.prettyDecl cWrapperDecl ""
            , cWrapperImport = getMainHashIncludeArg info
            }

    foreignImportParams :: [Hs.FunctionParameter]
    foreignImportParams = [
           Hs.FunctionParameter
           { functionParameterName    = fmap (Hs.unsafeHsIdHsName . (.hsName)) mbName
           , functionParameterType    = Type.inContext Type.FunArg (toPrimitiveType (toIsPrimitiveType ty))
           , functionParameterComment = Nothing
           }
        | (mbName, ty) <- C.functionArgs origCFun
        ] ++ toList mbResultParam

    -- Alias to the C wrapper. This function _does not have_ the same signature
    -- as the original C function.
    aliasCWrapper :: Hs.Decl
    aliasCWrapper =
        Hs.DeclFunction $ Hs.FunctionDecl {
            name       = mangledOrigNameWrapper
          , parameters = aliasParams
          , resultType = resultType
          , body       = SHs.EFree $ Hs.InternalName cWrapperName
          , origin     = Origin.Function origCFun
          , pragmas    = []
          , comment    = (Just pointerComment <> mbIoComment)
          }
      where
        pointerComment :: HsDoc.Comment
        pointerComment = HsDoc.title [
              HsDoc.TextContent "Pointer-based API for"
            , HsDoc.Identifier mangledOrigId.text
            ]

    -- Alias to the original C function. This function _does have_ the same
    -- signature as the original C function.
    aliasOrig :: Hs.Decl
    aliasOrig =
        Hs.DeclFunction $ Hs.FunctionDecl {
             name       = mangledOrigName
           , parameters = aliasParams
           , resultType = resultType
           , body       = SHs.EFree $ Hs.InternalName cWrapperName
           , origin     = Origin.Function origCFun
           , pragmas    = []
           , comment    = mbAliasComment <> mbIoComment
          }

    mbAliasComment :: Maybe HsDoc.Comment
    -- These are the same parameters as 'foreignImportParams', enriched with
    -- documentation.
    aliasParams :: [Hs.FunctionParameter]
    (mbAliasComment, aliasParams) =
      mkHaddocksDecorateParams haddockConfig info mangledOrigName foreignImportParams

    restoreOrigSignature :: Hs.Decl
    restoreOrigSignature =
        getRestoreOrigSignatureDecl
          mangledOrigName
          (Hs.InternalName cWrapperName)
          primResult
          primParams
          restoreOrigSignatureParams
          origCFun
          mbRestoreOrigSignatureComment


    mbRestoreOrigSignatureComment :: Maybe HsDoc.Comment
    restoreOrigSignatureParams :: [Hs.FunctionParameter]
    (mbRestoreOrigSignatureComment, restoreOrigSignatureParams) =
      let params :: [Hs.FunctionParameter]
          params = [
               Hs.FunctionParameter
               { functionParameterName    = fmap (Hs.unsafeHsIdHsName . (.hsName)) mbName
               , functionParameterType    = Type.inContext Type.FunArg (toOrigType (toIsPrimitiveType ty))
               , functionParameterComment = Nothing
               }
            | (mbName, ty) <- C.functionArgs origCFun
            ]
      in  mkHaddocksDecorateParams haddockConfig info mangledOrigName params

    -- When translating a 'C.Type' there are C types which we cannot pass
    -- directly using C FFI. We need to distinguish these.
    --
    -- Result types can be heap types, which are types we can't return by value
    -- due to Haskell FFI limitation. Or they can be normal types supported by
    -- Haskell FFI. This is also true for function parameters as well, result types
    -- are a special case where unsupported result types become parameters.
    mbResultParam :: Maybe Hs.FunctionParameter
    resultType    :: Hs.HsType
    (mbResultParam, resultType) = case primResult of
        -- A heap type that is not supported by the Haskell FFI as a function
        -- result. We pass it as a function parameter instead.
        HeapType {} -> (Just Hs.FunctionParameter {
            functionParameterName = Nothing
          , functionParameterType = Type.inContext Type.FunArg $ toPrimitiveType primResult
          , functionParameterComment = Nothing
          }
          , mbIO $ HsPrimType HsPrimUnit
          )

        -- A "normal" result type that is supported by the Haskell FFI.
        PrimitiveType {} ->
          ( Nothing
          , mbIO $ Type.inContext Type.FunRes $ toPrimitiveType primResult
          )

        CAType {} ->
            panicPure "ConstantArray cannot occur as a result type"

        AType {} ->
            panicPure "Array cannot occur as a result type"

    -- Decide based on the function attributes whether to include 'IO' in the
    -- result type of the foreign import alias. See the documentation on
    -- 'C.FunctionPurity'.
    --
    -- An exception to the rules: the foreign import function returns @void@
    -- when @res@ is a heap type, in which case a @const@ or @pure@ attribute
    -- does not make much sense, and so we just return the result in 'IO'.
    mbIO :: Hs.HsType -> Hs.HsType
    mbIoComment :: Maybe HsDoc.Comment
    (mbIO, mbIoComment) = case C.functionPurity (C.functionAttrs origCFun) of
        C.HaskellPureFunction -> (id  , Nothing)
        C.CPureFunction       -> (HsIO, Just pureComment)
        C.ImpureFunction      -> (HsIO, Nothing)
      where
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

getMainHashIncludeArg :: HasCallStack => C.DeclInfo Final -> HashIncludeArg
getMainHashIncludeArg declInfo = case C.declHeaderInfo declInfo of
    Nothing -> panicPure "no main header for builtin"
    Just C.HeaderInfo{headerMainHeaders} -> NonEmpty.head headerMainHeaders

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

data IsPrimitiveType =
      -- | Ordinary, "primitive" types which can be handled by Haskell FFI
      --   directly
      PrimitiveType (C.Type Final)
      -- | Types passed on heap
    | HeapType (C.Type Final)
      -- | An array of known size, with const-qualified array elements
      --
      -- Only if the array elements are const qualified do we know for sure that
      -- the array is read-only. In such cases, we generate a high-level
      -- wrapper.
    | CAType (C.Type Final) Natural (C.Type Final)
      -- | An array of unknown size, with const-qualified array elements
      --
      -- Only if the array elements are const qualified do we know for sure that
      -- the array is read-only. In such cases, we generate a high-level
      -- wrapper.
    | AType (C.Type Final) (C.Type Final)
  deriving Show

-- | Heap types and constant arrays are non-primitive types. We have to treat
--   functions handling non-primitive types in a special way.
isPrimitive :: IsPrimitiveType -> Bool
isPrimitive = \case
    PrimitiveType{} -> True
    HeapType{}     -> False
    CAType{}       -> False
    AType{}        -> False

-- | Types that we cannot directly pass via C FFI
toIsPrimitiveType ::  C.Type Final -> IsPrimitiveType
toIsPrimitiveType ty
  -- Heap types
  | C.isCanonicalTypeStruct ty ||
    C.isCanonicalTypeUnion ty ||
    C.isCanonicalTypeComplex ty
  = HeapType ty

  -- Array types
  | Just aTy <- C.isCanonicalTypeArray ty
  = if C.isErasedTypeConstQualified ty then
      case aTy of
        C.ConstantArrayClassification n eTy -> CAType ty n eTy
        C.IncompleteArrayClassification eTy -> AType ty eTy
    else
      PrimitiveType $ C.TypePointers 1 (C.getArrayElementType aTy)

  -- Other types
  | otherwise
  = PrimitiveType ty

-- | Recover type used in foreign import or its aliases
toPrimitiveType :: IsPrimitiveType -> C.Type Final
toPrimitiveType = \case
    PrimitiveType ty -> ty
    HeapType ty -> C.TypePointers 1 ty
    CAType aTy _ eTy -> firstElemPtr aTy eTy
    AType aTy eTy -> firstElemPtr aTy eTy
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
      = C.TypePointers 1 $ C.TypeQualified C.TypeQualifierConst eTy
      -- No const qualifiers on either the array type or the array element type.
      | otherwise
      = C.TypePointers 1 eTy


-- | Recover type used in `restoreOrigSignature`
toOrigType :: IsPrimitiveType -> C.Type Final
toOrigType (PrimitiveType ty) = ty
toOrigType (HeapType ty)     = ty
toOrigType (CAType oty _ _)  = oty
toOrigType (AType oty _)     = oty

-- | Userland-API C wrapper
getCWrapperDecl
    :: String       -- ^ original C name
    -> String       -- ^ C wrapper name
    -> IsPrimitiveType    -- ^ result type
    -> [IsPrimitiveType]  -- ^ parameters
    -> PC.Decl
getCWrapperDecl origName wrapperName res args
    | isVoidType res
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName C.TypeVoid C.ImpureFunction (toPrimitiveType <$> args')
          [PC.Expr $ PC.Call origName (callArgs args' (PC.argsToIdx args'))]

    | isHeapType res
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName C.TypeVoid C.ImpureFunction (toPrimitiveType <$> (args' :> res))
          [PC.Assign (PC.LDeRef (PC.LVar IZ)) $ PC.Call origName (callArgs args' (IS <$> PC.argsToIdx args'))]

    | otherwise
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName (toPrimitiveType res) C.ImpureFunction (toPrimitiveType <$> args')
          [PC.Return $ PC.Call origName (callArgs args' (PC.argsToIdx args'))]
  where
    callArgs :: Env ctx' IsPrimitiveType -> Env ctx' (Idx ctx) -> [PC.Expr ctx]
    callArgs tys ids = toList (zipWithEnv f tys ids)
      where f ty idx = if isHeapType ty then PC.DeRef (PC.Var idx) else PC.Var idx

    isHeapType :: IsPrimitiveType -> Bool
    isHeapType PrimitiveType {} = False
    isHeapType HeapType {} = True
    isHeapType CAType {}   = False
    isHeapType AType {}    = False

    isVoidType :: IsPrimitiveType -> Bool
    isVoidType = C.isVoid . toPrimitiveType

-- | Generate a function declaration restoring the signature of the original C
--   function
getRestoreOrigSignatureDecl ::
     Hs.Name Hs.NsVar       -- ^ name of new function
  -> Hs.Name Hs.NsVar       -- ^ name of foreign import
  -> IsPrimitiveType              -- ^ result type
  -> [IsPrimitiveType]            -- ^ types of function parameters
  -> [Hs.FunctionParameter] -- ^ function parameter with comments
  -> C.Function Final       -- ^ original C function
  -> Maybe HsDoc.Comment    -- ^ function comment
  -> Hs.Decl
getRestoreOrigSignatureDecl hiName loName primResult primParams params cFunc mbComment =
    let resType :: HsType
        resType = Type.inContext Type.FunRes $ toOrigType primResult
    in  case primResult of
      HeapType {} ->
        Hs.DeclFunction $ Hs.FunctionDecl
          { name       = hiName
          , parameters = params
          , resultType = HsIO resType
          , body       = goA EmptyEnv primParams
          , origin     = Origin.Function cFunc
          , pragmas    = []
          , comment    = mbComment
          }

      PrimitiveType {} ->
        Hs.DeclFunction $ Hs.FunctionDecl
          { name       = hiName
          , parameters = params
          , resultType = HsIO resType
          , body       = goB EmptyEnv primParams
          , origin     = Origin.Function cFunc
          , pragmas    = []
          , comment    = mbComment
          }

      CAType {} ->
        panicPure "ConstantArray cannot occur as a result type"

      AType {} ->
        panicPure "Array cannot occur as a result type"
  where
    -- Wrapper for non-primitive result
    goA :: Env ctx IsPrimitiveType -> [IsPrimitiveType] -> SHs.SExpr ctx
    goA env []     = goA' env (tabulateEnv (sizeEnv env) id) []
    goA env (x:xs) = SHs.ELam "x" $ goA (env :> x) xs

    goA' :: Env ctx' IsPrimitiveType -> Env ctx' (Idx ctx) -> [(Bool, Idx ctx)] -> SHs.SExpr ctx
    goA' EmptyEnv    EmptyEnv  zs
        = shsApps (SHs.EGlobal SHs.CAPI_allocaAndPeek)
          [ SHs.ELam "z" $ shsApps (SHs.EFree loName)
              (map
                (\(useConstPtr, x) -> constPtr useConstPtr $ SHs.EBound x)
                (fmap (second IS) zs ++ [(False, IZ)]))
          ]
      where
        constPtr :: Bool -> SHs.SExpr ctx -> SHs.SExpr ctx
        constPtr useConstPtr
          | useConstPtr = SHs.EApp (SHs.EGlobal SHs.ConstPtr_constructor)
          | otherwise = id

    goA' (tys :> ty) (xs :> x) zs = case ty of
        HeapType ty' -> shsApps (SHs.EGlobal SHs.CAPI_with) $
            let useConstPtr = C.isErasedTypeConstQualified ty' in
            [ SHs.EBound x
            , SHs.ELam "y" $ goA' tys (IS <$> xs) ((useConstPtr, IZ) : fmap (second IS) zs)
            ]

        CAType aTy _ _ -> shsApps (SHs.EGlobal SHs.ConstantArray_withPtr) $
            let useConstPtr = C.isErasedTypeConstQualified aTy in
            [ SHs.EBound x
            , SHs.ELam "ptr" $ goA' tys (IS <$> xs) ((useConstPtr, IZ) : fmap (second IS) zs)
            ]

        AType aTy _ -> shsApps (SHs.EGlobal SHs.IncompleteArray_withPtr) $
            let useConstPtr = C.isErasedTypeConstQualified aTy in
            [ SHs.EBound x
            , SHs.ELam "ptr" $ goA' tys (IS <$> xs) ((useConstPtr, IZ) : fmap (second IS) zs)
            ]

        PrimitiveType{} ->
            goA' tys xs ((False, x) : zs)

    -- Wrapper for primitive result
    goB :: Env ctx IsPrimitiveType -> [IsPrimitiveType] -> SHs.SExpr ctx
    goB env []     = goB' env (tabulateEnv (sizeEnv env) id) []
    goB env (x:xs) = SHs.ELam "x" $ goB (env :> x) xs

    goB' :: Env ctx' IsPrimitiveType -> Env ctx' (Idx ctx) -> [(Bool, Idx ctx)] -> SHs.SExpr ctx
    goB' EmptyEnv    EmptyEnv  zs
        = shsApps (SHs.EFree loName)
            (map
              (\(useConstPtr, x) -> constPtr useConstPtr $ SHs.EBound x)
              zs)
      where
        constPtr :: Bool -> SHs.SExpr ctx -> SHs.SExpr ctx
        constPtr useConstPtr
          | useConstPtr = SHs.EApp (SHs.EGlobal SHs.ConstPtr_constructor)
          | otherwise = id

    goB' (tys :> ty) (xs :> x) zs = case ty of
        HeapType ty' -> shsApps (SHs.EGlobal SHs.CAPI_with) $
          let useConstPtr = C.isErasedTypeConstQualified ty' in
          [ SHs.EBound x
          , SHs.ELam "y" $ goB' tys (IS <$> xs) ((useConstPtr, IZ) : fmap (second IS) zs)
          ]

        CAType aTy _ _ -> shsApps (SHs.EGlobal SHs.ConstantArray_withPtr) $
            let useConstPtr = C.isErasedTypeConstQualified aTy in
            [ SHs.EBound x
            , SHs.ELam "ptr" $ goB' tys (IS <$> xs) ((useConstPtr, IZ) : fmap (second IS) zs)
            ]

        AType aTy _ -> shsApps (SHs.EGlobal SHs.IncompleteArray_withPtr) $
            let useConstPtr = C.isErasedTypeConstQualified aTy in
            [ SHs.EBound x
            , SHs.ELam "ptr" $ goB' tys (IS <$> xs) ((useConstPtr, IZ) : fmap (second IS) zs)
            ]

        PrimitiveType {} ->
            goB' tys xs ((False, x) : zs)

shsApps :: SHs.SExpr ctx -> [SHs.SExpr ctx] -> SHs.SExpr ctx
shsApps = foldl' SHs.EApp
