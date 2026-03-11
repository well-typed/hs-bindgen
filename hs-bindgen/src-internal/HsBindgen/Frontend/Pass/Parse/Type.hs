-- | Fold types
module HsBindgen.Frontend.Pass.Parse.Type (fromCXType) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Error.Class
import GHC.Stack

import Clang.Enum.Simple
import Clang.LowLevel.Core

import HsBindgen.Frontend.AST.Decl qualified as C ()
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass (NoAnn (..))
import HsBindgen.Frontend.Pass.Parse.Context
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Monad.Type (ParseType)
import HsBindgen.Frontend.Pass.Parse.Monad.Type qualified as ParseType
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (PrelimDeclId)
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId qualified as PrelimDeclId
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

fromCXType :: forall m.
  (MonadIO m, HasCallStack)
  => ParseCtx -> CXType -> m (C.Type Parse)
fromCXType ctx =
    liftIO . handle h . ParseType.run . cxtype
  where
    h :: SomeException -> IO a
    h = addCtxHandler (Proxy :: Proxy DelayedParseMsg) ctx

{-------------------------------------------------------------------------------
  Dispatch
-------------------------------------------------------------------------------}

cxtype :: HasCallStack => CXType -> ParseType (C.Type Parse)
cxtype ty = do
    reifiedType <- ParseType.dispatchWithArg ty $ \case
      CXType_Char_S     -> prim $ C.PrimChar (C.PrimSignImplicit $ Just C.Signed)
      CXType_Char_U     -> prim $ C.PrimChar (C.PrimSignImplicit $ Just C.Unsigned)
      CXType_SChar      -> prim $ C.PrimChar (C.PrimSignExplicit C.Signed)
      CXType_UChar      -> prim $ C.PrimChar (C.PrimSignExplicit C.Unsigned)
      CXType_Short      -> prim $ C.PrimIntegral C.PrimShort    C.Signed
      CXType_UShort     -> prim $ C.PrimIntegral C.PrimShort    C.Unsigned
      CXType_Int        -> prim $ C.PrimIntegral C.PrimInt      C.Signed
      CXType_UInt       -> prim $ C.PrimIntegral C.PrimInt      C.Unsigned
      CXType_Long       -> prim $ C.PrimIntegral C.PrimLong     C.Signed
      CXType_ULong      -> prim $ C.PrimIntegral C.PrimLong     C.Unsigned
      CXType_LongLong   -> prim $ C.PrimIntegral C.PrimLongLong C.Signed
      CXType_ULongLong  -> prim $ C.PrimIntegral C.PrimLongLong C.Unsigned
      CXType_Float      -> prim $ C.PrimFloating C.PrimFloat
      CXType_Float128   -> failure ParseUnsupportedFloat128
      CXType_Double     -> prim $ C.PrimFloating C.PrimDouble
      CXType_LongDouble -> failure ParseUnsupportedLongDouble
      CXType_Bool       -> prim $ C.PrimBool
      CXType_Complex    -> complex

      CXType_Attributed      -> attributed
      CXType_BlockPointer    -> blockPointer
      CXType_ConstantArray   -> constantArray
      CXType_Elaborated      -> elaborated
      CXType_Enum            -> fromDecl
      CXType_FunctionNoProto -> function False
      CXType_FunctionProto   -> function True
      CXType_IncompleteArray -> incompleteArray
      CXType_Pointer         -> pointer
      CXType_Record          -> fromDecl
      CXType_Typedef         -> fromDecl
      CXType_Void            -> const (pure C.TypeVoid)

      kind -> failure $ ParseUnexpectedTypeKind (Right kind)

    addQualifiers <- qualifiers ty
    pure $ addQualifiers reifiedType
  where
    failure :: DelayedParseMsg -> CXType -> ParseType (C.Type Parse)
    failure err _ty = throwError err

qualifiers :: CXType -> ParseType (C.Type Parse -> C.Type Parse)
qualifiers ty = do
    isConst <- clang_isConstQualifiedType ty
    pure $ if isConst
             then C.TypeQual C.QualConst
             else id

{-------------------------------------------------------------------------------
  Functions for each kind of type
-------------------------------------------------------------------------------}

prim :: C.PrimType -> CXType -> ParseType (C.Type Parse)
prim ty _ = return $ C.TypePrim ty

complex :: CXType -> ParseType (C.Type Parse)
complex ty = do
  complexType <- clang_getElementType ty
  cty         <- cxtype complexType
  case cty of
    C.TypePrim p -> pure (C.TypeComplex p)
    _            -> throwError $ ParseUnexpectedComplexType complexType

elaborated :: CXType -> ParseType (C.Type Parse)
elaborated = clang_Type_getNamedType >=> cxtype

pointer :: CXType -> ParseType (C.Type Parse)
pointer = clang_getPointeeType >=> fmap (C.TypePointers 1) . cxtype

fromDecl :: HasCallStack => CXType -> ParseType (C.Type Parse)
fromDecl ty = do
    decl     <- clang_getTypeDeclaration ty
    mBuiltin <- PrelimDeclId.checkIsBuiltin decl
    case mBuiltin of
      Just builtin ->
        -- Built-in types don't have a corresponding declaration; if we want
        -- to support them, we have to special-case each one. For now, we don't
        -- support any.
        throwError $ ParseUnsupportedBuiltin builtin
      Nothing -> ParseType.dispatchDecl decl $ \case
        CXCursor_EnumDecl   -> typeEnum decl
        CXCursor_StructDecl -> checkNotOutOfScopeDecl decl CTagKindStruct
                            >> typeRef decl CTagKindStruct
        CXCursor_UnionDecl  -> checkNotOutOfScopeDecl decl CTagKindUnion
                            >> typeRef decl CTagKindUnion

        CXCursor_TypedefDecl -> typeTypedef decl

        kind -> throwError $ ParseUnexpectedCursorKind (Right kind)

typeRef :: MonadIO m => CXCursor -> CTagKind -> m (C.Type Parse)
typeRef decl kind =
    C.TypeRef <$> PrelimDeclId.atCursor decl (CNameKindTagged kind)

-- | Check that a struct/union declaration is not out of scope inside a
-- function prototype.
--
-- We detect this by combining two clang queries:
--
-- 1. @clang_getCursorSemanticParent@: for a file-scope @struct foo@, the
--    semantic parent is the translation unit. But when @struct foo@ is out of
--    scope inside a function, clang parents it under the
--    @CXCursor_FunctionDecl@.
--
-- 2. @clang_isCursorDefinition@: distinguishes the out of scope declarations
--    from inline definitions. A struct /defined/ inline in a prototype (e.g.
--    @void f(struct s {int x;} a)@) has @isDef = True@ and a complete type,
--    so we allow it. A bare reference like @void f(struct s *p)@ where
--    @struct s@ has no file-scope declaration has @isDef = False@.
--
-- Examples:
--
-- @
-- struct opaque;
-- void f(struct opaque *p);           -- OK: parent is TranslationUnit
-- void g(struct s { int x; } arg);    -- OK: isDef = True
-- @
--
checkNotOutOfScopeDecl :: CXCursor -> CTagKind -> ParseType ()
checkNotOutOfScopeDecl decl kind = do
    parent     <- clang_getCursorSemanticParent decl
    parentKind <- fromSimpleEnum <$> clang_getCursorKind parent
    isDef      <- clang_isCursorDefinition decl
    when (parentKind == Right CXCursor_FunctionDecl && not isDef) $ do
      name <- clang_getCursorSpelling decl
      throwError $ ParseDeclarationOutOfScope kind name

typeEnum :: HasCallStack => CXCursor -> ParseType (C.Type Parse)
typeEnum decl = do
    declId <- PrelimDeclId.atCursor decl (CNameKindTagged CTagKindEnum)
    -- Enums can be anonymous. In such cases, we bypass the cache and parse the
    -- enum type directly.
    let mDeclName = PrelimDeclId.sourceName declId
    ParseType.cachedMaybe mDeclName $ do
      underlying <- handle (addUnderlyingTypeContextHandler declId)
                      (cxtype =<< clang_getEnumDeclIntegerType decl)
      pure $ C.TypeEnum $ C.Ref {
            name = declId
          , underlying = underlying
          }

typeTypedef :: HasCallStack => CXCursor -> ParseType (C.Type Parse)
typeTypedef curr = do
    declId <- PrelimDeclId.atCursor curr CNameKindOrdinary
    -- Typedefs can not be anonymous, but we use 'cachedMaybe' for safety
    -- anyway
    let mDeclName = PrelimDeclId.sourceName declId
    ParseType.cachedMaybe mDeclName $ do
        underlying <- handle (addUnderlyingTypeContextHandler declId) $
                        getUnderlyingType curr
        pure $ C.TypeTypedef $ C.Ref {
            name = declId
          , underlying = underlying
          }
  where
    getUnderlyingType :: CXCursor -> ParseType (C.Type Parse)
    getUnderlyingType typedefCurr = do
      uTy  <- clang_getTypedefDeclUnderlyingType typedefCurr
      -- Later versions of Clang use elaborated types, earlier versions do not
      case fromSimpleEnum (cxtKind uTy) of
        Right CXType_Elaborated -> do
          -- The named type @refTy@ that is referenced by the underlying type
          -- might not have the same qualifiers as the underlying type. We add
          -- them back in using 'qualifiers'. We did not do this in the past,
          -- leading to bugs, see PR #1488.
          refTy <- clang_Type_getNamedType uTy
          addQualifiers <- qualifiers uTy
          addQualifiers <$> cxtype refTy
        Right{} ->
          cxtype uTy
        typeKind ->
          throwError $ ParseUnexpectedTypeKind typeKind

function :: Bool -> CXType -> ParseType (C.Type Parse)
function hasProto = \ty -> do
    isVariadic <-
      -- Functions without a prototype (that is, without declared arguments)
      -- are technically speaking considered variadic. However, we reinterpret
      -- such declarations following C23, and assume they have /no/ arguments.
      if hasProto
        then clang_isFunctionTypeVariadic ty
        else return False
    if isVariadic then do
      throwError ParseUnsupportedVariadicFunction
    else do
      res   <- clang_getResultType ty >>= cxtype
      nargs <- clang_getNumArgTypes ty
      args  <- forM [0 .. nargs - 1] $ \i ->
                 clang_getArgType ty (fromIntegral i) >>= cxtype
      pure $ C.TypeFun (map mkTypeFunArg args) res
  where
    mkTypeFunArg ty = C.TypeFunArgF {
          typ = ty
        , ann = NoAnn
        }

constantArray :: CXType -> ParseType (C.Type Parse)
constantArray ty = do
    n   <- fromIntegral <$> clang_getArraySize ty
    ty' <- cxtype =<< clang_getArrayElementType ty
    return (C.TypeConstArray n ty')

incompleteArray :: CXType -> ParseType (C.Type Parse)
incompleteArray ty = do
    ty' <- cxtype =<< clang_getArrayElementType ty
    return (C.TypeIncompleteArray ty')

attributed :: CXType -> ParseType (C.Type Parse)
attributed ty = cxtype =<< clang_Type_getModifiedType ty

blockPointer :: CXType -> ParseType (C.Type Parse)
blockPointer ty = do
    fun <- function True =<< clang_getPointeeType ty
    return (C.TypeBlock fun)

{-------------------------------------------------------------------------------
  Underlying types
-------------------------------------------------------------------------------}

addUnderlyingTypeContextHandler :: PrelimDeclId -> SomeException -> ParseType a
addUnderlyingTypeContextHandler n e
  | Just e' <- (fromException @DelayedParseMsg e)
  = throwM (ParseUnderlyingTypeFailed n e')
  | otherwise  = throwM e
