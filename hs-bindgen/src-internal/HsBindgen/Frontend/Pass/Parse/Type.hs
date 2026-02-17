-- | Fold types
module HsBindgen.Frontend.Pass.Parse.Type (fromCXType) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Error.Class
import Data.Data (Typeable)
import GHC.Stack

import Clang.Enum.Simple
import Clang.LowLevel.Core

import HsBindgen.Errors
import HsBindgen.Frontend.AST.Decl qualified as C ()
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (PrelimDeclId)
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId qualified as PrelimDeclId
import HsBindgen.Frontend.Pass.Parse.Type.Monad (ParseType)
import HsBindgen.Frontend.Pass.Parse.Type.Monad qualified as ParseType
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

fromCXType :: forall ctx m.
  (MonadIO m, Show ctx, Typeable ctx, HasCallStack)
  => ctx -> CXType -> m (C.Type Parse)
fromCXType context =
    liftIO . handle addContextHandler . ParseType.run . cxtype
  where
    addContextHandler :: SomeException -> IO a
    addContextHandler e
      | Just e' <- (fromException @ParseTypeException e) =
          throwIO (ParseType.ParseTypeExceptionInContext context e')
      | otherwise = throwIO e

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
      CXType_Double     -> prim $ C.PrimFloating C.PrimDouble
      CXType_LongDouble -> failure UnsupportedLongDouble
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

      kind -> failure $ UnexpectedTypeKind (Right kind)

    addQualifiers <- qualifiers ty
    pure $ addQualifiers reifiedType
  where
    failure :: ParseTypeException -> CXType -> ParseType (C.Type Parse)
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
    _            -> throwError $ UnexpectedComplexType complexType

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
        throwError $ UnsupportedBuiltin builtin
      Nothing -> ParseType.dispatchDecl decl $ \case
        CXCursor_EnumDecl   -> typeEnum decl
        CXCursor_StructDecl -> typeRef decl C.TagKindStruct
        CXCursor_UnionDecl  -> typeRef decl C.TagKindUnion

        CXCursor_TypedefDecl -> typeTypedef decl

        kind -> throwError $ UnexpectedTypeDecl (Right kind)

typeRef :: MonadIO m => CXCursor -> C.TagKind -> m (C.Type Parse)
typeRef decl kind =
    C.TypeRef <$> PrelimDeclId.atCursor decl (C.NameKindTagged kind)

typeEnum :: HasCallStack => CXCursor -> ParseType (C.Type Parse)
typeEnum decl = do
    declId <- PrelimDeclId.atCursor decl (C.NameKindTagged C.TagKindEnum)
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
typeTypedef decl = do
    declId <- PrelimDeclId.atCursor decl C.NameKindOrdinary
    -- Typedefs can not be anonymous, but we use 'cachedMaybe' for safety
    -- anyway
    let mDeclName = PrelimDeclId.sourceName declId
    ParseType.cachedMaybe mDeclName $ do
        underlying <- handle (addUnderlyingTypeContextHandler declId) $
                        getUnderlyingType decl
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
        Right{}                 -> cxtype uTy
        _otherwise              -> panicPure "Invalid underlying type"

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
      throwError UnsupportedVariadicFunction
    else do
      res   <- clang_getResultType ty >>= cxtype
      nargs <- clang_getNumArgTypes ty
      args  <- forM [0 .. nargs - 1] $ \i ->
                 clang_getArgType ty (fromIntegral i) >>= cxtype
      pure $ C.TypeFun args res

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
  | Just e' <- (fromException @ParseTypeException e)
  = throwM (UnsupportedUnderlyingType n e')
  | otherwise  = throwM e
