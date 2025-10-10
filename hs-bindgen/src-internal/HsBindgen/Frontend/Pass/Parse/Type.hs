-- | Fold types
module HsBindgen.Frontend.Pass.Parse.Type (fromCXType) where

import Control.Exception (Exception (..), SomeException (..), handle)
import Control.Monad
import Control.Monad.Error.Class
import Data.Data (Typeable)
import GHC.Stack

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types (CursorSpelling (..))
import Clang.LowLevel.Core

import HsBindgen.Errors
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Type.Monad
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

fromCXType :: forall ctx m.
  (MonadIO m, Show ctx, Typeable ctx, HasCallStack)
  => ctx -> CXType -> m (C.Type Parse)
fromCXType context = liftIO . handle addContextHandler . run . cxtype
  where
    addContextHandler :: SomeException -> IO a
    addContextHandler e
      | Just e' <- (fromException @ParseTypeException e) =
          throwIO (ParseTypeExceptionInContext context e')
      | otherwise = throwIO e

{-------------------------------------------------------------------------------
  Dispatch
-------------------------------------------------------------------------------}

cxtype :: HasCallStack => CXType -> ParseType (C.Type Parse)
cxtype ty = do
    reifiedType <- dispatchWithArg ty $ \case
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
    isConst <- clang_isConstQualifiedType ty
    pure $ if isConst then C.TypeConst reifiedType else reifiedType
  where
    failure :: ParseTypeException -> CXType -> ParseType (C.Type Parse)
    failure err _ty = throwError err

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
pointer = clang_getPointeeType >=> fmap C.TypePointer . cxtype

fromDecl :: HasCallStack => CXType -> ParseType (C.Type Parse)
fromDecl ty = do
    decl   <- clang_getTypeDeclaration ty
    declId <- C.getPrelimDeclId decl
    dispatchDecl decl $ \case
      CXCursor_EnumDecl    -> return $ C.TypeEnum   declId
      CXCursor_StructDecl  -> return $ C.TypeStruct declId
      CXCursor_UnionDecl   -> return $ C.TypeUnion  declId
      CXCursor_TypedefDecl -> do
        getUnderlyingTypeCursorSpelling decl >>= \case
          ClangBuiltin "__builtin_va_list" | declId == C.PrelimDeclIdNamed "va_list" ->
            throwError UnsupportedVariadicFunction
          _ -> do
            uTy <- cxtype =<< getUnderlyingCXType decl
            n <- typedefName declId
            pure (C.TypeTypedef $ OrigTypedefRef n uTy)
      kind                 -> throwError $ UnexpectedTypeDecl (Right kind)
  where
    typedefName :: C.PrelimDeclId -> ParseType C.Name
    typedefName = \case
      C.PrelimDeclIdNamed name   -> return name
      C.PrelimDeclIdAnon{}       -> panicPure "Unexpected anonymous typedef"
      C.PrelimDeclIdBuiltin name -> throwError $ UnsupportedBuiltin name

    getUnderlyingCXType :: MonadIO m => CXCursor -> m CXType
    getUnderlyingCXType typedefCurr = do
      uTy  <- clang_getTypedefDeclUnderlyingType typedefCurr
      -- Later versions of Clang use elaborated types, earlier versions do not
      case fromSimpleEnum (cxtKind uTy) of
        Right CXType_Elaborated -> clang_Type_getNamedType uTy
        Right{}                 -> return uTy
        _otherwise              -> panicPure "Invalid underlying type"

    getUnderlyingTypeCursorSpelling :: MonadIO m => CXCursor -> m CursorSpelling
    getUnderlyingTypeCursorSpelling typedefCurr = do
      uTy <- getUnderlyingCXType typedefCurr
      uDecl  <- clang_getTypeDeclaration uTy
      HighLevel.clang_getCursorSpelling uDecl

function :: Bool -> CXType -> ParseType (C.Type Parse)
function hasProto ty = do
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
      pure $ C.TypeFun (map adjustFunctionTypesToPointers args)
                       (adjustFunctionTypesToPointers res)

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
  Implicit function to pointer conversion
-------------------------------------------------------------------------------}

-- | Recursively convert each function type to a pointer-to-function type.
--
-- See the "Functions" section of the manual.
adjustFunctionTypesToPointers :: C.Type Parse -> C.Type Parse
adjustFunctionTypesToPointers = go False
  where
    go ctx = \case
      C.TypePrim pt -> C.TypePrim pt
      C.TypeStruct n -> C.TypeStruct n
      C.TypeUnion n -> C.TypeUnion n
      C.TypeEnum n -> C.TypeEnum n
      -- TODO: this should look through typedefs. See issue
      -- #1142 and issue #1033.
      C.TypeTypedef ref ->  C.TypeTypedef ref
      C.TypeMacroTypedef n -> C.TypeMacroTypedef n
      C.TypePointer t -> C.TypePointer $ go True t
      C.TypeFun args res -> do
        let args' = map (go False) args
            res' = go False res
        if ctx then
          C.TypeFun args' res'
        else
          C.TypePointer (C.TypeFun args' res')
      C.TypeVoid -> C.TypeVoid
      C.TypeConstArray n t -> C.TypeConstArray n $ go ctx t
      C.TypeExtBinding eb -> C.TypeExtBinding eb
      C.TypeIncompleteArray t -> C.TypeIncompleteArray $ go ctx t
      -- This is a slightly weird case. From what I understand, blocks are
      -- similar to pointers. So, I'm treating them like pointers.
      C.TypeBlock t -> C.TypeBlock $ go True t
      C.TypeConst t -> C.TypeConst $ go ctx t
      C.TypeComplex pt -> C.TypeComplex pt
