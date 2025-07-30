-- | Fold types
module HsBindgen.Frontend.Pass.Parse.Type (fromCXType) where

import Control.Monad
import Control.Monad.Error.Class
import GHC.Stack

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

fromCXType :: (MonadIO m, HasCallStack) => CXType -> m (C.Type Parse)
fromCXType = run . cxtype

{-------------------------------------------------------------------------------
  Dispatch
-------------------------------------------------------------------------------}

cxtype :: HasCallStack => CXType -> ParseType (C.Type Parse)
cxtype ty =
    dispatchWithArg ty $ \case
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
  where
    failure :: ParseTypeException -> CXType -> ParseType (C.Type Parse)
    failure err _ty = throwError err

{-------------------------------------------------------------------------------
  Functions for each kind of type
-------------------------------------------------------------------------------}

prim :: C.PrimType -> CXType -> ParseType (C.Type Parse)
prim ty _ = return $ C.TypePrim ty

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
      CXCursor_TypedefDecl -> C.TypeTypedef <$> typedefName declId
      kind                 -> throwError $ UnexpectedTypeDecl (Right kind)
  where
    typedefName :: C.PrelimDeclId -> ParseType C.Name
    typedefName = \case
      C.PrelimDeclIdNamed name   -> return name
      C.PrelimDeclIdAnon{}       -> panicPure "Unexpected anonymous typedef"
      C.PrelimDeclIdBuiltin name -> throwError $ UnsupportedBuiltin name

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

