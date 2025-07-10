-- | Fold types
module HsBindgen.Frontend.Pass.Parse.Type (fromCXType) where

import Control.Monad
import Control.Monad.Error.Class
import GHC.Stack

import Clang.LowLevel.Core
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Type.DeclId
import HsBindgen.Frontend.Pass.Parse.Type.Monad
import HsBindgen.Imports
import HsBindgen.Language.C
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
      CXType_Char_S     -> prim $ PrimChar (PrimSignImplicit $ Just Signed)
      CXType_Char_U     -> prim $ PrimChar (PrimSignImplicit $ Just Unsigned)
      CXType_SChar      -> prim $ PrimChar (PrimSignExplicit Signed)
      CXType_UChar      -> prim $ PrimChar (PrimSignExplicit Unsigned)
      CXType_Short      -> prim $ PrimIntegral PrimShort    Signed
      CXType_UShort     -> prim $ PrimIntegral PrimShort    Unsigned
      CXType_Int        -> prim $ PrimIntegral PrimInt      Signed
      CXType_UInt       -> prim $ PrimIntegral PrimInt      Unsigned
      CXType_Long       -> prim $ PrimIntegral PrimLong     Signed
      CXType_ULong      -> prim $ PrimIntegral PrimLong     Unsigned
      CXType_LongLong   -> prim $ PrimIntegral PrimLongLong Signed
      CXType_ULongLong  -> prim $ PrimIntegral PrimLongLong Unsigned
      CXType_Float      -> prim $ PrimFloating PrimFloat
      CXType_Double     -> prim $ PrimFloating PrimDouble
      CXType_LongDouble -> failure UnsupportedLongDouble
      CXType_Bool       -> prim $ PrimBool

      CXType_Attributed      -> attributed
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

prim :: PrimType -> CXType -> ParseType (C.Type Parse)
prim ty _ = return $ C.TypePrim ty

elaborated :: CXType -> ParseType (C.Type Parse)
elaborated = clang_Type_getNamedType >=> cxtype

pointer :: CXType -> ParseType (C.Type Parse)
pointer = clang_getPointeeType >=> fmap C.TypePointer . cxtype

fromDecl :: HasCallStack => CXType -> ParseType (C.Type Parse)
fromDecl ty = do
    decl   <- clang_getTypeDeclaration ty
    declId <- getDeclId decl
    dispatchDecl decl $ \case
      CXCursor_EnumDecl    -> return $ C.TypeEnum    declId C.NameOriginInSource
      CXCursor_StructDecl  -> return $ C.TypeStruct  declId C.NameOriginInSource
      CXCursor_UnionDecl   -> return $ C.TypeUnion   declId C.NameOriginInSource
      CXCursor_TypedefDecl -> C.TypeTypedef <$> typedefName declId
      kind                 -> throwError $ UnexpectedTypeDecl (Right kind)
  where
    typedefName :: DeclId -> ParseType CName
    typedefName (DeclNamed name)   = return name
    typedefName (DeclAnon _)       = panicPure "Unexpected anonymous typedef"
    typedefName (DeclBuiltin name) = throwError $ UnsupportedBuiltin name

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
