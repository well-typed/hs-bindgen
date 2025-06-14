-- | Fold types (at /use/ sites, not at declaration sites)
module HsBindgen.Frontend.Pass.Parse.Type (
    fromCXType
  ) where

import Control.Monad
import GHC.Stack

import Clang.LowLevel.Core

import HsBindgen.Errors
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Monad
import HsBindgen.Frontend.Util.Fold
import HsBindgen.Language.C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

fromCXType :: HasCallStack => CXType -> M (C.Type Parse)
fromCXType ty =
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
      CXType_LongDouble -> prim $ PrimFloating PrimLongDouble
      CXType_Bool       -> prim $ PrimBool

      CXType_Attributed      -> attributed
      CXType_ConstantArray   -> constantArray
      CXType_Elaborated      -> elaborated
      CXType_Enum            -> fromDecl
      CXType_FunctionNoProto -> function
      CXType_FunctionProto   -> function
      CXType_IncompleteArray -> incompleteArray
      CXType_Pointer         -> pointer
      CXType_Record          -> fromDecl
      CXType_Typedef         -> fromDecl
      CXType_Void            -> const (pure C.TypeVoid)

      kind -> unknownTypeKind kind

{-------------------------------------------------------------------------------
  Functions for each kind of type
-------------------------------------------------------------------------------}

prim :: PrimType -> CXType -> M (C.Type Parse)
prim ty _ = return $ C.TypePrim ty

elaborated :: CXType -> M (C.Type Parse)
elaborated = clang_Type_getNamedType >=> fromCXType

pointer :: CXType -> M (C.Type Parse)
pointer = clang_getPointeeType >=> fmap C.TypePointer . fromCXType

fromDecl :: HasCallStack => CXType -> M (C.Type Parse)
fromDecl ty = do
    decl   <- clang_getTypeDeclaration ty
    declId <- getDeclId decl
    dispatch decl $ \case
      CXCursor_EnumDecl    -> return $ C.TypeEnum    declId
      CXCursor_StructDecl  -> return $ C.TypeStruct  declId
      CXCursor_UnionDecl   -> return $ C.TypeUnion   declId
      CXCursor_TypedefDecl -> return $ C.TypeTypedef (typedefName declId)
      kind                 -> unknownCursorKind kind decl
  where
    typedefName :: DeclId -> CName
    typedefName (DeclNamed name) = name
    typedefName (DeclAnon _)     = panicPure "Unexpected anonymous typedef"

function :: CXType -> M (C.Type Parse)
function ty = do
    res   <- clang_getResultType ty >>= fromCXType
    nargs <- clang_getNumArgTypes ty
    args  <- forM [0 .. nargs - 1] $ \i ->
               clang_getArgType ty (fromIntegral i) >>= fromCXType
    pure $ C.TypeFun args res

constantArray :: CXType -> M (C.Type Parse)
constantArray ty = do
    n   <- fromIntegral <$> clang_getArraySize ty
    ty' <- fromCXType =<< clang_getArrayElementType ty
    return (C.TypeConstArray n ty')

incompleteArray :: CXType -> M (C.Type Parse)
incompleteArray ty = do
    ty' <- fromCXType =<< clang_getArrayElementType ty
    return (C.TypeIncompleteArray ty')

attributed :: CXType -> M (C.Type Parse)
attributed ty = fromCXType =<< clang_Type_getModifiedType ty
