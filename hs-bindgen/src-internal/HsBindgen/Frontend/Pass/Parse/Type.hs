-- | Fold types (at /use/ sites, not at declaration sites)
module HsBindgen.Frontend.Pass.Parse.Type (
    fromCXType
  ) where

import Control.Monad
import GHC.Stack

import Clang.LowLevel.Core

import HsBindgen.Errors
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Monad
import HsBindgen.Frontend.Pass.Parse.Util
import HsBindgen.Language.C.Prim

{-------------------------------------------------------------------------------
  Top-level

  TODO: This needs entries for const and incomplete arrays.
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

      CXType_Elaborated -> elaborated
      CXType_Pointer    -> pointer

      CXType_Enum          -> fromDecl
      CXType_Record        -> fromDecl
      CXType_Typedef       -> fromDecl
      CXType_FunctionProto -> function

      CXType_Void          -> const (pure C.TypeVoid)

      kind -> \_ -> panicIO $ "fromCXType: " ++ show kind

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
      CXCursor_TypedefDecl -> return $ C.TypeTypedef declId NoAnn
      kind -> panicIO $ "fromDecl: " ++ show kind

function :: CXType -> M (C.Type Parse)
function ty = do
    res   <- clang_getResultType ty >>= fromCXType
    nargs <- clang_getNumArgTypes ty
    args  <- forM [0 .. nargs - 1] $ \i ->
               clang_getArgType ty (fromIntegral i) >>= fromCXType
    pure $ C.TypeFun args res
