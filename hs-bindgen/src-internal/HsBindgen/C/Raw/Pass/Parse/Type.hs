-- | Fold types (at /use/ sites, not at declaration sites)
module HsBindgen.C.Raw.Pass.Parse.Type (
    fromCXType
  ) where

import Control.Monad
import GHC.Stack

import Clang.LowLevel.Core

import HsBindgen.C.Raw.AST
import HsBindgen.C.Raw.Pass.Parse.IsPass
import HsBindgen.C.Raw.Pass.Parse.Monad
import HsBindgen.C.Raw.Util
import HsBindgen.Errors

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

fromCXType :: HasCallStack => CXType -> M (Type Parsed)
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

      CXType_Record  -> fromDecl
      CXType_Typedef -> fromDecl

      kind -> \_ -> panicIO $ "fromCXType: " ++ show kind

{-------------------------------------------------------------------------------
  Functions for each kind of type
-------------------------------------------------------------------------------}

prim :: PrimType -> CXType -> M (Type Parsed)
prim ty _ = return $ TypePrim ty

elaborated :: CXType -> M (Type Parsed)
elaborated = clang_Type_getNamedType >=> fromCXType

pointer :: CXType -> M (Type Parsed)
pointer = clang_getPointeeType >=> fmap TypePointer . fromCXType

fromDecl :: HasCallStack => CXType -> M (Type Parsed)
fromDecl ty = do
    decl   <- clang_getTypeDeclaration ty
    declId <- getDeclId decl
    dispatch decl $ \case
      CXCursor_TypedefDecl -> return $ TypeTypedef declId
      CXCursor_StructDecl  -> return $ TypeStruct  declId
      kind -> panicIO $ "fromDecl: " ++ show kind
