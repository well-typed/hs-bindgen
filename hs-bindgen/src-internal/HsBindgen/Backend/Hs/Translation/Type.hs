-- | Translate types (use sites)
--
-- Intended for qualified import
--
-- > import HsBindgen.Backend.Hs.Translation.Type qualified as Type
module HsBindgen.Backend.Hs.Translation.Type (
    topLevel
  , TypeContext(..)
  , inContext
  ) where

import GHC.Stack

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Errors
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

data TypeContext =
    Top     -- ^ Anything else
  | FunArg  -- ^ Function argument
  | FunRes  -- ^ Function result
  | PtrArg  -- ^ Pointer argument
  deriving stock (Show)

topLevel :: HasCallStack => C.Type -> Hs.HsType
topLevel = inContext Top

inContext :: HasCallStack => TypeContext -> C.Type -> Hs.HsType
inContext ctx = go ctx
  where
    go :: TypeContext -> C.Type -> Hs.HsType
    go _ (C.TypeTypedef (C.TypedefRegular declId _)) =
        Hs.HsTypRef (C.unsafeDeclIdHaskellName declId)
    go c (C.TypeTypedef (C.TypedefSquashed _name ty)) =
        go c ty
    go _ (C.TypeStruct declId) =
        Hs.HsTypRef (C.unsafeDeclIdHaskellName declId)
    go _ (C.TypeUnion declId) =
        Hs.HsTypRef (C.unsafeDeclIdHaskellName declId)
    go _ (C.TypeEnum declId) =
        Hs.HsTypRef (C.unsafeDeclIdHaskellName declId)
    go _ (C.TypeMacroTypedef declId) =
        Hs.HsTypRef (C.unsafeDeclIdHaskellName declId)
    go c C.TypeVoid =
        Hs.HsPrimType (void c)
    go _ (C.TypePrim p) =
        Hs.HsPrimType (primType p)
    go _ (C.TypePointer t)
      -- Use a 'FunPtr' if the type is a function type. We inspect the
      -- /canonical/ type because we want to see through typedefs and type
      -- qualifiers like @const@.
      | C.isCanonicalTypeFunction t
      = Hs.HsFunPtr (go PtrArg t)
      | C.isErasedTypeConstQualified t
      = Hs.HsConstPtr (go PtrArg t)
      | otherwise
      = Hs.HsPtr (go PtrArg t)
    go _ (C.TypeConstArray n ty) =
        Hs.HsConstArray n $ go Top ty
    go _ (C.TypeIncompleteArray ty) =
        Hs.HsIncompleteArray $ go Top ty
    go _ (C.TypeFun xs y) =
        foldr (\x res -> Hs.HsFun (go FunArg x) res) (Hs.HsIO (go FunRes y)) xs
    go _ (C.TypeBlock ty) =
        HsBlock $ go Top ty
    go _ (C.TypeExtBinding ext) =
        Hs.HsExtBinding (C.extHsRef ext) (C.extCSpec ext) (C.extHsSpec ext)
    go c (C.TypeQualified C.TypeQualifierConst ty) =
        go c ty
    go _ (C.TypeComplex p) =
        Hs.HsComplexType (primType p)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

primType :: C.PrimType -> HsPrimType
primType C.PrimBool           = HsPrimCBool
primType (C.PrimIntegral i s) = integralType i s
primType (C.PrimFloating f)   = floatingType f
primType C.PrimPtrDiff        = HsPrimCPtrDiff
primType C.PrimSize           = HsPrimCSize
primType (C.PrimChar sign)    = primChar sign

primChar :: C.PrimSignChar -> HsPrimType
primChar (C.PrimSignImplicit _inferred)  = HsPrimCChar
primChar (C.PrimSignExplicit C.Signed)   = HsPrimCSChar
primChar (C.PrimSignExplicit C.Unsigned) = HsPrimCUChar

-- | Translate @void@
--
-- This only makes sense in non-top-level contexts.
-- (We take special care in macro type parsing to rule out top-level @void@.)
void :: HasCallStack => TypeContext -> HsPrimType
void FunRes = HsPrimUnit
void PtrArg = HsPrimVoid
void c      = panicPure $ "unexpected type void in context " ++ show c

integralType :: C.PrimIntType -> C.PrimSign -> HsPrimType
integralType C.PrimInt      C.Signed   = HsPrimCInt
integralType C.PrimInt      C.Unsigned = HsPrimCUInt
integralType C.PrimShort    C.Signed   = HsPrimCShort
integralType C.PrimShort    C.Unsigned = HsPrimCUShort
integralType C.PrimLong     C.Signed   = HsPrimCLong
integralType C.PrimLong     C.Unsigned = HsPrimCULong
integralType C.PrimLongLong C.Signed   = HsPrimCLLong
integralType C.PrimLongLong C.Unsigned = HsPrimCULLong

floatingType :: C.PrimFloatType -> HsPrimType
floatingType C.PrimFloat  = HsPrimCFloat
floatingType C.PrimDouble = HsPrimCDouble
