-- | Translate types (use sites)
--
-- Intended for qualified import
--
-- > import HsBindgen.Backend.Hs.Translation.Type qualified as Type
module HsBindgen.Backend.Hs.Translation.Type (
    topLevel
  , TypeContext(..)
  , InContext(..)
  , primType
  ) where

import Data.Proxy (Proxy (..))
import GHC.Stack

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Errors
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass (AdjustedFrom (..))
import HsBindgen.Frontend.Pass.Final
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Translation
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

data TypeContext =
    Top     -- ^ Anything else
  | FunArg  -- ^ Function argument
  | FunRes  -- ^ Function result
  | PtrArg  -- ^ Pointer argument
  deriving stock (Show)

topLevel :: HasCallStack => C.Type Final -> Hs.Type
topLevel = inContext Top

class InContext a where
  inContext :: HasCallStack => TypeContext -> a -> Hs.Type

instance InContext (C.Type Final) where
  inContext ctx = go ctx
    where
      go :: TypeContext -> C.Type Final -> Hs.Type
      go c (C.TypeMacro ref) =
          Hs.TypRef
            (Hs.assertNs (Proxy @Hs.NsTypeConstr) ref.name.hsName)
            (Just $ go c ref.underlying)
      go c (C.TypeTypedef ref) =
          Hs.TypRef
            (Hs.assertNs (Proxy @Hs.NsTypeConstr) ref.name.hsName)
            (Just $ go c ref.underlying)
      go _ (C.TypeRef ref) =
          Hs.TypRef
            (Hs.assertNs (Proxy @Hs.NsTypeConstr) ref.hsName)
            Nothing
      go c (C.TypeEnum ref) =
          Hs.TypRef
            (Hs.assertNs (Proxy @Hs.NsTypeConstr) ref.name.hsName)
            (Just $ go c ref.underlying)
      go c C.TypeVoid =
          Hs.PrimType (void c)
      go _ (C.TypePrim p) =
          Hs.PrimType (primType p)
      go _ (C.TypePointers n t)
        -- Use a 'FunPtr' if the type is a function type. We inspect the
        -- /canonical/ type because we want to see through typedefs and type
        -- qualifiers like @const@.
        | C.isCanonicalTypeFunction t
        = foldr ($) (Hs.FunPtr (go PtrArg t))
                    (replicate (n - 1) Hs.Ptr)
        | C.isErasedTypeConstQualified t
        = foldr ($) (Hs.PtrConst (go PtrArg t))
                    (replicate (n - 1) Hs.Ptr)
        | otherwise
        = foldr ($) (go PtrArg t)
                    (replicate n Hs.Ptr)
      go _ (C.TypeConstArray n ty) =
          Hs.ConstArray n $ go Top ty
      go _ (C.TypeIncompleteArray ty) =
          Hs.IncompleteArray $ go Top ty
      go _ (C.TypeFun xs y) =
          foldr (\x res -> Hs.Fun (inContext FunArg x) res) (Hs.IO (go FunRes y)) xs
      go _ (C.TypeBlock ty) =
          Hs.Block $ go Top ty
      go c (C.TypeExtBinding ref) =
          let ext = ref.name in
          Hs.ExtBinding ext.hsName ext.cSpec ext.hsSpec $ go c ref.underlying
      go c (C.TypeQual C.QualConst ty) =
          go c ty
      go _ (C.TypeComplex p) =
          Hs.ComplexType (primType p)

instance InContext (C.TypeFunArg Final) where
  inContext ctx arg = case arg.ann of
      AdjustedFromArray origTy
          | C.isErasedTypeConstQualified origTy
          -> Hs.PtrConstArrayElem (inContext Top origTy)
          | otherwise
          -> Hs.PtrArrayElem (inContext Top origTy)
      AdjustedFromFunction _origTy -> inContext ctx arg.typ
      NotAdjusted -> inContext ctx arg.typ

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

primType :: C.PrimType -> Hs.PrimType
primType C.PrimBool           = Hs.PrimCBool
primType (C.PrimIntegral i s) = integralType i s
primType (C.PrimFloating f)   = floatingType f
primType (C.PrimChar sign)    = primChar sign

primChar :: C.PrimSignChar -> Hs.PrimType
primChar (C.PrimSignImplicit _inferred)  = Hs.PrimCChar
primChar (C.PrimSignExplicit C.Signed)   = Hs.PrimCSChar
primChar (C.PrimSignExplicit C.Unsigned) = Hs.PrimCUChar

-- | Translate @void@
--
-- This only makes sense in non-top-level contexts.
-- (We take special care in macro type parsing to rule out top-level @void@.)
void :: HasCallStack => TypeContext -> Hs.PrimType
void FunRes = Hs.PrimUnit
void PtrArg = Hs.PrimVoid
void c      = panicPure $ "Unexpected type void in context " ++ show c

integralType :: C.PrimIntType -> C.PrimSign -> Hs.PrimType
integralType C.PrimInt      C.Signed   = Hs.PrimCInt
integralType C.PrimInt      C.Unsigned = Hs.PrimCUInt
integralType C.PrimShort    C.Signed   = Hs.PrimCShort
integralType C.PrimShort    C.Unsigned = Hs.PrimCUShort
integralType C.PrimLong     C.Signed   = Hs.PrimCLong
integralType C.PrimLong     C.Unsigned = Hs.PrimCULong
integralType C.PrimLongLong C.Signed   = Hs.PrimCLLong
integralType C.PrimLongLong C.Unsigned = Hs.PrimCULLong

floatingType :: C.PrimFloatType -> Hs.PrimType
floatingType C.PrimFloat  = Hs.PrimCFloat
floatingType C.PrimDouble = Hs.PrimCDouble
