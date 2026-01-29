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
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass qualified as ResolveBindingSpecs
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

topLevel :: HasCallStack => C.Type Final -> Hs.HsType
topLevel = inContext Top

inContext :: HasCallStack => TypeContext -> C.Type Final -> Hs.HsType
inContext ctx = go ctx
  where
    go :: TypeContext -> C.Type Final -> Hs.HsType
    go c (C.TypeMacro ref) =
        Hs.HsTypRef (Hs.unsafeHsIdHsName ref.name.unsafeHsName) (Just $ go c ref.underlying)
    go c (C.TypeTypedef ref) =
        Hs.HsTypRef (Hs.unsafeHsIdHsName ref.name.unsafeHsName) (Just $ go c ref.underlying)
    go _ (C.TypeRef ref) =
        Hs.HsTypRef (Hs.unsafeHsIdHsName ref.unsafeHsName) Nothing
    go c (C.TypeEnum ref) =
        Hs.HsTypRef (Hs.unsafeHsIdHsName ref.name.unsafeHsName) (Just $ go c ref.underlying)
    go c C.TypeVoid =
        Hs.HsPrimType (void c)
    go _ (C.TypePrim p) =
        Hs.HsPrimType (primType p)
    go _ (C.TypePointers n t)
      -- Use a 'FunPtr' if the type is a function type. We inspect the
      -- /canonical/ type because we want to see through typedefs and type
      -- qualifiers like @const@.
      | C.isCanonicalTypeFunction t
      = foldr ($) (Hs.HsFunPtr (go PtrArg t))
                  (replicate (n - 1) Hs.HsPtr)
      | C.isErasedTypeConstQualified t
      = foldr ($) (Hs.HsPtrConst (go PtrArg t))
                  (replicate (n - 1) Hs.HsPtr)
      | otherwise
      = foldr ($) (go PtrArg t)
                  (replicate n Hs.HsPtr)
    go _ (C.TypeConstArray n ty) =
        Hs.HsConstArray n $ go Top ty
    go _ (C.TypeIncompleteArray ty) =
        Hs.HsIncompleteArray $ go Top ty
    go _ (C.TypeFun xs y) =
        foldr (\x res -> Hs.HsFun (go FunArg x) res) (Hs.HsIO (go FunRes y)) xs
    go _ (C.TypeBlock ty) =
        Hs.HsBlock $ go Top ty
    go c (C.TypeExtBinding ref) =
        let ext = ref.name in
        Hs.HsExtBinding ext.hsName ext.cSpec ext.hsSpec $ go c ref.underlying
    go c (C.TypeQual C.QualConst ty) =
        go c ty
    go _ (C.TypeComplex p) =
        Hs.HsComplexType (primType p)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

primType :: C.PrimType -> Hs.HsPrimType
primType C.PrimBool           = Hs.HsPrimCBool
primType (C.PrimIntegral i s) = integralType i s
primType (C.PrimFloating f)   = floatingType f
primType (C.PrimChar sign)    = primChar sign

primChar :: C.PrimSignChar -> Hs.HsPrimType
primChar (C.PrimSignImplicit _inferred)  = Hs.HsPrimCChar
primChar (C.PrimSignExplicit C.Signed)   = Hs.HsPrimCSChar
primChar (C.PrimSignExplicit C.Unsigned) = Hs.HsPrimCUChar

-- | Translate @void@
--
-- This only makes sense in non-top-level contexts.
-- (We take special care in macro type parsing to rule out top-level @void@.)
void :: HasCallStack => TypeContext -> Hs.HsPrimType
void FunRes = Hs.HsPrimUnit
void PtrArg = Hs.HsPrimVoid
void c      = panicPure $ "unexpected type void in context " ++ show c

integralType :: C.PrimIntType -> C.PrimSign -> Hs.HsPrimType
integralType C.PrimInt      C.Signed   = Hs.HsPrimCInt
integralType C.PrimInt      C.Unsigned = Hs.HsPrimCUInt
integralType C.PrimShort    C.Signed   = Hs.HsPrimCShort
integralType C.PrimShort    C.Unsigned = Hs.HsPrimCUShort
integralType C.PrimLong     C.Signed   = Hs.HsPrimCLong
integralType C.PrimLong     C.Unsigned = Hs.HsPrimCULong
integralType C.PrimLongLong C.Signed   = Hs.HsPrimCLLong
integralType C.PrimLongLong C.Unsigned = Hs.HsPrimCULLong

floatingType :: C.PrimFloatType -> Hs.HsPrimType
floatingType C.PrimFloat  = Hs.HsPrimCFloat
floatingType C.PrimDouble = Hs.HsPrimCDouble
