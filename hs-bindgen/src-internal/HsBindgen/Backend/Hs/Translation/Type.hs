-- | Translate types (use sites)
--
-- Intended for qualified import
--
-- > import HsBindgen.Backend.Hs.Translation.Type qualified as Type
module HsBindgen.Backend.Hs.Translation.Type (
    topLevel
  , TypeContext(..)
  , inContext
    -- * FFI types
  , NewtypeMap
  , toFFIType
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Stack

import HsBindgen.Runtime.BaseForeignType qualified as BFT

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass qualified as ResolveBindingSpecs
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

topLevel :: HasCallStack => C.Type Final -> Hs.HsType
topLevel = inContext Top

inContext :: HasCallStack => TypeContext -> C.Type Final -> Hs.HsType
inContext ctx = go ctx
  where
    go :: TypeContext -> C.Type Final -> Hs.HsType
    go _ (C.TypeMacro ref) =
        Hs.HsTypRef (Hs.unsafeHsIdHsName ref.ref.unsafeHsName)
    go _ (C.TypeTypedef ref) =
        Hs.HsTypRef (Hs.unsafeHsIdHsName ref.ref.unsafeHsName)
    go _ (C.TypeRef ref) =
        Hs.HsTypRef (Hs.unsafeHsIdHsName ref.unsafeHsName)
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
      = foldr ($) (Hs.HsConstPtr (go PtrArg t))
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
        HsBlock $ go Top ty
    go _ (C.TypeExtBinding (C.Ref ext _)) =
        Hs.HsExtBinding ext.hsName ext.cSpec ext.hsSpec
    go c (C.TypeQual C.QualConst ty) =
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
primType C.PrimPtrDiff        = HsPrimCPtrdiff
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

{-------------------------------------------------------------------------------
  FFI types
-------------------------------------------------------------------------------}

type NewtypeMap = Map (Hs.Name Hs.NsTypeConstr) HsType

toFFIType :: NewtypeMap -> HsType -> Maybe BindingSpec.FFIType
toFFIType ntMap = \case
    HsPrimType pt -> primToFFIType pt
    HsTypRef name
      | Just t' <- Map.lookup name ntMap
      -> toFFIType ntMap t'
      -- If the referenced type is not in the newtype map, then it is assumed to
      -- be something other than a newtype, such as a datatype.
      | otherwise
      -> no
    HsConstArray{} -> no
    HsIncompleteArray{} -> no
    HsPtr{} -> yes $ BindingSpec.Basic BFT.Ptr
    HsFunPtr{} -> yes $ BindingSpec.Basic BFT.FunPtr
    HsStablePtr{} -> yes $ BindingSpec.Basic BFT.StablePtr
    HsConstPtr{} -> yes $ BindingSpec.Builtin BFT.ConstPtr
    HsIO{} -> no
    HsFun{} -> no
    HsExtBinding _ _ hsTypeSpec -> do
        case hsTypeSpec.hsRep of
          Just rep -> case rep of
            BindingSpec.HsTypeRepNewtype newtypeRep -> newtypeRep.ffiType
            -- only newtypes can have an FFI type
            _ -> Nothing
          -- this should be impossible because the @ResolveBindingSpecs@ pass
          -- checks that each haskell type spec has a haskell type rep.
          _ -> panicPure "toFFIType: haskell type spec has no type rep"
    HsByteArray -> no
    HsSizedByteArray{} -> no
    HsBlock{} -> yes $ BindingSpec.Basic BFT.Ptr
    HsComplexType{} -> no
    HsStrLit{} -> no
  where
    yes = Just
    no = Nothing

primToFFIType :: HsPrimType -> Maybe BindingSpec.FFIType
primToFFIType = \case
    HsPrimVoid -> no
    HsPrimUnit -> no
    HsPrimCStringLen -> no
    HsPrimChar -> yes $ BindingSpec.Basic BFT.Char
    HsPrimInt -> yes $ BindingSpec.Basic BFT.Int
    HsPrimDouble -> yes $ BindingSpec.Basic BFT.Double
    HsPrimFloat -> yes $ BindingSpec.Basic BFT.Float
    HsPrimBool -> yes $ BindingSpec.Basic BFT.Bool
    HsPrimInt8 -> yes $ BindingSpec.Basic BFT.Int8
    HsPrimInt16 -> yes $ BindingSpec.Basic BFT.Int16
    HsPrimInt32 -> yes $ BindingSpec.Basic BFT.Int32
    HsPrimInt64 -> yes $ BindingSpec.Basic BFT.Int64
    HsPrimWord -> yes $ BindingSpec.Basic BFT.Word
    HsPrimWord8 -> yes $ BindingSpec.Basic BFT.Word8
    HsPrimWord16 -> yes $ BindingSpec.Basic BFT.Word16
    HsPrimWord32 -> yes $ BindingSpec.Basic BFT.Word32
    HsPrimWord64 -> yes $ BindingSpec.Basic BFT.Word64
    HsPrimIntPtr -> yes $ BindingSpec.Builtin BFT.IntPtr
    HsPrimWordPtr -> yes $ BindingSpec.Builtin BFT.WordPtr
    HsPrimCChar -> yes $ BindingSpec.Builtin BFT.CChar
    HsPrimCSChar -> yes $ BindingSpec.Builtin BFT.CSChar
    HsPrimCUChar -> yes $ BindingSpec.Builtin BFT.CUChar
    HsPrimCShort -> yes $ BindingSpec.Builtin BFT.CShort
    HsPrimCUShort -> yes $ BindingSpec.Builtin BFT.CUShort
    HsPrimCInt -> yes $ BindingSpec.Builtin BFT.CInt
    HsPrimCUInt -> yes $ BindingSpec.Builtin BFT.CUInt
    HsPrimCLong -> yes $ BindingSpec.Builtin BFT.CLong
    HsPrimCULong -> yes $ BindingSpec.Builtin BFT.CULong
    HsPrimCPtrdiff -> yes $ BindingSpec.Builtin BFT.CPtrdiff
    HsPrimCSize -> yes $ BindingSpec.Builtin BFT.CSize
    HsPrimCWchar -> yes $ BindingSpec.Builtin BFT.CWchar
    HsPrimCSigAtomic -> yes $ BindingSpec.Builtin BFT.CSigAtomic
    HsPrimCLLong -> yes $ BindingSpec.Builtin BFT.CLLong
    HsPrimCULLong -> yes $ BindingSpec.Builtin BFT.CULLong
    HsPrimCBool -> yes $ BindingSpec.Builtin BFT.CBool
    HsPrimCIntPtr -> yes $ BindingSpec.Builtin BFT.CIntPtr
    HsPrimCUIntPtr -> yes $ BindingSpec.Builtin BFT.CUIntPtr
    HsPrimCIntMax -> yes $ BindingSpec.Builtin BFT.CIntMax
    HsPrimCUIntMax -> yes $ BindingSpec.Builtin BFT.CUIntMax
    HsPrimCClock -> yes $ BindingSpec.Builtin BFT.CClock
    HsPrimCTime -> yes $ BindingSpec.Builtin BFT.CTime
    HsPrimCUSeconds -> yes $ BindingSpec.Builtin BFT.CUSeconds
    HsPrimCSUSeconds -> yes $ BindingSpec.Builtin BFT.CSUSeconds
    HsPrimCFloat -> yes $ BindingSpec.Builtin BFT.CFloat
    HsPrimCDouble -> yes $ BindingSpec.Builtin BFT.CDouble
  where
    yes = Just
    no = Nothing
