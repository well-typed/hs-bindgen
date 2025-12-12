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
  , toBaseForeignType
  , toFFIType
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Stack
import Text.Printf (printf)

import HsBindgen.Runtime.BaseForeignType qualified as BFT

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.BindingSpec.Private.FFIType qualified as FFIType
import HsBindgen.Errors
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.Naming qualified as C
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

topLevel :: HasCallStack => C.Type -> Hs.HsType
topLevel = inContext Top

inContext :: HasCallStack => TypeContext -> C.Type -> Hs.HsType
inContext ctx = go ctx
  where
    go :: TypeContext -> C.Type -> Hs.HsType
    go _ (C.TypeTypedef (C.TypedefRef declId _)) =
        Hs.HsTypRef (C.unsafeDeclIdHaskellName declId)
    go _ (C.TypeRef declId) =
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

toBaseForeignType :: NewtypeMap -> HsType -> HsBaseForeignType
toBaseForeignType ntMap t = HsBaseForeignType $ go t
  where
    go :: HsType -> BFT.BaseForeignType
    go = \case
      HsPrimType pt -> unHsBaseForeignType $ primToBaseForeignType pt
      HsTypRef name
        | Just t' <- Map.lookup name ntMap
        -> go t'
        -- If the referenced type is not in the newtype map, then it is
        -- assumed to be a struct or union
        | otherwise
        -> no
      HsConstArray{} -> no
      HsIncompleteArray{} -> no
      HsPtr{} -> BFT.Basic FFIType.Ptr
      HsFunPtr{} -> BFT.Basic FFIType.FunPtr
      HsStablePtr{} -> BFT.Basic FFIType.StablePtr
      HsConstPtr{} -> BFT.Builtin FFIType.ConstPtr
      HsIO t' -> BFT.IO $ go t'
      HsFun s t' -> BFT.FunArrow (go s) (go t')
      HsExtBinding extRef _ mHsTypeSpec ->
          case mHsTypeSpec >>= BindingSpec.hsTypeSpecFFIType of
            Nothing ->
              panicPure $
              printf "toBaseForeignType: no ffitype found for external reference %s"
                    (show extRef)
            Just t' -> case t' of
              FFIType.Function -> no
              FFIType.Data -> no
              FFIType.Array -> no
              FFIType.Basic t'' -> BFT.Basic t''
              FFIType.Builtin t'' -> BFT.Builtin t''

      HsByteArray -> no
      HsSizedByteArray{} -> no
      HsBlock{} -> BFT.Basic FFIType.Ptr
      HsComplexType{} -> no
      HsStrLit{} -> no
    no = panicPure "primToBaseForeignType: %s is not a a base foreign type" (show t)

primToBaseForeignType :: HsPrimType -> HsBaseForeignType
primToBaseForeignType pt = HsBaseForeignType $ case pt of
    HsPrimVoid -> no
    HsPrimUnit -> BFT.Unit
    HsPrimCStringLen -> no
    HsPrimChar -> BFT.Basic FFIType.Char
    HsPrimInt -> BFT.Basic FFIType.Int
    HsPrimDouble -> BFT.Basic FFIType.Double
    HsPrimFloat -> BFT.Basic FFIType.Float
    HsPrimBool -> BFT.Basic FFIType.Bool
    HsPrimInt8 -> BFT.Basic FFIType.Int8
    HsPrimInt16 -> BFT.Basic FFIType.Int16
    HsPrimInt32 -> BFT.Basic FFIType.Int32
    HsPrimInt64 -> BFT.Basic FFIType.Int64
    HsPrimWord -> BFT.Basic FFIType.Word
    HsPrimWord8 -> BFT.Basic FFIType.Word8
    HsPrimWord16 -> BFT.Basic FFIType.Word16
    HsPrimWord32 -> BFT.Basic FFIType.Word32
    HsPrimWord64 -> BFT.Basic FFIType.Word64
    HsPrimIntPtr -> BFT.Builtin FFIType.IntPtr
    HsPrimWordPtr -> BFT.Builtin FFIType.WordPtr
    HsPrimCChar -> BFT.Builtin FFIType.CChar
    HsPrimCSChar -> BFT.Builtin FFIType.CSChar
    HsPrimCUChar -> BFT.Builtin FFIType.CUChar
    HsPrimCShort -> BFT.Builtin FFIType.CShort
    HsPrimCUShort -> BFT.Builtin FFIType.CUShort
    HsPrimCInt -> BFT.Builtin FFIType.CInt
    HsPrimCUInt -> BFT.Builtin FFIType.CUInt
    HsPrimCLong -> BFT.Builtin FFIType.CLong
    HsPrimCULong -> BFT.Builtin FFIType.CULong
    HsPrimCPtrdiff -> BFT.Builtin FFIType.CPtrdiff
    HsPrimCSize -> BFT.Builtin FFIType.CSize
    HsPrimCWchar -> BFT.Builtin FFIType.CWchar
    HsPrimCSigAtomic -> BFT.Builtin FFIType.CSigAtomic
    HsPrimCLLong -> BFT.Builtin FFIType.CLLong
    HsPrimCULLong -> BFT.Builtin FFIType.CULLong
    HsPrimCBool -> BFT.Builtin FFIType.CBool
    HsPrimCIntPtr -> BFT.Builtin FFIType.CIntPtr
    HsPrimCUIntPtr -> BFT.Builtin FFIType.CUIntPtr
    HsPrimCIntMax -> BFT.Builtin FFIType.CIntMax
    HsPrimCUIntMax -> BFT.Builtin FFIType.CUIntMax
    HsPrimCClock -> BFT.Builtin FFIType.CClock
    HsPrimCTime -> BFT.Builtin FFIType.CTime
    HsPrimCUSeconds -> BFT.Builtin FFIType.CUSeconds
    HsPrimCSUSeconds -> BFT.Builtin FFIType.CSUSeconds
    HsPrimCFloat -> BFT.Builtin FFIType.CFloat
    HsPrimCDouble -> BFT.Builtin FFIType.CDouble
  where
    no = panicPure "primToBaseForeignType: %s is not a a base foreign type" (show pt)

toFFIType :: NewtypeMap -> HsType -> FFIType.FFIType
toFFIType ntMap = \case
    HsPrimType pt -> primToFFIType pt
    HsTypRef name
      | Just t' <- Map.lookup name ntMap
      -> toFFIType ntMap t'
      -- If the referenced type is not in the newtype map, then it is
      -- assumed to be a struct or union
      | otherwise
      -> FFIType.Data
    HsConstArray{} -> FFIType.Array
    HsIncompleteArray{} -> FFIType.Array
    HsPtr{} -> FFIType.Basic FFIType.Ptr
    HsFunPtr{} -> FFIType.Basic FFIType.FunPtr
    HsStablePtr{} -> FFIType.Basic FFIType.StablePtr
    HsConstPtr{} -> FFIType.Builtin FFIType.ConstPtr
    HsIO{} -> FFIType.Function
    HsFun{} -> FFIType.Function
    HsExtBinding extRef _ mHsTypeSpec ->
        case mHsTypeSpec >>= BindingSpec.hsTypeSpecFFIType of
          Nothing ->
            panicPure $
            printf "toFFIType: no ffitype found for external reference %s"
                   (show extRef)
          Just t' -> t'
    HsByteArray -> FFIType.Data
    HsSizedByteArray{} -> FFIType.Data
    HsBlock{} -> FFIType.Basic FFIType.Ptr
    HsComplexType{} -> FFIType.Data
    HsStrLit{} -> panicPure "toFFIType: string literals are not an ffitype"

primToFFIType :: HsPrimType -> FFIType.FFIType
primToFFIType = \case
    HsPrimVoid -> FFIType.Data
    HsPrimUnit -> FFIType.Data
    HsPrimCStringLen -> FFIType.Data
    HsPrimChar -> FFIType.Basic FFIType.Char
    HsPrimInt -> FFIType.Basic FFIType.Int
    HsPrimDouble -> FFIType.Basic FFIType.Double
    HsPrimFloat -> FFIType.Basic FFIType.Float
    HsPrimBool -> FFIType.Basic FFIType.Bool
    HsPrimInt8 -> FFIType.Basic FFIType.Int8
    HsPrimInt16 -> FFIType.Basic FFIType.Int16
    HsPrimInt32 -> FFIType.Basic FFIType.Int32
    HsPrimInt64 -> FFIType.Basic FFIType.Int64
    HsPrimWord -> FFIType.Basic FFIType.Word
    HsPrimWord8 -> FFIType.Basic FFIType.Word8
    HsPrimWord16 -> FFIType.Basic FFIType.Word16
    HsPrimWord32 -> FFIType.Basic FFIType.Word32
    HsPrimWord64 -> FFIType.Basic FFIType.Word64
    HsPrimIntPtr -> FFIType.Builtin FFIType.IntPtr
    HsPrimWordPtr -> FFIType.Builtin FFIType.WordPtr
    HsPrimCChar -> FFIType.Builtin FFIType.CChar
    HsPrimCSChar -> FFIType.Builtin FFIType.CSChar
    HsPrimCUChar -> FFIType.Builtin FFIType.CUChar
    HsPrimCShort -> FFIType.Builtin FFIType.CShort
    HsPrimCUShort -> FFIType.Builtin FFIType.CUShort
    HsPrimCInt -> FFIType.Builtin FFIType.CInt
    HsPrimCUInt -> FFIType.Builtin FFIType.CUInt
    HsPrimCLong -> FFIType.Builtin FFIType.CLong
    HsPrimCULong -> FFIType.Builtin FFIType.CULong
    HsPrimCPtrdiff -> FFIType.Builtin FFIType.CPtrdiff
    HsPrimCSize -> FFIType.Builtin FFIType.CSize
    HsPrimCWchar -> FFIType.Builtin FFIType.CWchar
    HsPrimCSigAtomic -> FFIType.Builtin FFIType.CSigAtomic
    HsPrimCLLong -> FFIType.Builtin FFIType.CLLong
    HsPrimCULLong -> FFIType.Builtin FFIType.CULLong
    HsPrimCBool -> FFIType.Builtin FFIType.CBool
    HsPrimCIntPtr -> FFIType.Builtin FFIType.CIntPtr
    HsPrimCUIntPtr -> FFIType.Builtin FFIType.CUIntPtr
    HsPrimCIntMax -> FFIType.Builtin FFIType.CIntMax
    HsPrimCUIntMax -> FFIType.Builtin FFIType.CUIntMax
    HsPrimCClock -> FFIType.Builtin FFIType.CClock
    HsPrimCTime -> FFIType.Builtin FFIType.CTime
    HsPrimCUSeconds -> FFIType.Builtin FFIType.CUSeconds
    HsPrimCSUSeconds -> FFIType.Builtin FFIType.CSUSeconds
    HsPrimCFloat -> FFIType.Builtin FFIType.CFloat
    HsPrimCDouble -> FFIType.Builtin FFIType.CDouble
