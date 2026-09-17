-- | Haskell types
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.Hs" should be used.
--
-- Within @HsBindgen.IR@, all modules aside from "HsBindgen.IR.Hs" should import
-- this module qualified for consistency.
--
-- > import HsBindgen.IR.Hs.Type qualified as Hs
module HsBindgen.IR.Hs.Type (
    PrimType(..)
  , Type (..)
    -- * FFI types
  , FFIFunType(..)
  , unconsArg
  , FFIResType(..)
  , FFIType(..)
  , ffiExtRef
  ) where

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data PrimType =
      -- * Others
      PrimVoid
    | PrimUnit

      -- * Data.Int
    | PrimInt

      -- * Foreign.C.Types
    | PrimCChar
    | PrimCSChar
    | PrimCUChar
    | PrimCShort
    | PrimCUShort
    | PrimCInt
    | PrimCUInt
    | PrimCLong
    | PrimCULong
    | PrimCLLong
    | PrimCULLong
    | PrimCBool
    | PrimCFloat
    | PrimCDouble
  deriving stock (Eq, Generic, Ord, Show)

data Type =
    PrimType PrimType
  | TypRef (Hs.Name Hs.NsTypeConstr)
      -- | Underlying type (for non-union and non-struct references)
      (Maybe Type)
  | ConstArray Natural Type
  | IncompleteArray Type
  | PtrArrayElem Type
  | PtrConstArrayElem Type
  | Ptr Type
  | FunPtr Type
  | PtrConst Type
  | IO Type
  | Fun Type Type
  | ExtBinding Hs.ExtRef BindingSpec.CTypeSpec BindingSpec.HsTypeSpec
      -- | Underlying type
      Type
  | ByteArray
  | SizedByteArray Natural Natural
  | Block Type
  | ComplexType PrimType
  | StrLit String
  | WithFlam Type Type
  | EquivStorable Type
    -- | 'HsBindgen.Runtime.Struct.IsStructViaReadRaw'
  | IsStructViaReadRaw Type
  deriving stock (Generic, Show, Eq)

{-------------------------------------------------------------------------------
  FFI types
-------------------------------------------------------------------------------}

data FFIFunType = FFIFunType {
    args :: [FFIType]
  , res  :: FFIResType
  }
  deriving stock (Generic, Show, Eq)

unconsArg :: FFIFunType -> Either (FFIType, FFIFunType) FFIResType
unconsArg ty = case ty.args of
    [] -> Right ty.res
    (arg:args) -> Left (arg, FFIFunType { args = args, res = ty.res })

data FFIResType =
    FFIResUnit
  | FFIResIOUnit
  | FFIResIO FFIType
  | FFIRes FFIType
  deriving stock (Generic, Show, Eq)

data FFIType =
    FFIExternal Hs.ExtRef

    -- * Pointers
  | FFIPtrVoid
  | FFIFunPtrVoid

    -- * Primitive
  | FFIPrimCChar
  | FFIPrimCSChar
  | FFIPrimCUChar
  | FFIPrimCShort
  | FFIPrimCUShort
  | FFIPrimCInt
  | FFIPrimCUInt
  | FFIPrimCLong
  | FFIPrimCULong
  | FFIPrimCLLong
  | FFIPrimCULLong
  | FFIPrimCBool
  | FFIPrimCFloat
  | FFIPrimCDouble
  deriving stock (Generic, Show, Eq)

ffiExtRef :: FFIType -> Hs.ExtRef
ffiExtRef = \case
    FFIExternal r  -> r
    FFIPtrVoid     -> mkExtRef "HsBindgen.Runtime.Support" "PtrVoid"
    FFIFunPtrVoid  -> mkExtRef "HsBindgen.Runtime.Support" "FunPtrVoid"
    FFIPrimCChar   -> mkExtRef "HsBindgen.Runtime.Support" "CChar"
    FFIPrimCSChar  -> mkExtRef "HsBindgen.Runtime.Support" "CSChar"
    FFIPrimCUChar  -> mkExtRef "HsBindgen.Runtime.Support" "CUChar"
    FFIPrimCShort  -> mkExtRef "HsBindgen.Runtime.Support" "CShort"
    FFIPrimCUShort -> mkExtRef "HsBindgen.Runtime.Support" "CUShort"
    FFIPrimCInt    -> mkExtRef "HsBindgen.Runtime.Support" "CInt"
    FFIPrimCUInt   -> mkExtRef "HsBindgen.Runtime.Support" "CUInt"
    FFIPrimCLong   -> mkExtRef "HsBindgen.Runtime.Support" "CLong"
    FFIPrimCULong  -> mkExtRef "HsBindgen.Runtime.Support" "CULong"
    FFIPrimCLLong  -> mkExtRef "HsBindgen.Runtime.Support" "CLLong"
    FFIPrimCULLong -> mkExtRef "HsBindgen.Runtime.Support" "CULLong"
    FFIPrimCBool   -> mkExtRef "HsBindgen.Runtime.Support" "CBool"
    FFIPrimCFloat  -> mkExtRef "HsBindgen.Runtime.Support" "CFloat"
    FFIPrimCDouble -> mkExtRef "HsBindgen.Runtime.Support" "CDouble"
  where
    mkExtRef :: Hs.ModuleName -> Text -> Hs.ExtRef
    mkExtRef moduleName typeName = Hs.ExtRef moduleName (Hs.UnsafeName typeName)
