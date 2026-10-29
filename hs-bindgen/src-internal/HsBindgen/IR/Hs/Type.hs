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
  , ffiIdentifier
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
  | ExtBinding Hs.Identifier BindingSpec.CTypeSpec BindingSpec.HsTypeSpec
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

-- | A function type using FFI types in argument and result positions
data FFIFunType = FFIFunType {
    args :: [FFIType]
  , res  :: FFIResType
  }
  deriving stock (Generic, Show, Eq)

unconsArg :: FFIFunType -> Either (FFIType, FFIFunType) FFIResType
unconsArg ty = case ty.args of
    [] -> Right ty.res
    (arg:args) -> Left (arg, FFIFunType { args = args, res = ty.res })

-- | A function result type using FFI types
data FFIResType =
    FFIResUnit
  | FFIResIOUnit
  | FFIResIO FFIType
  | FFIRes FFIType
  deriving stock (Generic, Show, Eq)

data FFIType =
    FFIExternal Hs.Identifier

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

-- | Convert an 'FFIType' to an 'Hs.Identifier'.
--
-- NOTE: this is very similar logic to
-- 'HsBindgen.Backend.SHs.Translation.translateFFIType'. We could try to unify
-- this in the future.
ffiIdentifier :: FFIType -> Hs.Identifier
ffiIdentifier = \case
    FFIExternal r  -> r
    FFIPtrVoid     -> mkIdent "HsBindgen.Runtime.Support" "PtrVoid"
    FFIFunPtrVoid  -> mkIdent "HsBindgen.Runtime.Support" "FunPtrVoid"
    FFIPrimCChar   -> mkIdent "HsBindgen.Runtime.Support" "CChar"
    FFIPrimCSChar  -> mkIdent "HsBindgen.Runtime.Support" "CSChar"
    FFIPrimCUChar  -> mkIdent "HsBindgen.Runtime.Support" "CUChar"
    FFIPrimCShort  -> mkIdent "HsBindgen.Runtime.Support" "CShort"
    FFIPrimCUShort -> mkIdent "HsBindgen.Runtime.Support" "CUShort"
    FFIPrimCInt    -> mkIdent "HsBindgen.Runtime.Support" "CInt"
    FFIPrimCUInt   -> mkIdent "HsBindgen.Runtime.Support" "CUInt"
    FFIPrimCLong   -> mkIdent "HsBindgen.Runtime.Support" "CLong"
    FFIPrimCULong  -> mkIdent "HsBindgen.Runtime.Support" "CULong"
    FFIPrimCLLong  -> mkIdent "HsBindgen.Runtime.Support" "CLLong"
    FFIPrimCULLong -> mkIdent "HsBindgen.Runtime.Support" "CULLong"
    FFIPrimCBool   -> mkIdent "HsBindgen.Runtime.Support" "CBool"
    FFIPrimCFloat  -> mkIdent "HsBindgen.Runtime.Support" "CFloat"
    FFIPrimCDouble -> mkIdent "HsBindgen.Runtime.Support" "CDouble"
  where
    mkIdent :: Hs.ModuleName -> Text -> Hs.Identifier
    mkIdent moduleName typeName =
        Hs.Identifier moduleName (Hs.UnsafeSomeName Hs.NsTypeConstr typeName)
