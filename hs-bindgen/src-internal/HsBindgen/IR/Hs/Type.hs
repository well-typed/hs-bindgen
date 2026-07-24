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
  , Type(..),
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

      -- * Basic FFI types (see 'BasicFFIType')
    | PrimChar
    | PrimInt
    | PrimDouble
    | PrimFloat
    | PrimBool
    | PrimInt8
    | PrimInt16
    | PrimInt32
    | PrimInt64
    | PrimWord
    | PrimWord8
    | PrimWord16
    | PrimWord32
    | PrimWord64

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
  | StablePtr Type
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
    -- | 'HsBindgen.Runtime.Struct.IsStructViaStorable'
  | IsStructViaStorable Type
  deriving stock (Generic, Show, Eq)
