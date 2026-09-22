{-# LANGUAGE CPP #-}

module HsBindgen.Runtime.HasFFIType (
    -- * Class
    HasFFIType (FFIType, toFFIType, fromFFIType)
    -- * Shorthand types
  , PtrVoid
  , FunPtrVoid
    -- * Deriving-via
  , ViaIdentity (..)
  ) where

import Prelude as Types (Bool, Char, Double, Float, Int, Word)
import Prelude hiding (Bool, Char, Double, Float, Int, Word)

import Data.Int as Types (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Void (Void)
import Data.Word as Types (Word16, Word32, Word64, Word8)
import Foreign.C.Error as Types (Errno (..))
import Foreign.C.Types as Types (CBool (..), CChar (..), CClock (..),
                                 CDouble (..), CFloat (..), CInt (..),
                                 CIntMax (..), CIntPtr (..), CLLong (..),
                                 CLong (..), CPtrdiff (..), CSChar (..),
                                 CSUSeconds (..), CShort (..), CSigAtomic (..),
                                 CSize (..), CTime (..), CUChar (..),
                                 CUInt (..), CUIntMax (..), CUIntPtr (..),
                                 CULLong (..), CULong (..), CUSeconds (..),
                                 CUShort (..), CWchar (..))
import Foreign.Ptr (castFunPtr, castPtr)
import Foreign.Ptr as Types (FunPtr, IntPtr (..), Ptr, WordPtr (..))
import Foreign.StablePtr (castPtrToStablePtr, castStablePtrToPtr)
import Foreign.StablePtr as Types (StablePtr)

import HsBindgen.Runtime.PtrConst as Types (PtrConst, unsafeFromPtr,
                                            unsafeToPtr)

{-------------------------------------------------------------------------------
  Class
-------------------------------------------------------------------------------}

-- | The 'HasFFIType' class captures Haskell types that can be converted to and
-- from its /FFI type/.
--
-- A 'HasFFIType' instance declaration for a type @T@, mapping @FFIType T@ to
-- @M.T'@, is valid if @T'@ is legal to appear as an argument or result in
-- @foreign import@ declarations in a context where @M@ is in scope.
--
-- @foreign import@ declarations only compile if their type is a valid /foreign
-- type/. This depends on the context of which modules are in scope. A @foreign
-- import@ that uses FFI types exclusively will always compile.
--
-- Foreign types and its sub-kinds are described by the the "Haskell 2010
-- Language" report. See the "8.4.2 Foreign Types" section of the report for
-- more information:
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1560008.4.2>
--
class HasFFIType a where
  type FFIType a :: Type
  -- | Convert a type to its FFI type
  --
  -- See the 'HasFFIType' class for more information
  toFFIType :: a -> FFIType a
  -- | Inverse of 'toFFIType'
  --
  -- See the 'HasFFIType' class for more information
  fromFFIType :: FFIType a -> a

{-------------------------------------------------------------------------------
  Shorthand types
-------------------------------------------------------------------------------}

-- | 'Ptr' 'Void'
type PtrVoid = Ptr Void

-- | 'FunPtr' 'Void'
type FunPtrVoid = FunPtr Void

{-------------------------------------------------------------------------------
  Deriving-via
-------------------------------------------------------------------------------}

type ViaIdentity :: Type -> Type
newtype ViaIdentity a = ViaIdentity a

instance HasFFIType (ViaIdentity a) where
  type FFIType (ViaIdentity a) = a
  {-# INLINE toFFIType #-}
  toFFIType (ViaIdentity x) = x
  {-# INLINE fromFFIType #-}
  fromFFIType x = ViaIdentity x

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

-- === Prelude ===

deriving via ViaIdentity Char   instance HasFFIType Char
deriving via ViaIdentity Int    instance HasFFIType Int
deriving via ViaIdentity Double instance HasFFIType Double
deriving via ViaIdentity Float  instance HasFFIType Float
deriving via ViaIdentity Bool   instance HasFFIType Bool

-- === Data.Int ===

deriving via ViaIdentity Int8  instance HasFFIType Int8
deriving via ViaIdentity Int16 instance HasFFIType Int16
deriving via ViaIdentity Int32 instance HasFFIType Int32
deriving via ViaIdentity Int64 instance HasFFIType Int64

-- === Data.Word ===

deriving via ViaIdentity Word   instance HasFFIType Word
deriving via ViaIdentity Word8  instance HasFFIType Word8
deriving via ViaIdentity Word16 instance HasFFIType Word16
deriving via ViaIdentity Word32 instance HasFFIType Word32
deriving via ViaIdentity Word64 instance HasFFIType Word64

-- === Foreign.Ptr ===

instance HasFFIType (Ptr a) where
  type FFIType (Ptr a) = PtrVoid
  {-# INLINE toFFIType #-}
  toFFIType = castPtr
  {-# INLINE fromFFIType #-}
  fromFFIType = castPtr

instance HasFFIType (FunPtr a) where
  type FFIType (FunPtr a) = FunPtrVoid
  {-# INLINE toFFIType #-}
  toFFIType = castFunPtr
  {-# INLINE fromFFIType #-}
  fromFFIType = castFunPtr

deriving via ViaIdentity IntPtr  instance HasFFIType IntPtr
deriving via ViaIdentity WordPtr instance HasFFIType WordPtr

-- === Foreign.StablePtr ===

instance HasFFIType (StablePtr a) where
  type FFIType (StablePtr a) = StablePtr Void
  {-# INLINE toFFIType #-}
  toFFIType = castStablePtr
  {-# INLINE fromFFIType #-}
  fromFFIType = castStablePtr

{-# INLINE castStablePtr #-}
castStablePtr :: StablePtr a -> StablePtr b
castStablePtr = castPtrToStablePtr . castStablePtrToPtr

-- === Foreign.C.ConstPtr ===

instance HasFFIType (PtrConst a) where
  type FFIType (PtrConst a) = Ptr Void
  {-# INLINE toFFIType #-}
  toFFIType = castPtr . unsafeToPtr
  {-# INLINE fromFFIType #-}
  fromFFIType = unsafeFromPtr . castPtr

-- === Foreign.C.Error ===

deriving via ViaIdentity Errno instance HasFFIType Errno

-- === Foreign.C.Types ===

deriving via ViaIdentity CChar      instance HasFFIType CChar
deriving via ViaIdentity CSChar     instance HasFFIType CSChar
deriving via ViaIdentity CUChar     instance HasFFIType CUChar
deriving via ViaIdentity CShort     instance HasFFIType CShort
deriving via ViaIdentity CUShort    instance HasFFIType CUShort
deriving via ViaIdentity CInt       instance HasFFIType CInt
deriving via ViaIdentity CUInt      instance HasFFIType CUInt
deriving via ViaIdentity CLong      instance HasFFIType CLong
deriving via ViaIdentity CULong     instance HasFFIType CULong
deriving via ViaIdentity CPtrdiff   instance HasFFIType CPtrdiff
deriving via ViaIdentity CSize      instance HasFFIType CSize
deriving via ViaIdentity CWchar     instance HasFFIType CWchar
deriving via ViaIdentity CSigAtomic instance HasFFIType CSigAtomic
deriving via ViaIdentity CLLong     instance HasFFIType CLLong
deriving via ViaIdentity CULLong    instance HasFFIType CULLong
deriving via ViaIdentity CBool      instance HasFFIType CBool
deriving via ViaIdentity CIntPtr    instance HasFFIType CIntPtr
deriving via ViaIdentity CUIntPtr   instance HasFFIType CUIntPtr
deriving via ViaIdentity CIntMax    instance HasFFIType CIntMax
deriving via ViaIdentity CUIntMax   instance HasFFIType CUIntMax

-- === Foreign.C.Types : Numeric types ===

deriving via ViaIdentity CClock     instance HasFFIType CClock
deriving via ViaIdentity CTime      instance HasFFIType CTime
deriving via ViaIdentity CUSeconds  instance HasFFIType CUSeconds
deriving via ViaIdentity CSUSeconds instance HasFFIType CSUSeconds

-- === Foreign.C.Types : Floating types ===

deriving via ViaIdentity CFloat  instance HasFFIType CFloat
deriving via ViaIdentity CDouble instance HasFFIType CDouble
