{-# LANGUAGE CPP #-}

module HsBindgen.Runtime.HasFFIType (
    -- * Class
    HasFFIType (FFIType, toFFIType, fromFFIType)
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

-- | The 'HasFFIType' class broadly captures Haskell types that can be
-- converted to and from an /FFI type/.
--
-- An FFI type is similar to a /foreign type/, but with all newtypes removed.
-- Foreign types are the kinds of types that are allowed in @foreign import@
-- declarations.
--
-- Some laws apply to this class:
--
-- * If @x :: a@ is a foreign type, then @toFFIType x :: FFIType
--   a@ is also a valid foreign type and contains no newtypes.
-- * If @x :: FFIType a@ is a foreign type, then @fromFFIType x
--   :: a@ is also a valid foreign type.
--
-- Note in particular that this does /not/ guarantee that:
--
-- * Every type @a@ that is an instance of 'HasFFIType' is a valid
-- foreign type
-- * Every type @'FFIType' a@ is a valid foreign type.
--
-- Informally, 'toFFIType' and 'fromFFIType' preserve
-- /valid-foreign-type-ness/.
--
-- === User-supplied instances
--
-- Generally as a rule of thumb, if @a@ is a valid foreign type, then there
-- should be a sensible 'HasFFIType' instance. Instances are provided in this
-- module for most basic type constructors, like 'Prelude.(->)', 'IO',
-- 'Prelude.()', and all eligible types from the "Foreign" module hierarchy.
-- However, we can't magically generate instance for user-defined newtypes, nor
-- do we try to generate instances for all newtypes from the @base@ package or
-- other core packages. Instead, the user should newtype-derive those instances
-- or write them by hand. The @UndecidableInstances@ language extension should
-- probably also be enabled.
--
-- === Foreign types
--
-- Foreign types and its sub-kinds are described by the the "Haskell 2010 Language"
-- report. Kinds of foreign types include:
--
-- * top-level /foreign types/
-- * /basic foreign types/
-- * /marshallable foreign result types/
-- * /marshallable foreign types/
--
-- See the "8.4.2 Foreign Types" section of the report for more information:
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
  type FFIType (Ptr a) = Ptr Void
  {-# INLINE toFFIType #-}
  toFFIType = castPtr
  {-# INLINE fromFFIType #-}
  fromFFIType = castPtr

instance HasFFIType (FunPtr a) where
  type FFIType (FunPtr a) = FunPtr Void
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
