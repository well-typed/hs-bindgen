{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}

-- NOTE: For now, this module is classified "Support"; however, it may become
-- public in the future. See also
-- https://github.com/well-typed/hs-bindgen/issues/1565.

module HsBindgen.Runtime.Support.HasFFIType (
    -- * Class
    HasFFIType (FFIType, toFFIType, fromFFIType)
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
  -- | Convert a foreign type to its FFI type.
  --
  -- See the 'HasFFIType' class for more information
  toFFIType :: a -> FFIType a
  -- | Convert an FFI type a foreign type.
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
  Foreign types
-------------------------------------------------------------------------------}

instance (HasFFIType a, HasFFIType b) => HasFFIType (a -> b) where
  type FFIType (a -> b) = FFIType a -> FFIType b
  {-# INLINE toFFIType #-}
  toFFIType f = \x -> toFFIType (f $ fromFFIType x)
  {-# INLINE fromFFIType #-}
  fromFFIType f = \x -> fromFFIType (f $ toFFIType x)

{-------------------------------------------------------------------------------
  Marshallable foreign result types
-------------------------------------------------------------------------------}

deriving via ViaIdentity () instance HasFFIType ()

instance HasFFIType a => HasFFIType (IO a) where
  type FFIType (IO a) = IO (FFIType a)
  {-# INLINE toFFIType #-}
  toFFIType = fmap toFFIType
  {-# INLINE fromFFIType #-}
  fromFFIType = fmap fromFFIType

{-------------------------------------------------------------------------------
  Marshallable foreign types
-------------------------------------------------------------------------------}

-- === Prelude ===

-- == Basic foreign types ==

deriving via ViaIdentity Char   instance HasFFIType Char
deriving via ViaIdentity Int    instance HasFFIType Int
deriving via ViaIdentity Double instance HasFFIType Double
deriving via ViaIdentity Float  instance HasFFIType Float
deriving via ViaIdentity Bool   instance HasFFIType Bool

-- === Data.Int ===

-- == Basic foreign types ==

deriving via ViaIdentity Int8  instance HasFFIType Int8
deriving via ViaIdentity Int16 instance HasFFIType Int16
deriving via ViaIdentity Int32 instance HasFFIType Int32
deriving via ViaIdentity Int64 instance HasFFIType Int64

-- === Data.Word ===

-- == Basic foreign types ==

deriving via ViaIdentity Word   instance HasFFIType Word
deriving via ViaIdentity Word8  instance HasFFIType Word8
deriving via ViaIdentity Word16 instance HasFFIType Word16
deriving via ViaIdentity Word32 instance HasFFIType Word32
deriving via ViaIdentity Word64 instance HasFFIType Word64

-- === Foreign.Ptr ===

-- == Basic foreign types ==

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

-- == Newtypes around basic foreign types ==

deriving newtype instance HasFFIType IntPtr
deriving newtype instance HasFFIType WordPtr

-- === Foreign.StablePtr ===

-- == Basic foreign types ==

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

-- == Newtypes around basic foreign types ==

instance HasFFIType (PtrConst a) where
  type FFIType (PtrConst a) = Ptr Void
  {-# INLINE toFFIType #-}
  toFFIType = castPtr . unsafeToPtr
  {-# INLINE fromFFIType #-}
  fromFFIType = unsafeFromPtr . castPtr

-- === Foreign.C.Error ===

-- == Newtypes around basic foreign types ==

deriving newtype instance HasFFIType Errno

-- === Foreign.C.Types ===

-- == Newtypes around basic foreign types ==

deriving newtype instance HasFFIType CChar
deriving newtype instance HasFFIType CSChar
deriving newtype instance HasFFIType CUChar
deriving newtype instance HasFFIType CShort
deriving newtype instance HasFFIType CUShort
deriving newtype instance HasFFIType CInt
deriving newtype instance HasFFIType CUInt
deriving newtype instance HasFFIType CLong
deriving newtype instance HasFFIType CULong
deriving newtype instance HasFFIType CPtrdiff
deriving newtype instance HasFFIType CSize
deriving newtype instance HasFFIType CWchar
deriving newtype instance HasFFIType CSigAtomic
deriving newtype instance HasFFIType CLLong
deriving newtype instance HasFFIType CULLong
deriving newtype instance HasFFIType CBool
deriving newtype instance HasFFIType CIntPtr
deriving newtype instance HasFFIType CUIntPtr
deriving newtype instance HasFFIType CIntMax
deriving newtype instance HasFFIType CUIntMax

-- === Foreign.C.Types : Numeric types ===

-- == Newtypes around basic foreign types ==

deriving newtype instance HasFFIType CClock
deriving newtype instance HasFFIType CTime
deriving newtype instance HasFFIType CUSeconds
deriving newtype instance HasFFIType CSUSeconds

-- === Foreign.C.Types : Floating types ===

-- == Newtypes around basic foreign types ==

deriving newtype instance HasFFIType CFloat
deriving newtype instance HasFFIType CDouble

