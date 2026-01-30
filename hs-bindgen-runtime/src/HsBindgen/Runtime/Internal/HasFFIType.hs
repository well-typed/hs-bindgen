{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- NOTE: For now, this module is classified "Internal"; however, it may become
-- public in the future. See also
-- https://github.com/well-typed/hs-bindgen/issues/1565.

module HsBindgen.Runtime.Internal.HasFFIType (
    -- * Class
    HasFFIType
  , FFIType
  , toFFIType
  , fromFFIType
  , castFunPtrToFFIType
  , castFunPtrFromFFIType
    -- * Deriving-via
  , ViaNewtype(..)
  , ViaCoercible(..)
  ) where

import Prelude as Types (Bool, Char, Double, Float, Int, Word)
import Prelude hiding (Bool, Char, Double, Float, Int, Word)

import Data.Coerce (Coercible, coerce)
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

import HsBindgen.Runtime.Internal.FFIType qualified as FFI
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
-- should be a sensible 'HasFFIType' instance. Instances are provided in
-- this module for most basic type constructors, like '(->)', 'IO', '()', and
-- all eligible types from the "Foreign" module hierarchy. However, we can't
-- magically generate instance for user-defined newtypes, nor do we try to
-- generate instances for all newtypes from the @base@ package or other core
-- packages. Instead, the user should derive such instances either using
-- newtype-deriving or using deriving-via with the 'ViaNewtype'\/'ViaCoercible'
-- helper datatype. Instances can otherwise not be defined by hand. Regardless
-- of the deriving method, the @UndecidableInstances@ language extension should
-- also be enabled.
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
  type ToFFIType a :: FFI.FFIType
  -- | Convert a foreign type to its FFI type.
  --
  -- See the 'HasFFIType' class for more information
  toFFIType :: a -> FFIType a
  -- | Convert an FFI type a foreign type.
  --
  -- See the 'HasFFIType' class for more information
  fromFFIType :: FFIType a -> a

type FFIType a = FromFFIType (ToFFIType a)

-- | Cast the foreign type inside the function pointer to its FFI type.
castFunPtrToFFIType ::
     forall a. HasFFIType a
  => FunPtr a
  -> FunPtr (FFIType a)
castFunPtrToFFIType = castFunPtr
  where
    -- NOTE: the constaint is unused, but we want to restrict what types can be
    -- cast, so we work around "unused constraint" warnings with this local
    -- definition.
    _unused = toFFIType @a

-- | Cast the FFI type inside a function pointer to a foreign type.
castFunPtrFromFFIType ::
     forall a. HasFFIType a
  => FunPtr (FFIType a)
  -> FunPtr a
castFunPtrFromFFIType = castFunPtr
  where
    -- NOTE: the constaint is unused, but we want to restrict what types can be
    -- cast, so we work around "unused constraint" warnings with this local
    -- definition.
    _unused = fromFFIType @a

type FromFFIType :: FFI.FFIType -> Type
type family FromFFIType ft where
  -- === Foreign types ===
  FromFFIType (FFI.FunArrow a b) = FromFFIType a -> FromFFIType b

  -- === Marshallable foreign result types ===
  FromFFIType FFI.Unit = ()
  FromFFIType (FFI.IO a) = IO (FromFFIType a)

  -- === Marshallable foreign types ===
  FromFFIType (FFI.Basic a) = FromBasicFFIType a

type FromBasicFFIType :: FFI.BasicFFIType -> Type
type family FromBasicFFIType ft where
  -- Prelude
  FromBasicFFIType FFI.Char   = Char
  FromBasicFFIType FFI.Int    = Int
  FromBasicFFIType FFI.Double = Double
  FromBasicFFIType FFI.Float  = Float
  FromBasicFFIType FFI.Bool   = Bool
  -- Data.Int
  FromBasicFFIType FFI.Int8  = Int8
  FromBasicFFIType FFI.Int16 = Int16
  FromBasicFFIType FFI.Int32 = Int32
  FromBasicFFIType FFI.Int64 = Int64
  -- Data.Word
  FromBasicFFIType FFI.Word   = Word
  FromBasicFFIType FFI.Word8  = Word8
  FromBasicFFIType FFI.Word16 = Word16
  FromBasicFFIType FFI.Word32 = Word32
  FromBasicFFIType FFI.Word64 = Word64
  -- Foreign.Ptr
  FromBasicFFIType FFI.Ptr     = Ptr Void
  FromBasicFFIType FFI.FunPtr  = FunPtr Void
  -- Foreign.StablePtr
  FromBasicFFIType FFI.StablePtr = StablePtr Void

{-------------------------------------------------------------------------------
  Deriving-via
-------------------------------------------------------------------------------}

-- === Via newtype ===

type ViaNewtype :: Type -> Type
newtype ViaNewtype a = ViaNewtype a

-- | This produces almost the same instance as you would get using @deriving
-- newtype@, but /this/ instance has explicit @INLINE@ pragmas.
instance HasFFIType a => HasFFIType (ViaNewtype a) where
  type ToFFIType (ViaNewtype a) = ToFFIType a
  {-# INLINE toFFIType #-}
  toFFIType (ViaNewtype x) = toFFIType x
  {-# INLINE fromFFIType #-}
  fromFFIType x = ViaNewtype (fromFFIType x)

type ViaCoercible :: Type -> Type -> Type
newtype ViaCoercible a b = ViaCoercible b

instance (Coercible a b, HasFFIType a) => HasFFIType (ViaCoercible a b) where
  type ToFFIType (ViaCoercible a b) = ToFFIType a
  {-# INLINE toFFIType #-}
  toFFIType (ViaCoercible x) = toFFIType (coerce @b @a x)
  {-# INLINE fromFFIType #-}
  fromFFIType x = ViaCoercible (coerce @a @b (fromFFIType x))

-- === Via an FFI type ===

type ViaFFIType :: k -> Type -> Type
newtype ViaFFIType k a = ViaFFIType a

instance FromFFIType ft ~ a => HasFFIType (ViaFFIType ft a) where
  type ToFFIType (ViaFFIType ft a) = ft
  {-# INLINE toFFIType #-}
  toFFIType (ViaFFIType x) = x
  {-# INLINE fromFFIType #-}
  fromFFIType x = ViaFFIType x

-- === Via a basic foreign type ===

type ViaBasicFFIType :: k -> Type -> Type
newtype ViaBasicFFIType k a = ViaBasicFFIType a

instance FromFFIType (FFI.Basic ft) ~ a => HasFFIType (ViaBasicFFIType ft a) where
  type ToFFIType (ViaBasicFFIType ft a) = FFI.Basic ft
  {-# INLINE toFFIType #-}
  toFFIType (ViaBasicFFIType x) = x
  {-# INLINE fromFFIType #-}
  fromFFIType x = ViaBasicFFIType x

{-------------------------------------------------------------------------------
  Foreign types
-------------------------------------------------------------------------------}

instance (HasFFIType a, HasFFIType b) => HasFFIType (a -> b) where
  type ToFFIType (a -> b) = FFI.FunArrow (ToFFIType a) (ToFFIType b)
  {-# INLINE toFFIType #-}
  toFFIType f = \x -> toFFIType (f $ fromFFIType x)
  {-# INLINE fromFFIType #-}
  fromFFIType f = \x -> fromFFIType (f $ toFFIType x)

{-------------------------------------------------------------------------------
  Marshallable foreign result types
-------------------------------------------------------------------------------}

deriving via ViaFFIType FFI.Unit () instance HasFFIType ()

instance HasFFIType a => HasFFIType (IO a) where
  type ToFFIType (IO ( a)) = FFI.IO (ToFFIType a)
  {-# INLINE toFFIType #-}
  toFFIType = fmap toFFIType
  {-# INLINE fromFFIType #-}
  fromFFIType = fmap fromFFIType

{-------------------------------------------------------------------------------
  Marshallable foreign types
-------------------------------------------------------------------------------}

-- === Prelude ===

-- == Basic foreign types ==

deriving via ViaBasicFFIType FFI.Char Char instance HasFFIType Char
deriving via ViaBasicFFIType FFI.Int Int instance HasFFIType Int
deriving via ViaBasicFFIType FFI.Double Double instance HasFFIType Double
deriving via ViaBasicFFIType FFI.Float Float instance HasFFIType Float
deriving via ViaBasicFFIType FFI.Bool Bool instance HasFFIType Bool

-- === Data.Int ===

-- == Basic foreign types ==

deriving via ViaBasicFFIType FFI.Int8 Int8 instance HasFFIType Int8
deriving via ViaBasicFFIType FFI.Int16 Int16 instance HasFFIType Int16
deriving via ViaBasicFFIType FFI.Int32 Int32 instance HasFFIType Int32
deriving via ViaBasicFFIType FFI.Int64 Int64 instance HasFFIType Int64

-- === Data.Word ===

-- == Basic foreign types ==

deriving via ViaBasicFFIType FFI.Word Word instance HasFFIType Word
deriving via ViaBasicFFIType FFI.Word8 Word8 instance HasFFIType Word8
deriving via ViaBasicFFIType FFI.Word16 Word16 instance HasFFIType Word16
deriving via ViaBasicFFIType FFI.Word32 Word32 instance HasFFIType Word32
deriving via ViaBasicFFIType FFI.Word64 Word64 instance HasFFIType Word64

-- === Foreign.Ptr ===

-- == Basic foreign types ==

instance HasFFIType (Ptr a) where
  type ToFFIType (Ptr a) = FFI.Basic FFI.Ptr
  {-# INLINE toFFIType #-}
  toFFIType = castPtr
  {-# INLINE fromFFIType #-}
  fromFFIType = castPtr

instance HasFFIType (FunPtr a) where
  type ToFFIType (FunPtr a) = FFI.Basic FFI.FunPtr
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
  type ToFFIType (StablePtr a) = FFI.Basic FFI.StablePtr
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
  type ToFFIType (PtrConst a) = FFI.Basic FFI.Ptr
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

