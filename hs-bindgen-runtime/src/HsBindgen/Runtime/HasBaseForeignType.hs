{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- TODO: finish documentation, including a manual section

module HsBindgen.Runtime.HasBaseForeignType (
    -- * Class
    HasBaseForeignType
  , BaseForeignType
  , toBaseForeignType
  , fromBaseForeignType
    -- * Deriving-via
  , ViaNewtype(..)
    -- * Re-exports
    --
    -- These re-exports include all types that the
    -- "HsBindgen.Runtime.HasBaseForeignType" module provides
    -- 'HasBaseForeignType' instances for.
    --
    -- TODO: replace by a catch-all import in @hs-bindgen@?
  , module Reexports
  ) where

#if MIN_VERSION_base(4,17,0)
import Prelude (type (~))
#endif

import Prelude (Show, Eq, IO, ($), fmap, (.))
import Prelude as Reexports (Char, Int, Double, Float, Bool, Word)
import Data.Int as Reexports (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Void (Void)
import Data.Word as Reexports (Word16, Word32, Word64, Word8)
import Foreign.C.Error as Reexports  (Errno (..))
import Foreign.C.Types as Reexports (CBool (..), CChar (..), CClock (..), CDouble (..),
                        CFloat (..), CInt (..), CIntMax (..), CIntPtr (..),
                        CLLong (..), CLong (..), CPtrdiff (..), CSChar (..),
                        CSUSeconds (..), CShort (..), CSigAtomic (..),
                        CSize (..), CTime (..), CUChar (..), CUInt (..),
                        CUIntMax (..), CUIntPtr (..), CULLong (..), CULong (..),
                        CUSeconds (..), CUShort (..), CWchar (..))
import Foreign.Ptr (castFunPtr, castPtr)
import Foreign.Ptr as Reexports (FunPtr, IntPtr (..), Ptr, WordPtr (..))
import Foreign.StablePtr (castPtrToStablePtr, castStablePtrToPtr)
import Foreign.StablePtr as Reexports (StablePtr)

#if MIN_VERSION_base(4, 18, 0)
import Data.Coerce (coerce)
import Foreign.C.ConstPtr as Reexports (ConstPtr (..))
#endif

{-------------------------------------------------------------------------------
  Class
-------------------------------------------------------------------------------}

-- | The 'HasBaseForeignType' class broadly captures Haskell types that can be
-- converted to and from a /base foreign type/.
--
-- A base foreign type is similar to a /foreign type/, but with all newtypes
-- removed. Foreign types are the kinds of types that are allowed in @foreign
-- import@ declarations.
--
-- Some laws apply to this class:
--
-- * If @x :: a@ is a foreign type, then @toBaseForeignType x :: BaseForeignType
--   a@ is also a valid foreign type and contains no newtypes.
-- * If @x :: BaseForeignType a@ is a foreign type, then @fromBaseForeignType x
--   :: a@ is also a valid foreign type.
--
-- Note in particular that this does /not/ guarantee that:
-- * Every type @a@ that is an instance of 'HasBaseForeignType' is a valid
-- foreign type
-- * Every type @'BaseForeignType' a@ is a valid foreign type.
--
-- Informally, 'toBaseForeignType' and 'fromBaseForeignType' preserve
-- /valid-foreign-type-ness/.
--
-- === User-supplied instances
--
-- Generally as a rule of thumb, if @a@ is a valid foreign type, then there
-- should be a sensible 'HasBaseForeignType' instance. Instances are provided in
-- this module for most basic type constructors, like '(->)', 'IO', '()', and
-- all eligible types from the "Foreign" module hierarchy. However, we can't
-- magically generate instance for user-defined newtypes, nor do we try to
-- generate instances for all newtypes from the @base@ package or other core
-- packages. Instead, the user should derive such instances either using
-- newtype-deriving or using deriving-via with the 'ViaNewtype' helper datatype.
-- Regardless of the deriving method, the @UndecidableInstances@ language
-- extension should also be enabled.
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
class HasBaseForeignType a where
  type ToBaseForeignKind a :: BaseForeignKind
  toBaseType :: a -> BaseForeignType a
  fromBaseType :: BaseForeignType a -> a

type BaseForeignType a = FromBaseForeignKind (ToBaseForeignKind a)

{-# INLINE toBaseForeignType #-}
-- | Convert a foreign type to its base foreign type.
--
-- See the 'HasBaseForeignType' class for more information
toBaseForeignType :: HasBaseForeignType a => a -> BaseForeignType a
toBaseForeignType = toBaseType

{-# INLINE fromBaseForeignType #-}
-- | Convert a base foreign type to a foreign type.
--
-- See the 'HasBaseForeignType' class for more information
fromBaseForeignType :: HasBaseForeignType a => BaseForeignType a -> a
fromBaseForeignType = fromBaseType

data BaseForeignKind =
    -- === Foreign types ===
    FT_FunArrow BaseForeignKind BaseForeignKind

    -- === Marshallable foreign result types ===
  | FRT_Unit
  | FRT_IO BaseForeignKind

    -- === Marshallable foreign types ===
    -- Prelude
  | FAT_Char
  | FAT_Int
  | FAT_Double
  | FAT_Float
  | FAT_Bool
    -- Data.Int
  | FAT_Int8
  | FAT_Int16
  | FAT_Int32
  | FAT_Int64
    -- Data.Word
  | FAT_Word
  | FAT_Word8
  | FAT_Word16
  | FAT_Word32
  | FAT_Word64
    -- Foreign.Ptr
  | FAT_Ptr
  | FAT_FunPtr
  | FAT_IntPtr
  | FAT_WordPtr
    -- Foreign.StablePtr
  | FAT_StablePtr
#if MIN_VERSION_base(4, 18, 0)
    -- Foreign.C.ConstPtr
  | FAT_ConstPtr
#endif
    -- Foreign.C.Types
  | FAT_CChar
  | FAT_CSChar
  | FAT_CUChar
  | FAT_CShort
  | FAT_CUShort
  | FAT_CInt
  | FAT_CUInt
  | FAT_CLong
  | FAT_CULong
  | FAT_CPtrdiff
  | FAT_CSize
  | FAT_CWchar
  | FAT_CSigAtomic
  | FAT_CLLong
  | FAT_CULLong
  | FAT_CBool
  | FAT_CIntPtr
  | FAT_CUIntPtr
  | FAT_CIntMax
  | FAT_CUIntMax
    -- Foreign.C.Types : Numeric types
  | FAT_CClock
  | FAT_CTime
  | FAT_CUSeconds
  | FAT_CSUSeconds
    -- Foreign.C.Types : Floating type
  | FAT_CFloat
  | FAT_CDouble
  deriving stock (Show, Eq)

type FromBaseForeignKind :: BaseForeignKind -> Type
type family FromBaseForeignKind ft where
  -- === Foreign types ===
  FromBaseForeignKind (FT_FunArrow a b) = FromBaseForeignKind a -> FromBaseForeignKind b

  -- === Marshallable foreign result types ===
  FromBaseForeignKind FRT_Unit = ()
  FromBaseForeignKind (FRT_IO a) = IO (FromBaseForeignKind a)

  -- === Marshallable foreign types ===
  -- Prelude
  FromBaseForeignKind FAT_Char   = Char
  FromBaseForeignKind FAT_Int    = Int
  FromBaseForeignKind FAT_Double = Double
  FromBaseForeignKind FAT_Float  = Float
  FromBaseForeignKind FAT_Bool   = Bool
  -- Data.Int
  FromBaseForeignKind FAT_Int8  = Int8
  FromBaseForeignKind FAT_Int16 = Int16
  FromBaseForeignKind FAT_Int32 = Int32
  FromBaseForeignKind FAT_Int64 = Int64
  -- Data.Word
  FromBaseForeignKind FAT_Word   = Word
  FromBaseForeignKind FAT_Word8  = Word8
  FromBaseForeignKind FAT_Word16 = Word16
  FromBaseForeignKind FAT_Word32 = Word32
  FromBaseForeignKind FAT_Word64 = Word64
  -- Foreign.Ptr
  FromBaseForeignKind FAT_Ptr     = Ptr Void
  FromBaseForeignKind FAT_FunPtr  = FunPtr Void
  FromBaseForeignKind FAT_IntPtr  = IntPtr
  FromBaseForeignKind FAT_WordPtr = WordPtr
  -- Foreign.StablePtr
  FromBaseForeignKind FAT_StablePtr = StablePtr Void
#if MIN_VERSION_base(4, 18, 0)
  -- Foreign.C.ConstPtr
  FromBaseForeignKind FAT_ConstPtr = ConstPtr Void
#endif
  -- Foreign.C.Types
  FromBaseForeignKind FAT_CChar      = CChar
  FromBaseForeignKind FAT_CSChar     = CSChar
  FromBaseForeignKind FAT_CUChar     = CUChar
  FromBaseForeignKind FAT_CShort     = CShort
  FromBaseForeignKind FAT_CUShort    = CUShort
  FromBaseForeignKind FAT_CInt       = CInt
  FromBaseForeignKind FAT_CUInt      = CUInt
  FromBaseForeignKind FAT_CLong      = CLong
  FromBaseForeignKind FAT_CULong     = CULong
  FromBaseForeignKind FAT_CPtrdiff   = CPtrdiff
  FromBaseForeignKind FAT_CSize      = CSize
  FromBaseForeignKind FAT_CWchar     = CWchar
  FromBaseForeignKind FAT_CSigAtomic = CSigAtomic
  FromBaseForeignKind FAT_CLLong     = CLLong
  FromBaseForeignKind FAT_CULLong    = CULLong
  FromBaseForeignKind FAT_CBool      = CBool
  FromBaseForeignKind FAT_CIntPtr    = CIntPtr
  FromBaseForeignKind FAT_CUIntPtr   = CUIntPtr
  FromBaseForeignKind FAT_CIntMax    = CIntMax
  FromBaseForeignKind FAT_CUIntMax   = CUIntMax
  -- Foreign.C.Types : Numeric types
  FromBaseForeignKind FAT_CClock     = CClock
  FromBaseForeignKind FAT_CTime      = CTime
  FromBaseForeignKind FAT_CUSeconds  = CUSeconds
  FromBaseForeignKind FAT_CSUSeconds = CSUSeconds
  -- Foreign.C.Types : Floating type
  FromBaseForeignKind FAT_CFloat  = CFloat
  FromBaseForeignKind FAT_CDouble = CDouble

{-------------------------------------------------------------------------------
  Deriving-via
-------------------------------------------------------------------------------}

-- === Via newtype ===

type ViaNewtype :: Type -> Type
newtype ViaNewtype a = ViaNewtype a

-- | This produces almost the same instance as you would get using @deriving
-- newtype@, but /this/ instance has explicit @INLINE@ pragmas.
instance HasBaseForeignType a => HasBaseForeignType (ViaNewtype a) where
  type ToBaseForeignKind (ViaNewtype a) = ToBaseForeignKind a
  {-# INLINE toBaseType #-}
  toBaseType (ViaNewtype x) = toBaseType x
  {-# INLINE fromBaseType #-}
  fromBaseType x = ViaNewtype (fromBaseType x)

-- === Via a base type ===

type ViaBaseForeignKind :: k -> Type -> Type
newtype ViaBaseForeignKind k a = ViaBaseForeignKind a

instance FromBaseForeignKind fat ~ a => HasBaseForeignType (ViaBaseForeignKind fat a) where
  type ToBaseForeignKind (ViaBaseForeignKind fat a) = fat
  {-# INLINE toBaseType #-}
  toBaseType (ViaBaseForeignKind x) = x
  {-# INLINE fromBaseType #-}
  fromBaseType x = ViaBaseForeignKind x

{-------------------------------------------------------------------------------
  Foreign types
-------------------------------------------------------------------------------}

instance (HasBaseForeignType a, HasBaseForeignType b) => HasBaseForeignType (a -> b) where
  type ToBaseForeignKind (a -> b) = FT_FunArrow (ToBaseForeignKind a) (ToBaseForeignKind b)
  {-# INLINE toBaseType #-}
  toBaseType f = \x -> toBaseType (f $ fromBaseType x)
  {-# INLINE fromBaseType #-}
  fromBaseType f = \x -> fromBaseType (f $ toBaseType x)

{-------------------------------------------------------------------------------
  Marshallable foreign result types
-------------------------------------------------------------------------------}

deriving via ViaBaseForeignKind FRT_Unit () instance HasBaseForeignType ()

instance HasBaseForeignType a => HasBaseForeignType (IO a) where
  type ToBaseForeignKind (IO ( a)) = FRT_IO (ToBaseForeignKind a)
  {-# INLINE toBaseType #-}
  toBaseType = fmap toBaseType
  {-# INLINE fromBaseType #-}
  fromBaseType = fmap fromBaseType

{-------------------------------------------------------------------------------
  Marshallable foreign types
-------------------------------------------------------------------------------}

-- NOTE: we use 'ViaBaseForeignKind' rather than 'ViaNewtype' to derive
-- 'HasBaseForeignType' instances even for non-basic foreign types coming from the
-- "Foreign.C" modules. Most of these types, like 'CInt', are newtypes around
-- basic foreign types, but the specific basic foreign type depends on the
-- platform\/operating system.

-- === Prelude ===

-- == Basic foreign types ==

deriving via ViaBaseForeignKind FAT_Char Char instance HasBaseForeignType Char
deriving via ViaBaseForeignKind FAT_Int Int instance HasBaseForeignType Int
deriving via ViaBaseForeignKind FAT_Double Double instance HasBaseForeignType Double
deriving via ViaBaseForeignKind FAT_Float Float instance HasBaseForeignType Float
deriving via ViaBaseForeignKind FAT_Bool Bool instance HasBaseForeignType Bool

-- === Data.Int ===

-- == Basic foreign types ==

deriving via ViaBaseForeignKind FAT_Int8 Int8 instance HasBaseForeignType Int8
deriving via ViaBaseForeignKind FAT_Int16 Int16 instance HasBaseForeignType Int16
deriving via ViaBaseForeignKind FAT_Int32 Int32 instance HasBaseForeignType Int32
deriving via ViaBaseForeignKind FAT_Int64 Int64 instance HasBaseForeignType Int64

-- === Data.Word ===

-- == Basic foreign types ==

deriving via ViaBaseForeignKind FAT_Word Word instance HasBaseForeignType Word
deriving via ViaBaseForeignKind FAT_Word8 Word8 instance HasBaseForeignType Word8
deriving via ViaBaseForeignKind FAT_Word16 Word16 instance HasBaseForeignType Word16
deriving via ViaBaseForeignKind FAT_Word32 Word32 instance HasBaseForeignType Word32
deriving via ViaBaseForeignKind FAT_Word64 Word64 instance HasBaseForeignType Word64

-- === Foreign.Ptr ===

-- == Basic foreign types ==

instance HasBaseForeignType (Ptr a) where
  type ToBaseForeignKind (Ptr a) = FAT_Ptr
  {-# INLINE toBaseType #-}
  toBaseType = castPtr
  {-# INLINE fromBaseType #-}
  fromBaseType = castPtr

instance HasBaseForeignType (FunPtr a) where
  type ToBaseForeignKind (FunPtr a) = FAT_FunPtr
  {-# INLINE toBaseType #-}
  toBaseType = castFunPtr
  {-# INLINE fromBaseType #-}
  fromBaseType = castFunPtr

-- == Newtypes around basic foreign types ==

deriving via ViaBaseForeignKind FAT_IntPtr IntPtr instance HasBaseForeignType IntPtr
deriving via ViaBaseForeignKind FAT_WordPtr WordPtr instance HasBaseForeignType WordPtr

-- === Foreign.StablePtr ===

-- == Basic foreign types ==

instance HasBaseForeignType (StablePtr a) where
  type ToBaseForeignKind (StablePtr a) = FAT_StablePtr
  {-# INLINE toBaseType #-}
  toBaseType = castStablePtr
  {-# INLINE fromBaseType #-}
  fromBaseType = castStablePtr

{-# INLINE castStablePtr #-}
castStablePtr :: StablePtr a -> StablePtr b
castStablePtr = castPtrToStablePtr . castStablePtrToPtr

#if MIN_VERSION_base(4, 18, 0)
-- === Foreign.C.ConstPtr ===

-- == Newtypes around basic foreign types ==

instance HasBaseForeignType (ConstPtr a) where
  type ToBaseForeignKind (ConstPtr a) = FAT_ConstPtr
  {-# INLINE toBaseType #-}
  toBaseType = coerce castPtr
  {-# INLINE fromBaseType #-}
  fromBaseType = coerce castPtr
#endif

-- === Foreign.C.Error ===

-- == Newtypes around basic foreign types ==

deriving via ViaNewtype CInt instance HasBaseForeignType Errno

-- === Foreign.C.Types ===

-- == Newtypes around basic foreign types ==

deriving via ViaBaseForeignKind FAT_CChar CChar instance HasBaseForeignType CChar
deriving via ViaBaseForeignKind FAT_CSChar CSChar instance HasBaseForeignType CSChar
deriving via ViaBaseForeignKind FAT_CUChar CUChar instance HasBaseForeignType CUChar
deriving via ViaBaseForeignKind FAT_CShort CShort instance HasBaseForeignType CShort
deriving via ViaBaseForeignKind FAT_CUShort CUShort instance HasBaseForeignType CUShort
deriving via ViaBaseForeignKind FAT_CInt CInt instance HasBaseForeignType CInt
deriving via ViaBaseForeignKind FAT_CUInt CUInt instance HasBaseForeignType CUInt
deriving via ViaBaseForeignKind FAT_CLong CLong instance HasBaseForeignType CLong
deriving via ViaBaseForeignKind FAT_CULong CULong instance HasBaseForeignType CULong
deriving via ViaBaseForeignKind FAT_CPtrdiff CPtrdiff instance HasBaseForeignType CPtrdiff
deriving via ViaBaseForeignKind FAT_CSize CSize instance HasBaseForeignType CSize
deriving via ViaBaseForeignKind FAT_CWchar CWchar instance HasBaseForeignType CWchar
deriving via ViaBaseForeignKind FAT_CSigAtomic CSigAtomic instance HasBaseForeignType CSigAtomic
deriving via ViaBaseForeignKind FAT_CLLong CLLong instance HasBaseForeignType CLLong
deriving via ViaBaseForeignKind FAT_CULLong CULLong instance HasBaseForeignType CULLong
deriving via ViaBaseForeignKind FAT_CBool CBool instance HasBaseForeignType CBool
deriving via ViaBaseForeignKind FAT_CIntPtr CIntPtr instance HasBaseForeignType CIntPtr
deriving via ViaBaseForeignKind FAT_CUIntPtr CUIntPtr instance HasBaseForeignType CUIntPtr
deriving via ViaBaseForeignKind FAT_CIntMax CIntMax instance HasBaseForeignType CIntMax
deriving via ViaBaseForeignKind FAT_CUIntMax CUIntMax instance HasBaseForeignType CUIntMax

-- === Foreign.C.Types : Numeric types ===

-- == Newtypes around basic foreign types ==

deriving via ViaBaseForeignKind FAT_CClock CClock instance HasBaseForeignType CClock
deriving via ViaBaseForeignKind FAT_CTime CTime instance HasBaseForeignType CTime
deriving via ViaBaseForeignKind FAT_CUSeconds CUSeconds instance HasBaseForeignType CUSeconds
deriving via ViaBaseForeignKind FAT_CSUSeconds CSUSeconds instance HasBaseForeignType CSUSeconds

-- === Foreign.C.Types : Floating types ===

-- == Newtypes around basic foreign types ==

deriving via ViaBaseForeignKind FAT_CFloat CFloat instance HasBaseForeignType CFloat
deriving via ViaBaseForeignKind FAT_CDouble CDouble instance HasBaseForeignType CDouble

