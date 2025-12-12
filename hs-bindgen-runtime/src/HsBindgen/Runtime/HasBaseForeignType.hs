{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module HsBindgen.Runtime.HasBaseForeignType (
    -- * Class
    HasBaseForeignType
  , BaseForeignType
  , toBaseForeignType
  , fromBaseForeignType
  , castFunPtrToBaseForeignType
  , castFunPtrFromBaseForeignType
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

import HsBindgen.Runtime.BaseForeignType qualified as BFT
import HsBindgen.Runtime.ConstPtr as Types (ConstPtr (..))

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
--
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
class HasBaseForeignType a where
  type ToBaseForeignType a :: BFT.BaseForeignType
  toBaseType :: a -> BaseForeignType a
  fromBaseType :: BaseForeignType a -> a

type BaseForeignType a = FromBaseForeignType (ToBaseForeignType a)

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

-- | Cast the foreign type inside the function pointer to its base foreign type.
castFunPtrToBaseForeignType ::
     forall a. HasBaseForeignType a
  => FunPtr a
  -> FunPtr (BaseForeignType a)
castFunPtrToBaseForeignType = castFunPtr
  where
    -- NOTE: the constaint is unused, but we want to restrict what types can be
    -- cast, so we work around "unused constraint" warnings with this local
    -- definition.
    _unused = toBaseForeignType @a

-- | Cast the base foreign type inside a function pointer to a foreign type.
castFunPtrFromBaseForeignType ::
     forall a. HasBaseForeignType a
  => FunPtr (BaseForeignType a)
  -> FunPtr a
castFunPtrFromBaseForeignType = castFunPtr
  where
    -- NOTE: the constaint is unused, but we want to restrict what types can be
    -- cast, so we work around "unused constraint" warnings with this local
    -- definition.
    _unused = fromBaseForeignType @a

type FromBaseForeignType :: BFT.BaseForeignType -> Type
type family FromBaseForeignType ft where
  -- === Foreign types ===
  FromBaseForeignType (BFT.FunArrow a b) = FromBaseForeignType a -> FromBaseForeignType b

  -- === Marshallable foreign result types ===
  FromBaseForeignType BFT.Unit = ()
  FromBaseForeignType (BFT.IO a) = IO (FromBaseForeignType a)

  -- === Marshallable foreign types ===
  FromBaseForeignType (BFT.Basic a) = FromBasicForeignType a

type FromBasicForeignType :: BFT.BasicForeignType -> Type
type family FromBasicForeignType ft where
  -- Prelude
  FromBasicForeignType BFT.Char   = Char
  FromBasicForeignType BFT.Int    = Int
  FromBasicForeignType BFT.Double = Double
  FromBasicForeignType BFT.Float  = Float
  FromBasicForeignType BFT.Bool   = Bool
  -- Data.Int
  FromBasicForeignType BFT.Int8  = Int8
  FromBasicForeignType BFT.Int16 = Int16
  FromBasicForeignType BFT.Int32 = Int32
  FromBasicForeignType BFT.Int64 = Int64
  -- Data.Word
  FromBasicForeignType BFT.Word   = Word
  FromBasicForeignType BFT.Word8  = Word8
  FromBasicForeignType BFT.Word16 = Word16
  FromBasicForeignType BFT.Word32 = Word32
  FromBasicForeignType BFT.Word64 = Word64
  -- Foreign.Ptr
  FromBasicForeignType BFT.Ptr     = Ptr Void
  FromBasicForeignType BFT.FunPtr  = FunPtr Void
  FromBasicForeignType BFT.IntPtr  = IntPtr
  FromBasicForeignType BFT.WordPtr = WordPtr
  -- Foreign.StablePtr
  FromBasicForeignType BFT.StablePtr = StablePtr Void
  -- Foreign.C.ConstPtr
  FromBasicForeignType BFT.ConstPtr = ConstPtr Void
  -- Foreign.C.Types
  FromBasicForeignType BFT.CChar      = CChar
  FromBasicForeignType BFT.CSChar     = CSChar
  FromBasicForeignType BFT.CUChar     = CUChar
  FromBasicForeignType BFT.CShort     = CShort
  FromBasicForeignType BFT.CUShort    = CUShort
  FromBasicForeignType BFT.CInt       = CInt
  FromBasicForeignType BFT.CUInt      = CUInt
  FromBasicForeignType BFT.CLong      = CLong
  FromBasicForeignType BFT.CULong     = CULong
  FromBasicForeignType BFT.CPtrdiff   = CPtrdiff
  FromBasicForeignType BFT.CSize      = CSize
  FromBasicForeignType BFT.CWchar     = CWchar
  FromBasicForeignType BFT.CSigAtomic = CSigAtomic
  FromBasicForeignType BFT.CLLong     = CLLong
  FromBasicForeignType BFT.CULLong    = CULLong
  FromBasicForeignType BFT.CBool      = CBool
  FromBasicForeignType BFT.CIntPtr    = CIntPtr
  FromBasicForeignType BFT.CUIntPtr   = CUIntPtr
  FromBasicForeignType BFT.CIntMax    = CIntMax
  FromBasicForeignType BFT.CUIntMax   = CUIntMax
  -- Foreign.C.Types : Numeric types
  FromBasicForeignType BFT.CClock     = CClock
  FromBasicForeignType BFT.CTime      = CTime
  FromBasicForeignType BFT.CUSeconds  = CUSeconds
  FromBasicForeignType BFT.CSUSeconds = CSUSeconds
  -- Foreign.C.Types : Floating type
  FromBasicForeignType BFT.CFloat  = CFloat
  FromBasicForeignType BFT.CDouble = CDouble

{-------------------------------------------------------------------------------
  Deriving-via
-------------------------------------------------------------------------------}

-- === Via newtype ===

type ViaNewtype :: Type -> Type
newtype ViaNewtype a = ViaNewtype a

-- | This produces almost the same instance as you would get using @deriving
-- newtype@, but /this/ instance has explicit @INLINE@ pragmas.
instance HasBaseForeignType a => HasBaseForeignType (ViaNewtype a) where
  type ToBaseForeignType (ViaNewtype a) = ToBaseForeignType a
  {-# INLINE toBaseType #-}
  toBaseType (ViaNewtype x) = toBaseType x
  {-# INLINE fromBaseType #-}
  fromBaseType x = ViaNewtype (fromBaseType x)

type ViaCoercible :: Type -> Type -> Type
newtype ViaCoercible a b = ViaCoercible b

instance (Coercible a b, HasBaseForeignType a) => HasBaseForeignType (ViaCoercible a b) where
  type ToBaseForeignType (ViaCoercible a b) = ToBaseForeignType a
  {-# INLINE toBaseType #-}
  toBaseType (ViaCoercible x) = toBaseType (coerce @b @a x)
  {-# INLINE fromBaseType #-}
  fromBaseType x = ViaCoercible (coerce @a @b (fromBaseType x))

-- === Via a base foreign type ===

type ViaBaseForeignType :: k -> Type -> Type
newtype ViaBaseForeignType k a = ViaBaseForeignType a

instance FromBaseForeignType fat ~ a => HasBaseForeignType (ViaBaseForeignType fat a) where
  type ToBaseForeignType (ViaBaseForeignType fat a) = fat
  {-# INLINE toBaseType #-}
  toBaseType (ViaBaseForeignType x) = x
  {-# INLINE fromBaseType #-}
  fromBaseType x = ViaBaseForeignType x

-- === Via a basic foreign type ===

type ViaBasicForeignType :: k -> Type -> Type
newtype ViaBasicForeignType k a = ViaBasicForeignType a

instance FromBaseForeignType (BFT.Basic fat) ~ a => HasBaseForeignType (ViaBasicForeignType fat a) where
  type ToBaseForeignType (ViaBasicForeignType fat a) = BFT.Basic fat
  {-# INLINE toBaseType #-}
  toBaseType (ViaBasicForeignType x) = x
  {-# INLINE fromBaseType #-}
  fromBaseType x = ViaBasicForeignType x

{-------------------------------------------------------------------------------
  Foreign types
-------------------------------------------------------------------------------}

instance (HasBaseForeignType a, HasBaseForeignType b) => HasBaseForeignType (a -> b) where
  type ToBaseForeignType (a -> b) = BFT.FunArrow (ToBaseForeignType a) (ToBaseForeignType b)
  {-# INLINE toBaseType #-}
  toBaseType f = \x -> toBaseType (f $ fromBaseType x)
  {-# INLINE fromBaseType #-}
  fromBaseType f = \x -> fromBaseType (f $ toBaseType x)

{-------------------------------------------------------------------------------
  Marshallable foreign result types
-------------------------------------------------------------------------------}

deriving via ViaBaseForeignType BFT.Unit () instance HasBaseForeignType ()

instance HasBaseForeignType a => HasBaseForeignType (IO a) where
  type ToBaseForeignType (IO ( a)) = BFT.IO (ToBaseForeignType a)
  {-# INLINE toBaseType #-}
  toBaseType = fmap toBaseType
  {-# INLINE fromBaseType #-}
  fromBaseType = fmap fromBaseType

{-------------------------------------------------------------------------------
  Marshallable foreign types
-------------------------------------------------------------------------------}

-- NOTE: we use 'ViaBasicForeignType' rather than 'ViaNewtype' to derive
-- 'HasBaseForeignType' instances even for non-basic foreign types coming from the
-- "Foreign.C" modules. Most of these types, like 'CInt', are newtypes around
-- basic foreign types, but the specific basic foreign type depends on the
-- platform\/operating system.

-- === Prelude ===

-- == Basic foreign types ==

deriving via ViaBasicForeignType BFT.Char Char instance HasBaseForeignType Char
deriving via ViaBasicForeignType BFT.Int Int instance HasBaseForeignType Int
deriving via ViaBasicForeignType BFT.Double Double instance HasBaseForeignType Double
deriving via ViaBasicForeignType BFT.Float Float instance HasBaseForeignType Float
deriving via ViaBasicForeignType BFT.Bool Bool instance HasBaseForeignType Bool

-- === Data.Int ===

-- == Basic foreign types ==

deriving via ViaBasicForeignType BFT.Int8 Int8 instance HasBaseForeignType Int8
deriving via ViaBasicForeignType BFT.Int16 Int16 instance HasBaseForeignType Int16
deriving via ViaBasicForeignType BFT.Int32 Int32 instance HasBaseForeignType Int32
deriving via ViaBasicForeignType BFT.Int64 Int64 instance HasBaseForeignType Int64

-- === Data.Word ===

-- == Basic foreign types ==

deriving via ViaBasicForeignType BFT.Word Word instance HasBaseForeignType Word
deriving via ViaBasicForeignType BFT.Word8 Word8 instance HasBaseForeignType Word8
deriving via ViaBasicForeignType BFT.Word16 Word16 instance HasBaseForeignType Word16
deriving via ViaBasicForeignType BFT.Word32 Word32 instance HasBaseForeignType Word32
deriving via ViaBasicForeignType BFT.Word64 Word64 instance HasBaseForeignType Word64

-- === Foreign.Ptr ===

-- == Basic foreign types ==

instance HasBaseForeignType (Ptr a) where
  type ToBaseForeignType (Ptr a) = BFT.Basic BFT.Ptr
  {-# INLINE toBaseType #-}
  toBaseType = castPtr
  {-# INLINE fromBaseType #-}
  fromBaseType = castPtr

instance HasBaseForeignType (FunPtr a) where
  type ToBaseForeignType (FunPtr a) = BFT.Basic BFT.FunPtr
  {-# INLINE toBaseType #-}
  toBaseType = castFunPtr
  {-# INLINE fromBaseType #-}
  fromBaseType = castFunPtr

-- == Newtypes around basic foreign types ==

deriving via ViaBasicForeignType BFT.IntPtr IntPtr instance HasBaseForeignType IntPtr
deriving via ViaBasicForeignType BFT.WordPtr WordPtr instance HasBaseForeignType WordPtr

-- === Foreign.StablePtr ===

-- == Basic foreign types ==

instance HasBaseForeignType (StablePtr a) where
  type ToBaseForeignType (StablePtr a) = BFT.Basic BFT.StablePtr
  {-# INLINE toBaseType #-}
  toBaseType = castStablePtr
  {-# INLINE fromBaseType #-}
  fromBaseType = castStablePtr

{-# INLINE castStablePtr #-}
castStablePtr :: StablePtr a -> StablePtr b
castStablePtr = castPtrToStablePtr . castStablePtrToPtr

-- === Foreign.C.ConstPtr ===

-- == Newtypes around basic foreign types ==

instance HasBaseForeignType (ConstPtr a) where
  type ToBaseForeignType (ConstPtr a) = BFT.Basic BFT.ConstPtr
  {-# INLINE toBaseType #-}
  toBaseType = coerce castPtr
  {-# INLINE fromBaseType #-}
  fromBaseType = coerce castPtr

-- === Foreign.C.Error ===

-- == Newtypes around basic foreign types ==

deriving via ViaNewtype CInt instance HasBaseForeignType Errno

-- === Foreign.C.Types ===

-- == Newtypes around basic foreign types ==

deriving via ViaBasicForeignType BFT.CChar CChar instance HasBaseForeignType CChar
deriving via ViaBasicForeignType BFT.CSChar CSChar instance HasBaseForeignType CSChar
deriving via ViaBasicForeignType BFT.CUChar CUChar instance HasBaseForeignType CUChar
deriving via ViaBasicForeignType BFT.CShort CShort instance HasBaseForeignType CShort
deriving via ViaBasicForeignType BFT.CUShort CUShort instance HasBaseForeignType CUShort
deriving via ViaBasicForeignType BFT.CInt CInt instance HasBaseForeignType CInt
deriving via ViaBasicForeignType BFT.CUInt CUInt instance HasBaseForeignType CUInt
deriving via ViaBasicForeignType BFT.CLong CLong instance HasBaseForeignType CLong
deriving via ViaBasicForeignType BFT.CULong CULong instance HasBaseForeignType CULong
deriving via ViaBasicForeignType BFT.CPtrdiff CPtrdiff instance HasBaseForeignType CPtrdiff
deriving via ViaBasicForeignType BFT.CSize CSize instance HasBaseForeignType CSize
deriving via ViaBasicForeignType BFT.CWchar CWchar instance HasBaseForeignType CWchar
deriving via ViaBasicForeignType BFT.CSigAtomic CSigAtomic instance HasBaseForeignType CSigAtomic
deriving via ViaBasicForeignType BFT.CLLong CLLong instance HasBaseForeignType CLLong
deriving via ViaBasicForeignType BFT.CULLong CULLong instance HasBaseForeignType CULLong
deriving via ViaBasicForeignType BFT.CBool CBool instance HasBaseForeignType CBool
deriving via ViaBasicForeignType BFT.CIntPtr CIntPtr instance HasBaseForeignType CIntPtr
deriving via ViaBasicForeignType BFT.CUIntPtr CUIntPtr instance HasBaseForeignType CUIntPtr
deriving via ViaBasicForeignType BFT.CIntMax CIntMax instance HasBaseForeignType CIntMax
deriving via ViaBasicForeignType BFT.CUIntMax CUIntMax instance HasBaseForeignType CUIntMax

-- === Foreign.C.Types : Numeric types ===

-- == Newtypes around basic foreign types ==

deriving via ViaBasicForeignType BFT.CClock CClock instance HasBaseForeignType CClock
deriving via ViaBasicForeignType BFT.CTime CTime instance HasBaseForeignType CTime
deriving via ViaBasicForeignType BFT.CUSeconds CUSeconds instance HasBaseForeignType CUSeconds
deriving via ViaBasicForeignType BFT.CSUSeconds CSUSeconds instance HasBaseForeignType CSUSeconds

-- === Foreign.C.Types : Floating types ===

-- == Newtypes around basic foreign types ==

deriving via ViaBasicForeignType BFT.CFloat CFloat instance HasBaseForeignType CFloat
deriving via ViaBasicForeignType BFT.CDouble CDouble instance HasBaseForeignType CDouble

