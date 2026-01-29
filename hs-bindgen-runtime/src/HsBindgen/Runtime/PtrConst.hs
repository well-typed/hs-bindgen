{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Read-only pointers
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.Prelude
-- > import HsBindgne.Runtime.PtrConst qualified as PtrConst
module HsBindgen.Runtime.PtrConst (
    PtrConst -- type synonym or opaque, depending on version of @base@
  , peek
  , unsafeToPtr
  , unsafeFromPtr
    -- * Relationship with 'ConstPtr'
    -- $constptr
  ) where

#if MIN_VERSION_base(4,18,0)
import Foreign.C.ConstPtr (ConstPtr(..))
#endif

import Data.Kind (Type)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import Foreign.Storable qualified as F

-- | A read-only pointer.
--
-- A @'PtrConst' a@ is a pointer to a type @a@ with a C @const@ qualifier. For
-- instance, the Haskell type @PtrConst CInt@ is equivalent to the C type @const
-- int*@. @const@-qualified contents of a pointer should not be modified, but
-- reading the contents is okay.
type PtrConst :: Type -> Type
#if MIN_VERSION_base(4,18,0)
type PtrConst a = ConstPtr a
#else
type role PtrConst phantom
newtype PtrConst a = PtrConst { un :: Ptr a }
    deriving stock (Eq, Ord)
    deriving newtype Storable

-- doesn't use record syntax
instance Show (PtrConst a) where
    showsPrec d (PtrConst p) = showParen (d > 10) $ showString "PtrConst " . showsPrec 11 p
#endif

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

unPtrConst :: PtrConst a -> Ptr a
#if MIN_VERSION_base(4,18,0)
unPtrConst = unConstPtr
#else
unPtrConst = (.un)
#endif

mkPtrConst :: Ptr a -> PtrConst a
#if MIN_VERSION_base(4,18,0)
mkPtrConst = ConstPtr
#else
mkPtrConst = PtrConst
#endif

{-------------------------------------------------------------------------------
  Public
-------------------------------------------------------------------------------}

-- | Like 'F.peek'
peek :: Storable a => PtrConst a -> IO a
peek ptrc = F.peek (unPtrConst ptrc)

-- | Unsafe: convert a 'PtrConst' to a 'Ptr'.
--
-- NOTE: use the output pointer only with read access.
unsafeToPtr :: PtrConst a -> Ptr a
unsafeToPtr ptrc = unPtrConst ptrc

-- | Unsafe: convert a 'Ptr' to a 'PtrConst'.
--
-- NOTE: use the input pointer only with read access.
unsafeFromPtr :: Ptr a -> PtrConst a
unsafeFromPtr ptr = mkPtrConst ptr

{-------------------------------------------------------------------------------
  Relationship with 'ConstPtr'
-------------------------------------------------------------------------------}

{- $constptr

'PtrConst' is a pointer-to-const-data, but 'ConstPtr' is too. They are mostly
equivalent, so why a new type? For two main reasons:

1. 'ConstPtr' is actually a misnomer: it is a pointer-to-const-data, not a
   const-pointer-to-data.
2. 'ConstPtr' can be freely converted to a 'Ptr', even though its contents
   should be considered to be read-only.

'PtrConst' is arguably a better name, and it goes further in ensuring that
the pointer only has read access.

On @base-4.18@ and up, 'PtrConst' is a type synonym around 'ConstPtr', while on
earlier @base@ versions it is a custom, opaque datatype. If you care about
compatibility with all @base@ versions that @hs-bindgen-runtime@ support, then
you should treat 'PtrConst' as its own type distinct from 'ConstPtr' using only
the functions in the "HsBindgen.Runtime.PtrConst" module rather than the
"Foreign.C.ConstPtr" module.
-}

