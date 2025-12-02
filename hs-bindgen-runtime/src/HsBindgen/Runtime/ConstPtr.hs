{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Foreign.C.ConstPtr
-- Copyright   :  (c) GHC Developers
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides typed @const@ pointers to foreign data. It is part
-- of the Foreign Function Interface (FFI).
--
-- NOTE: this is a copy of the "Foreign.C.ConstPtr" module from the @base@
-- package, added here because versions of @base@ before @4.18@ do not provide a
-- 'ConstPtr' type. When the @base@ version is @4.18@ or higher, the
-- "Foreign.C.ConstPtr" is re-exported instead. Licenses and copyrights for the
-- "Foreign.C.ConstPtr" module apply to the current module.
--
-----------------------------------------------------------------------------

module HsBindgen.Runtime.ConstPtr (
    ConstPtr(..)
) where

#if MIN_VERSION_base(4,18,0)

import Foreign.C.ConstPtr (ConstPtr(..))

#else

import Data.Kind (Type)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)

-- | A pointer with the C @const@ qualifier. For instance, an argument of type
-- @ConstPtr CInt@ would be marshalled as @const int*@.
--
-- While @const@-ness generally does not matter for @ccall@ imports (since
-- @const@ and non-@const@ pointers typically have equivalent calling
-- conventions), it does matter for @capi@ imports. See GHC #22043.
--
-- @since base-4.18.0.0
--
type ConstPtr :: Type -> Type
type role ConstPtr phantom
newtype ConstPtr a = ConstPtr { unConstPtr :: Ptr a }
    deriving stock (Eq, Ord)
    deriving newtype Storable

-- doesn't use record syntax
instance Show (ConstPtr a) where
    showsPrec d (ConstPtr p) = showParen (d > 10) $ showString "ConstPtr " . showsPrec 11 p

#endif
