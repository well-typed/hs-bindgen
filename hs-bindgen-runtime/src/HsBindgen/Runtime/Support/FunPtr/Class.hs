{-# OPTIONS_HADDOCK hide #-}

-- | Function pointer utilities and type class for converting Haskell functions
-- to C function pointers.
--
-- This module provides a type class 'ToFunPtr' that allows for a uniform
-- interface to convert Haskell functions to C function pointers.
module HsBindgen.Runtime.Support.FunPtr.Class (
    -- * Type class
    ToFunPtr(..)
  , FromFunPtr(..)

    -- * Utilities
  , withFunPtr
  , withFunPtrAs
  ) where

import Control.Exception (bracket)
import Data.Coerce (Coercible, coerce)
import Foreign qualified as F
import GHC.Ptr qualified as Ptr

-- | Type class for converting Haskell functions to C function pointers.
--
class ToFunPtr a where
  -- | Convert a Haskell function to a C function pointer.
  --
  -- The caller is responsible for freeing the function pointer using
  -- 'F.freeHaskellFunPtr' when it is no longer needed.
  --
  toFunPtr :: a -> IO (F.FunPtr a)

-- | Type class for converting C function pointers to Haskell functions.
--
class FromFunPtr a where
  -- | Convert C function pointer into a Haskell function.
  fromFunPtr :: F.FunPtr a -> a

-- | This function makes sure that 'F.freeHaskellFunPtr' is called after
-- 'toFunPtr' has allocated memory for a 'Ptr.FunPtr'.
--
withFunPtr :: ToFunPtr a => a -> (Ptr.FunPtr a -> IO b) -> IO b
withFunPtr x = bracket (toFunPtr x) F.freeHaskellFunPtr

-- | 'withFunPtr' for a callback whose own domain types are not covered by a
-- 'ToFunPtr' instance, but which is 'Coercible' to a signature @b@ that is. The
-- function is retagged with 'coerce' (zero-cost: a @'F.Ptr' a@ is phantom in @a@,
-- and a @newtype@ over a covered type coerces to it) onto @b@, whose 'F.FunPtr' the
-- C import takes.
--
-- hs-bindgen emits a 'ToFunPtr' instance per generated callback, so a generated
-- binding uses 'withFunPtr' at the domain type directly; reach for this only for a
-- hand-written signature the generated set does not cover, applying @b@ with a type
-- application: @withFunPtrAs \@CoveredSig domainFn@.
--
withFunPtrAs ::
     forall b a r. (Coercible a b, ToFunPtr b)
  => a -> (Ptr.FunPtr b -> IO r) -> IO r
withFunPtrAs f = withFunPtr (coerce f :: b)
