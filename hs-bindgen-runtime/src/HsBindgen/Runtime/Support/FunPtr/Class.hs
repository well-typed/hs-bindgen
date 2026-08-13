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

-- | Useful for callbacks whose own type has no 'ToFunPtr' instance. Calls
-- 'withFunPtr' provided the callback is 'Coercible' to a signature @b@ that has one.
--
-- Most users will never need this: when bindings are generated with @hs-bindgen@,
-- 'ToFunPtr' and 'FromFunPtr' instances are generated for their function types.
--
-- The instances cover the raw C types, so @b@ is normally the same signature with your
-- own pointer tags and newtypes replaced by what they wrap:
--
-- @
-- data Node                     -- our own pointer tag
-- newtype Result = Result CInt  -- our own status type
--
-- onNode :: Ptr Node -> IO Result
--
-- -- Ptr Node -> IO Result has no instance; Ptr Void -> IO CInt does.
-- withFunPtrAs \@(Ptr Void -> IO CInt) onNode $ \\fp -> c_walk tree fp
-- @
--
withFunPtrAs ::
     forall b a r. (Coercible a b, ToFunPtr b)
  => a -> (Ptr.FunPtr b -> IO r) -> IO r
withFunPtrAs f = withFunPtr (coerce f :: b)
