{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Function pointer utilities and type class for converting Haskell functions
-- to C function pointers.
--
-- This module provides a type class 'ToFunPtr' that allows for a uniform
-- interface to convert Haskell functions to C function pointers.
--
module HsBindgen.Runtime.FunPtr.Class (
    -- * Type class
    ToFunPtr(..)
  , FromFunPtr(..)

    -- * Utilities
  , withFunPtr

  ) where

import Control.Exception (bracket)
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

-- | This function makes sure that 'freeHaskellFunPtr' is called after
-- 'toFunPtr' has allocated memory for a 'FunPtr'.
--
withFunPtr :: ToFunPtr a => a -> (Ptr.FunPtr a -> IO b) -> IO b
withFunPtr x = bracket (toFunPtr x) F.freeHaskellFunPtr
