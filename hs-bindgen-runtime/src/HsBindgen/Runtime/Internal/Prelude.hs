{-# OPTIONS_HADDOCK hide #-}

-- | Prelude required by generated bindings.
--
-- This module ensures compatibility across GHC versions and @base@ library
-- versions. It re-exports most Haskell definitions that are used by the
-- generated bindings.
--
-- See https://github.com/well-typed/hs-bindgen/issues/1627.
--
-- We maintain minimal lists of explicit imports and exports.
module HsBindgen.Runtime.Internal.Prelude (
    -- * Function pointers
    ToFunPtr(toFunPtr)
  , FromFunPtr(fromFunPtr)

    -- * Foreign function interface
  , Ptr(Ptr)
  , FunPtr
  , plusPtr
  , StablePtr
  , ConstantArray
  , IncompleteArray
  , CharValue(CharValue)
  , charValueFromAddr
  , with
  , allocaAndPeek
  , Generic
  ) where


import Foreign (with)
import GHC.Generics (Generic)
import GHC.Ptr (FunPtr, Ptr (Ptr), plusPtr)
import GHC.Stable (StablePtr)

import C.Char (CharValue (CharValue), charValueFromAddr)

import HsBindgen.Runtime.ConstantArray (ConstantArray)
import HsBindgen.Runtime.IncompleteArray (IncompleteArray)
import HsBindgen.Runtime.Internal.CAPI (allocaAndPeek)
import HsBindgen.Runtime.Internal.FunPtr (FromFunPtr (fromFunPtr),
                                          ToFunPtr (toFunPtr))
