-- | Design patterns for writing high-level FFI bindings
--
-- This is the only exported module in this library. It is intended to be
-- imported unqualified.
--
-- __NOTE__: This library is little more than an experiment in its current form,
-- with some patterns to support the FFI bindings that @hs-bindgen@ itself needs
-- (for the @libclang@ bindings).
module HsBindgen.Patterns (
    -- * Enums
    -- ** Simple
    SimpleEnum(..)
  , IsSimpleEnum(..)
  , simpleEnum
  , fromSimpleEnum
  , unsafeFromSimpleEnum
    -- ** Bitfield
  , BitfieldEnum(..)
  , IsSingleFlag(..)
   -- *** API
  , bitfieldEnum
  , fromBitfieldEnum
  , flagIsSet
    -- * Foreign pointers
  , SafeForeignPtr
  , AccessedFinalizedForeignPtrException
    -- ** API
  , newSafeForeignPtr
  , withSafeForeignPtr
  , finalizeSafeForeignPtr
    -- * Backtrace
  , Stack
  , getStack
  , prettyStack
  , ContainsStack(..)
  ) where

import HsBindgen.Patterns.Enum.Bitfield
import HsBindgen.Patterns.Enum.Simple
import HsBindgen.Patterns.SafeForeignPtr
import HsBindgen.Patterns.Stack
