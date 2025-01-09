-- | Design patterns for writing high-level FFI bindings
--
-- This module is intended to be imported unqualified.
module HsBindgen.Runtime.Patterns (
    -- * Enums
    -- ** Simple
    SimpleEnum(..)
  , IsSimpleEnum(..)
  , SimpleEnumOutOfRange(..)
  , simpleEnum
  , coerceSimpleEnum
  , fromSimpleEnum
  , simpleEnumInRange
  , unsafeFromSimpleEnum
    -- ** Bitfield
  , BitfieldEnum(..)
  , IsSingleFlag(..)
   -- *** API
  , bitfieldEnum
  , fromBitfieldEnum
  , flagIsSet
    -- * Backtrace
  , Backtrace
  , collectBacktrace
  , prettyBacktrace
  , CollectedBacktrace(..)
    -- * Arithmetic
  , Div(..)
  ) where

import HsBindgen.Runtime.Patterns.Arithmetic
import HsBindgen.Runtime.Patterns.Backtrace
import HsBindgen.Runtime.Patterns.Enum.Bitfield
import HsBindgen.Runtime.Patterns.Enum.Simple
