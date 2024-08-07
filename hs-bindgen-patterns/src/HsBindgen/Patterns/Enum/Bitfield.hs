module HsBindgen.Patterns.Enum.Bitfield (
    BitfieldEnum(..)
  , IsSingleFlag(..)
    -- * API
  , bitfieldEnum
  , fromBitfieldEnum
  , flagIsSet
  ) where

import Foreign.C
import Data.Foldable qualified as Foldable
import Data.Bits

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Single flags
--
-- See 'BitfieldEnum' for discussion.
class IsSingleFlag flag where
  flagToC :: flag -> CUInt

-- | Enum that corresponds to a bitfield
--
-- Some C enumerations are defined like this:
--
-- > enum Flags {
-- >   Flag1 = 0x00,
-- >   Flag2 = 0x01,
-- >   Flag3 = 0x02,
-- >   Flag4 = 0x04,
-- >   Flag5 = 0x08,
-- >   ..
-- > };
--
-- The intention then is that these flags are ORed together to select multiple
-- flags. We term this a "bitfield enum": the @flag@ type is intended to be an
-- ADT with a 'IsSingleFlag' instance, mapping ADT constructors to the values from
-- the enum. Using @hsc2hs@, such an instance might look like
--
-- > data Flags = Flag1 | Flag2 | Flag3 | Flag 4 | Flag5
-- >
-- > instance IsSingleFlag Flags where
-- >   flagToC Flag1 = #const Flag1
-- >   flagToC Flag2 = #const Flag2
-- >   flagToC Flag3 = #const Flag3
-- >   flagToC Flag4 = #const Flag4
-- >   flagToC Flag5 = #const Flag5
newtype BitfieldEnum flag = BitfieldEnum CUInt

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Construct 'BitfieldEnum'
bitfieldEnum :: IsSingleFlag flag => [flag] -> BitfieldEnum flag
bitfieldEnum = BitfieldEnum . Foldable.foldl' (.|.) 0 . map flagToC

-- | Check if the given flag is set
flagIsSet :: IsSingleFlag flag => BitfieldEnum flag -> flag -> Bool
flagIsSet (BitfieldEnum i) flag = (i .&. flagToC flag) /= 0

-- | All set flags
--
-- This is @O(n)@ in the number of constructs of the @flag@ ADT; while that is
-- technically speaking a constant, making this function @O(1)@, this is still
-- a relatively expensive function. Consider using 'flagIsSet' instead.
--
-- NOTE: The @Enum@ and @Bounded@ instances are simply used to enumerate all
-- flags. Their definition has no bearing on the generated C code, and can
-- simply be derived.
fromBitfieldEnum ::
     (IsSingleFlag flag, Enum flag, Bounded flag)
  => BitfieldEnum flag -> [flag]
fromBitfieldEnum i = [flag | flag <- [minBound .. maxBound], flagIsSet i flag]