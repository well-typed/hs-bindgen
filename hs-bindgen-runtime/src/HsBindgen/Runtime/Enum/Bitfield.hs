module HsBindgen.Runtime.Enum.Bitfield (
    BitfieldEnum(..)
  , IsSingleFlag(..)
    -- * API
  , bitfieldEnum
  , fromBitfieldEnum
  , flagIsSet
  ) where

import Data.Bits
import Data.Foldable qualified as Foldable
import Data.Typeable
import Foreign.C
import GHC.Generics (Generic)
import GHC.Show (appPrec1, showSpace)
import Text.Show.Pretty (PrettyVal(..))
import Text.Show.Pretty qualified as Pretty

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Single flags
--
-- See 'BitfieldEnum' for discussion.
class Typeable hs => IsSingleFlag hs where
  flagToC :: hs -> CUInt

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
newtype BitfieldEnum hs = BitfieldEnum CUInt
  deriving stock (Eq, Ord, Generic)

{-------------------------------------------------------------------------------
  Showing values
-------------------------------------------------------------------------------}

instance (IsSingleFlag hs, Enum hs, Bounded hs, Show hs)
      => Show (BitfieldEnum hs) where
  showsPrec p i = showParen (p >= appPrec1) $
      either (uncurry showC) showHs $ showBitfieldEnum i
    where
      showC :: CUInt -> TypeRep -> ShowS
      showC c typ =
            showString "BitfieldEnum @"
          . showsPrec appPrec1 typ
          . showSpace
          . showsPrec appPrec1 c

      showHs :: [hs] -> ShowS
      showHs hs =
             showString "simpleEnum "
           . showsPrec appPrec1 hs

instance (IsSingleFlag hs, Enum hs, Bounded hs, PrettyVal hs)
      => PrettyVal (BitfieldEnum hs) where
  prettyVal =
      either (uncurry showC) showHs . showBitfieldEnum
    where
      showC :: CUInt -> TypeRep -> Pretty.Value
      showC c typ = Pretty.Con "BitfieldEnum" [
            Pretty.Con ("@" ++ show typ) []
          , prettyVal (fromIntegral c :: Int)
          ]

      showHs :: [hs] -> Pretty.Value
      showHs hs = Pretty.Con "bitfieldEnum" [
            prettyVal hs
          ]

-- | Internal auxiliary for showing 'SimpleEnum'
showBitfieldEnum :: forall hs.
     (IsSingleFlag hs, Enum hs, Bounded hs)
  => BitfieldEnum hs -> Either (CUInt, TypeRep) [hs]
showBitfieldEnum =
    either (Left . showC) Right . fromBitfieldEnum
  where
    showC :: CUInt -> (CUInt, TypeRep)
    showC c = (c, typeRep (Proxy @hs))

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Construct 'BitfieldEnum'
bitfieldEnum :: IsSingleFlag hs => [hs] -> BitfieldEnum hs
bitfieldEnum = BitfieldEnum . Foldable.foldl' (.|.) 0 . map flagToC

-- | Check if the given flag is set
flagIsSet :: IsSingleFlag hs => BitfieldEnum hs -> hs -> Bool
flagIsSet (BitfieldEnum i) flag = (i .&. flagToC flag) /= 0

-- | All set flags
--
-- This is @O(n)@ in the number of constructs of the @flag@ ADT; while that is
-- technically speaking a constant, making this function @O(1)@, this is still
-- a relatively expensive function. Consider using 'flagIsSet' instead.
--
-- Returns a 'Left' value if some bits in the enum did not correspond to any
-- known @hs@ flag.
--
-- NOTE: The @Enum@ and @Bounded@ instances are simply used to enumerate all
-- flags. Their definition has no bearing on the generated C code, and can
-- simply be derived.
fromBitfieldEnum :: forall hs.
     (IsSingleFlag hs, Enum hs, Bounded hs)
  => BitfieldEnum hs -> Either CUInt [hs]
fromBitfieldEnum i@(BitfieldEnum c)
  | bitfieldEnum allRecognized == i
  = Right allRecognized

  | otherwise
  = Left c
  where
    allRecognized :: [hs]
    allRecognized = [flag | flag <- [minBound .. maxBound], flagIsSet i flag]
