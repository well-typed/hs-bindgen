module C.Char
  ( CharValue(..)
  , charValueFromCodeUnit
  , charValueFromCodePoint
  , fromHaskellChar
  , utf8SingleByteCodeUnit
  ) where

-- base
import Data.Array.Byte
  ( ByteArray )
import Data.Bits
  ( Bits(..) )
import Data.Char
  ( ord, chr )
import Data.List
  ( unfoldr )
import Data.Maybe
  ( fromMaybe )
import Data.Word
  ( Word8, Word64 )
import GHC.Exts qualified as IsList
  ( IsList(..) )
import GHC.Generics
  ( Generic )

--------------------------------------------------------------------------------

-- | A character, represented both as a Unicode code point (if applicable)
-- and as the underlying character code units.
data CharValue
  = CharValue
    { charValue :: !ByteArray
      -- ^ The **code units** for a character: some bytes whose interpretation
      -- depends on the text encoding chosen.
    , unicodeCodePoint :: !( Maybe Char )
       -- ^ The (optional) **Unicode code point** for this character. This is
       -- an unsigned integer less than @0x10FF@ that uniquely identifies a
       -- Unicode character. For example, @永@ has the Unicode code point
       -- @0x6C38@ (also written @U+6C38@).
       --
       -- The representation of this code point as actual bits depends on the
       -- text encoding, e.g. the UTF-8 encoding of @永@ is @0xE6B0B8@
       -- (or @15118520@ in decimal notation).
    }
  deriving stock ( Eq, Ord, Show, Generic )

-- | Turn a Haskell 'Char' into a @C@ 'CharValue'.
fromHaskellChar :: Char -> CharValue
fromHaskellChar c =
  CharValue
    { charValue = IsList.fromList
                $ fromMaybe [ 0xEF, 0xBF, 0xBD ] -- Unicode replacement character
                $ utf8ByteCodeUnits ( fromIntegral ( ord c ) )
    , unicodeCodePoint = Just c
      -- A Haskell 'Char' value is precisely a Unicode code point.
    }


-- | Does this character fit into a single byte?
utf8SingleByteCodeUnit :: CharValue -> Maybe Word8
utf8SingleByteCodeUnit ( CharValue { charValue = ba } ) =
  case IsList.toList ba of
    [ b ] -> Just b
    _     -> Nothing

-- | Convert a number to a 'CharValue' as raw bytes.
charValueFromCodeUnit :: ( Integral i, Bits i ) => i -> CharValue
charValueFromCodeUnit u =
   CharValue
    { charValue = IsList.fromList $ bytes u
    , unicodeCodePoint = Nothing }
  where
    bytes = reverse . unfoldr step
    step 0 = Nothing
    step x = Just (fromIntegral (x .&. 0xFF), x `shiftR` 8)
{-# INLINEABLE charValueFromCodeUnit #-}

-- | Convert a numeric Unicode code point to a 'CharValue'.
charValueFromCodePoint :: ( Integral i, Bits i ) => i -> CharValue
charValueFromCodePoint i =
  CharValue
    { charValue = IsList.fromList
                $ fromMaybe [ 0xEF, 0xBF, 0xBD ] -- Unicode replacement character
                $ utf8ByteCodeUnits ( fromIntegral i )
    , unicodeCodePoint = Just $ chr $ fromIntegral i
    }

utf8ByteCodeUnits :: Word64 -> Maybe [ Word8 ]
utf8ByteCodeUnits p
  | p <= 0x7F
  = Just [ fromIntegral p ]
  | p <= 0x7FF
  = Just
      [ 0xC0 .|. ( fromIntegral ( p `shiftR` 6 ) .&. 0x1F )
      , 0X80 .|. ( fromIntegral   p              .&. 0X3F )
      ]
  | p <= 0xFFFF
  = Just
      [ 0xE0 .|. ( fromIntegral ( p `shiftR` 12 ) .&. 0x0F )
      , 0x80 .|. ( fromIntegral ( p `shiftR` 6  ) .&. 0x3F )
      , 0X80 .|. ( fromIntegral   p               .&. 0X3F )
      ]
  | p <= 0x10FFFF
  = Just
      [ 0xF0 .|. ( fromIntegral ( p `shiftR` 18 ) .&. 0x07 )
      , 0x80 .|. ( fromIntegral ( p `shiftR` 12 ) .&. 0x3F )
      , 0x80 .|. ( fromIntegral ( p `shiftR` 6  ) .&. 0x3F )
      , 0X80 .|. ( fromIntegral   p               .&. 0X3F )
      ]
  | otherwise
  = Nothing
