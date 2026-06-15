module C.Expr.Parse.Literal (
    IntSuffix(..)
  , parseLiteralInteger
  , parseLiteralFloating
  , parseLiteralChar
  , parseLiteralString
  ) where

import Control.Applicative (asum)
import Control.Monad (replicateM, void)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BSL
import Data.Char (chr, ord, toLower)
import Data.List (unfoldr)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Scientific qualified as Scientific
import Data.Word (Word8)
import Foreign.C (CChar)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Text.Parsec (ParsecT, Stream, char, choice, many, many1, option,
                    optionMaybe, satisfy, tokenPrim, try, unexpected)
import Text.Parsec.Pos (updatePosChar)

import C.Type qualified as Runtime

import C.Expr.Parse.Infra (TokenParser)
import C.Expr.Util.Parsec (caseInsensitive', satisfyWith)

{-------------------------------------------------------------------------------
  Parser for integer literals

  Reference: <https://en.cppreference.com/w/cpp/language/integer_literal>
-------------------------------------------------------------------------------}

data IntSuffix =
    IntSuffixUnsigned
  | IntSuffixLong
  | IntSuffixLongLong
  | IntSuffixSize
  deriving stock (Eq, Show, Generic)

intSuffix :: TokenParser IntSuffix
intSuffix = choice [
      IntSuffixUnsigned <$ caseInsensitive' "u"
    , IntSuffixLongLong <$ caseInsensitive' "ll"
    , IntSuffixLong     <$ caseInsensitive' "l"
    , IntSuffixSize     <$ caseInsensitive' "z"
    ]

parseLiteralInteger :: TokenParser (Integer, Runtime.IntLikeType)
parseLiteralInteger = do
    (b, ds, suffixes) <- aux

    let val = readInBase b ds

        ty = case suffixes of
          [] -> Runtime.Int Runtime.Signed
          _  ->
            let sign = if any ( == IntSuffixUnsigned ) suffixes
                       then Runtime.Unsigned
                       else Runtime.Signed
                long     = any ( == IntSuffixLong ) suffixes
                longlong = any ( == IntSuffixLongLong ) suffixes
             in
                if | longlong
                   -> Runtime.LongLong sign
                   | long
                   -> Runtime.Long sign
                   | otherwise
                   -> Runtime.Int sign

    return (fromIntegral val, ty)
  where
    aux :: TokenParser (Base, [Digit], [IntSuffix])
    aux = asum [
          try $ do
            b  <- base
            ds <- many1 $ digitInBase True b
            ss <- many intSuffix
            return (b, ds, ss)
        , do
            let b = BaseDec
            ds <- many1 $ digitInBase True b
            ss <- many intSuffix
            return (b, ds, ss)
        ]

readInBase :: Base -> [Digit] -> Natural
readInBase b ds =
  let
    multipliers = iterate (* baseToNat b) 1
  in
    sum $ zipWith (*) (reverse $ mapMaybe getDigit ds) multipliers

{-------------------------------------------------------------------------------
  Parser for floating-point literals

  Reference: <https://en.cppreference.com/w/cpp/language/floating_literal>
-------------------------------------------------------------------------------}

parseLiteralFloating :: TokenParser (Float, Double, Runtime.FloatingType)
parseLiteralFloating = do

  b     <- option BaseDec (BaseHex <$ caseInsensitive' "0x")
  as    <- many (digitInBase True b)
  mbXs  <- optionMaybe $ do { void (char '.') ; many (digitInBase True b) }
  mbExp <-
    case b of
      -- Exponent is non-optional with hexadecimal base
      BaseHex -> Just <$> parseExponent b
      _       -> optionMaybe (parseExponent b)

  if
    | Nothing <- mbXs
    , Nothing <- mbExp
    -> unexpected $ "cannot parse floating-point value: expected either '.' or '" ++ exponentText b ++ "'"
    | null as
    , case mbXs of { Nothing -> True; Just [] -> True; _ -> False }
    -> unexpected $ "cannot parse floating-point value without any digits"
    | otherwise
    -> do ty <- choice
            [ Runtime.FloatType <$ caseInsensitive' "f"
            , do { void $ caseInsensitive' "l"
                 ; unexpected "no support for long double literals"
                 }
            , pure Runtime.DoubleType
            ]
          let m :: Natural
              m = readInBase b (as ++ fromMaybe [] mbXs)
              e :: Int
              e = fromMaybe 0 mbExp - maybe 0 length mbXs
          return (fromScientific m e, fromScientific m e, ty)

fromScientific :: forall a. RealFloat a => Natural -> Int -> a
fromScientific m e =
  Scientific.toRealFloat $ Scientific.scientific (fromIntegral m) e

parseExponent :: Base -> TokenParser Int
parseExponent b = do
  void (caseInsensitive' $ exponentText b)
  s <- parseSign
  ds <- many (digitInBase True BaseDec)
  return $ applySign s (fromIntegral $ readInBase BaseDec ds)

exponentText :: Base -> String
exponentText BaseHex = "p"
exponentText _ = "e"

data Sign = Neg | Pos
  deriving stock ( Eq, Ord, Show )

parseSign :: TokenParser Sign
parseSign = choice
  [ Pos <$ char '+'
  , Neg <$ char '-'
  , return Pos
  ]

applySign :: Num a => Sign -> a -> a
applySign Neg x = negate x
applySign Pos x = x

{-------------------------------------------------------------------------------
  Auxiliary: integer representations in different bases
-------------------------------------------------------------------------------}

data Base =
    BaseDec
  | BaseOct
  | BaseHex
  | BaseBin
  deriving stock (Show)

baseToNat :: Base -> Natural
baseToNat BaseDec = 10
baseToNat BaseOct = 8
baseToNat BaseBin = 2
baseToNat BaseHex = 16

-- | Digit in a given base
data Digit = Digit Natural | Separator
  deriving stock (Show)

getDigit :: Digit -> Maybe Natural
getDigit (Digit i) = Just i
getDigit Separator = Nothing

base :: TokenParser Base
base = choice [
      BaseHex <$ caseInsensitive' "0x"
    , BaseBin <$ caseInsensitive' "0b"
    , BaseOct <$ caseInsensitive' "0"
    ]

-- | Parse digit in the given base
--
-- Returns the value of the digit, of 'Nothing' for single quotes
-- (which are allowed as a separator between digits).
digitInBase :: Bool -> Base -> TokenParser Digit
digitInBase allowSeparator = satisfyWith . (. toLower) . aux
  where
    aux :: Base -> Char -> Maybe Digit
    aux BaseDec c
      | c >= '0' && c <= '9' = Just . Digit $ c `relativeTo` '0'
    aux BaseBin c
      | c >= '0' && c <= '1' = Just . Digit $ c `relativeTo` '0'
    aux BaseOct c
      | c >= '0' && c <= '7' = Just . Digit $ c `relativeTo` '0'
    aux BaseHex c
      | c >= '0' && c <= '9' = Just . Digit $ c `relativeTo` '0'
      | c >= 'a' && c <= 'f' = Just . Digit $ c `relativeTo` 'a' + 10

    aux _ '\''
      | allowSeparator
      = Just $ Separator
    aux _ _
      = Nothing

    relativeTo :: Char -> Char -> Natural
    relativeTo c r = fromIntegral $ ord c - ord r

digitChar :: Digit -> Maybe Char
digitChar (Digit i)
  | i < 0
  = Nothing
  | i <= 9
  = Just $ chr (ord '0' + fromIntegral i)
  | i <= 35
  = Just $ chr (ord 'A' + fromIntegral i - 10)
  | otherwise
  = Nothing
digitChar Separator = Nothing

{-------------------------------------------------------------------------------
  Parser for character literals

  Reference: <https://en.cppreference.com/w/c/language/character_constant>
-------------------------------------------------------------------------------}

-- | Re-parse a character literal into a single byte.
--
-- Only single-byte character literals are supported: the result is the byte
-- value that a C compiler would assign to the literal.  This is the value
-- embedded in the generated Haskell binding, which may be passed directly to
-- C code expecting that same byte.
--
-- The libclang token text is a 'String' of Unicode code points (UTF-8-decoded).
-- We parse the C escape syntax and verify the resulting code point fits in one
-- byte (see 'parseLiteralString' for the analogous three-layer approach).
--
-- Note that, in C, character literals have type @int@, **not** @char@!
--
-- We reject wide literals (@L@\/@u@\/@U@\/@u8@ prefix), multi-character
-- constants, code points above @0xFF@, and numeric escapes that overflow a byte.
parseLiteralChar :: TokenParser CChar
parseLiteralChar = do
  prefix <- parseCharPrefix
  case prefix of
    Just {} ->
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/2039>
      -- Support prefixed character literals.
      unexpected "wide character literals are not supported"
    Nothing -> do
      void $ char '\''
      chars <- many1 $ choice [nonEscapedChar ['\''], escapedChar]
      void $ char '\''
      case chars of
        [c] ->
          if ord c <= 0xFF
            then return (fromIntegral (ord c))
            else unexpected "character literal value does not fit in a byte"
        _ ->
          unexpected "multi-character literal"

-- | Parse a single unescaped source character.
nonEscapedChar :: [Char] -> TokenParser Char
nonEscapedChar forbidden =
  satisfy ( not . ( `elem` '\n' : '\\' : forbidden ) )

data CharPrefix = Prefix_u8 | Prefix_u | Prefix_U | Prefix_L

parseCharPrefix :: TokenParser ( Maybe CharPrefix )
parseCharPrefix = choice
  [ do { c 'u' ; c '8'; return (Just Prefix_u8) }
  , do { c 'u';         return (Just Prefix_u) }
  , do { c 'U';         return (Just Prefix_U) }
  , do { c 'L';         return (Just Prefix_L) }
  , return Nothing
  ]
  where
    c = void . char

-- | Parse a single escaped character.
--
-- Numeric escapes that do not fit in a single byte are rejected, as they have
-- an implementation-defined value in C (see 'parseLiteralChar').
escapedChar :: TokenParser Char
escapedChar = do
  void $ char '\\'
  choice
    [ basicEscapedChar
    , numericCodeUnit hexCodeUnit
    , numericCodeUnit octalCodeUnit
    , chr . fromIntegral <$> universalCodePoint
    ]
  where
    numericCodeUnit p = do
      codeUnit <- p
      if codeUnit <= 0xFF
        then return $ chr ( fromIntegral codeUnit )
        else unexpected "character literal with implementation-defined value"

basicEscapedChar :: TokenParser Char
basicEscapedChar =
  satisfyM ( `lookup` ( basicSourceEscapedChars ++ executionEscapedChars ) )

-- | Like 'Text.Parsec.satisfy' but takes a @Char -> Maybe a@ predicate.
satisfyM :: Stream s m Char => (Char -> Maybe a) -> ParsecT s u m a
{-# INLINABLE satisfyM #-}
satisfyM f = tokenPrim
  (\c -> show [c])
  (\pos c _cs -> updatePosChar pos c)
  (\c -> f c)

-- | Escape sequences of basic (source) characters.
--
-- See https://en.cppreference.com/w/c/language/charset.
basicSourceEscapedChars :: [(Char, Char)]
basicSourceEscapedChars =
  [ ( '\'', '\'' )  -- single quote
  , ( '\"', '\"' )  -- double quote
  , ( '?' , '?'  )  -- question mark
  , ( '\\', '\\' )  -- backslash
  , ( 'f' , '\f'  ) -- form feed - new page
  , ( 't' , '\t'  ) -- horizontal tab
  , ( 'v' , '\v'  ) -- vertical tab
  ]

-- | Escape sequences of execution characters.
--
-- See https://en.cppreference.com/w/c/language/charset.
--
-- Note: @\\0@ is NOT listed here. It is an octal escape (value 0) and is
-- handled by 'octalCodeUnit'. Listing it here would cause @\\00@ / @\\000@
-- to be mis-parsed: the named escape would consume only the first @0@, leaving
-- the remaining digits to be interpreted as ordinary characters.
executionEscapedChars :: [(Char, Char)]
executionEscapedChars =
  [ ( 'a', '\a'   ) -- audible bell
  , ( 'b', '\b'   ) -- backspace
  , ( 'n', '\n'   ) -- line feed - new line
  , ( 'r', '\r'   ) -- carriage return
  ]

-- | Parse the value of a hexadecimal escape sequence (@\\xH...@).
hexCodeUnit :: TokenParser Natural
hexCodeUnit = do
  void $ char 'x'
  digs <- many1 (digitInBase False BaseHex)
  return $ readInBase BaseHex digs

-- | Parse the value of an octal escape sequence (@\\N@, @\\NN@ or @\\NNN@).
octalCodeUnit :: TokenParser Natural
octalCodeUnit = do
  -- NB (https://en.cppreference.com/w/c/language/escape):
  --
  -- Octal escape sequences have a length limit of three octal digits,
  -- but terminate at the first character that is not a valid octal digit
  -- if encountered sooner.
  dig1 <- digitInBase False BaseOct
  dig2 <- option Nothing (Just <$> digitInBase False BaseOct)
  dig3 <- option Nothing (Just <$> digitInBase False BaseOct)
  let
    digs :: [Digit]
    digs = dig1 : catMaybes [dig2, dig3]
  return $ readInBase BaseOct digs

-- | Parse and validate a universal character name (@\\uHHHH@ or
-- @\\UHHHHHHHH@), returning its Unicode code point.
universalCodePoint :: TokenParser Natural
universalCodePoint = do
  nbChars <- choice [4 <$ char 'u', 8 <$ char 'U']
  digs <- replicateM nbChars (digitInBase False BaseHex)
  let codePoint = readInBase BaseHex digs
      showCodePoint = mapMaybe digitChar digs

  -- See 'Range of universal character names' in https://en.cppreference.com/w/c/language/escape
  if | codePoint < 0xA0 && not (codePoint `elem` [0x24, 0x40, 0x60]) -- '$', '@', '`'
     -> unexpected $ "universal character names cannot refer to basic characters (" ++ showCodePoint ++ ")"
     | codePoint >= 0xD800 && codePoint < 0xDFFF
     -> unexpected $ "universal character names cannot refer to surrogate code points (" ++ showCodePoint ++ ")"
     | codePoint >= 0x10FFFF
     -> unexpected $ "universal character name is not a valid Unicode code point (" ++ showCodePoint ++ ")"
     | otherwise
     -> return codePoint

{-------------------------------------------------------------------------------
  Parser for string literals

  Reference: <https://en.cppreference.com/w/c/language/string_literal>
-------------------------------------------------------------------------------}

-- | Re-parse a string literal into its execution-encoding bytes.
--
-- The result is /bit-for-bit accurate/: the returned 'ByteString' contains
-- exactly the bytes that a C compiler targeting a UTF-8 execution charset would
-- store for this literal.  This matters because the generated Haskell bindings
-- may be passed directly to C functions that expect that same byte sequence.
--
-- The libclang token text is a 'String' of Unicode code points (UTF-8-decoded).
-- We apply a three-layer translation, e.g. for @\"你\\x41\"@:
--
--   1. /C source/ (UTF-8): raw byte sequence from the source file.
--   2. /Decoded/ ('String'): @['你', '\\\\', 'x', '4', '1']@ between the quotes.
--   3. /Execution encoding/ ('ByteString'): @[\<UTF-8 for 你\>, 0x41]@
--
-- Plain code points (including @\\uXXXX@) are UTF-8-encoded.  Numeric escapes
-- (@\\xNN@, @\\NNN@) contribute their raw code-unit bytes directly — they are
-- /not/ re-encoded as UTF-8.  This is what makes the output bit-for-bit
-- accurate: @\"\\xE3\\x81\\x82\"@ and @\"あ\"@ both yield the same three
-- bytes @[0xE3, 0x81, 0x82]@.
parseLiteralString :: TokenParser ByteString
parseLiteralString = do
  prefix <- parseCharPrefix
  case prefix of
    Just {} ->
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/2039>
      -- Support prefixed string literals.
      unexpected "unsupported string literal prefix"
    Nothing -> do
      void $ char '\"'
      cs <- many $ choice [nonEscapedByte ['\"'], escapedByte]
      void $ char '\"'
      return $ BS.pack $ concat cs

-- | Parse a single unescaped source character, as its UTF-8 bytes.
nonEscapedByte :: [Char] -> TokenParser [Word8]
nonEscapedByte forbidden = utf8EncodeChar <$> nonEscapedChar forbidden

-- | Parse a single escaped character, as its UTF-8 bytes.
escapedByte :: TokenParser [Word8]
escapedByte = do
  void $ char '\\'
  choice
    [ utf8EncodeChar      <$> basicEscapedChar
    , codeUnitBytes       <$> hexCodeUnit
    , codeUnitBytes       <$> octalCodeUnit
    , utf8EncodeCodePoint <$> universalCodePoint
    ]

{-------------------------------------------------------------------------------
  Character encoding
-------------------------------------------------------------------------------}

-- | UTF-8-encode a 'Char'.
utf8EncodeChar :: Char -> [Word8]
utf8EncodeChar = BSL.unpack . Builder.toLazyByteString . Builder.charUtf8

-- | UTF-8-encode a Unicode code point.
--
-- The code point must be valid (@<= 0x10FFFF@); 'universalCodePoint' is the
-- only producer and validates this.
utf8EncodeCodePoint :: Natural -> [Word8]
utf8EncodeCodePoint = utf8EncodeChar . chr . fromIntegral

-- | The big-endian bytes of a numeric code unit value, e.g. the value of
-- @\\1\\2\\3\\4@ is @0x01020304@. The zero value produces a single null byte.
--
-- This is C numeric-escape semantics, not a UTF-8 encoding.
codeUnitBytes :: Natural -> [Word8]
codeUnitBytes 0 = [0]
codeUnitBytes n = reverse . unfoldr step $ n
  where
    step 0 = Nothing
    step x = Just (fromIntegral (x .&. 0xFF), x `shiftR` 8)
