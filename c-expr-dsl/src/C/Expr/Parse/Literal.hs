module C.Expr.Parse.Literal (
    IntSuffix(..)
  , parseLiteralInteger
  , parseLiteralFloating
  , parseLiteralChar
  , parseLiteralString
  ) where

import Control.Applicative (asum)
import Control.Monad
import Data.Char
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Scientific qualified as Scientific
import GHC.Exts qualified as IsList (IsList (..))
import GHC.Generics
import Numeric.Natural
import Text.Parsec
import Text.Parsec.Pos (updatePosChar)

import C.Char qualified as Runtime
import C.Expr.Parse.Infra
import C.Expr.Util.Parsec
import C.Type qualified as Runtime

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

-- | Re-parse a character literal.
--
-- Note that, in C, character literals have type @int@, **not** @char@!
parseLiteralChar :: TokenParser Runtime.CharValue
parseLiteralChar = do
  let forbidden = [ '\'' ]
  prefix <- parseCharPrefix
  void $ char '\''
  let charSequence = if charPrefixAllowsSequence prefix
                     then many1
                     else fmap (:[])
  chars <- charSequence $ choice [ nonEscapedChar forbidden, escapedChar ]
  void $ char '\''
  case prefix of
    Just {} ->
      -- TODO: support other prefixes.
      unexpected "unsupported character literal prefix"
    Nothing ->
      case chars of
        [c] -> return c
        _ ->
          -- NB (https://en.cppreference.com/w/c/language/character_constant):
          --
          -- Multicharacter constants were inherited by C from the B programming language.
          -- Although not specified by the C standard, most compilers (MSVC is a notable exception)
          -- implement multicharacter constants as specified in B: the values of each char
          -- in the constant initialize successive bytes of the resulting integer, in big-endian
          -- zero-padded right-adjusted order, e.g. the value of '\1' is 0x00000001
          -- and the value of '\1\2\3\4' is 0x01020304.
          case traverse Runtime.utf8SingleByteCodeUnit chars of
            Nothing ->
              unexpected "multi-character literal contains a character wider than a byte"
            Just bs
              | length bs > 4
              -> unexpected "multi-character literal contains more than 4 characters"
              | otherwise
              -> return $
                   Runtime.CharValue
                     { Runtime.charValue = IsList.fromList bs
                     , Runtime.unicodeCodePoint = Nothing
                     }

charPrefixAllowsSequence :: Maybe CharPrefix -> Bool
charPrefixAllowsSequence = \case
  Nothing -> True
  Just p ->
    case p of
      Prefix_u8 -> False
      Prefix_u  -> False -- removed in C23
      Prefix_U  -> False -- removed in C23
      Prefix_L  -> True

nonEscapedChar :: [Char] -> TokenParser Runtime.CharValue
nonEscapedChar forbidden =
  Runtime.fromHaskellChar <$>
    satisfy ( not . ( `elem` '\n' : '\\' : forbidden ) )

data CharPrefix = Prefix_u8 | Prefix_u | Prefix_U | Prefix_L

parseCharPrefix :: TokenParser ( Maybe CharPrefix )
parseCharPrefix = choice
  [ do { c 'u' ; c '8'; return (Just Prefix_u8) }
  , do { c 'u'; return (Just Prefix_u) }
  , do { c 'U'; return (Just Prefix_U) }
  , do { c 'L'; return (Just Prefix_L) }
  , return Nothing
  ]
  where
    c = void . char

escapedChar :: TokenParser Runtime.CharValue
escapedChar = do
  void $ char '\\'
  choice
    [ basicEscapedChar
    , hexCodeUnitChar
    , octalCodeUnitChar
    , universalEscapedChar
    ]

basicEscapedChar :: TokenParser Runtime.CharValue
basicEscapedChar =
  Runtime.fromHaskellChar <$>
    satisfyM ( `lookup` ( basicSourceEscapedChars ++ executionEscapedChars ) )

-- | Like 'satisfy' but takes a @Char -> Maybe a@ predicate.
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
executionEscapedChars :: [(Char, Char)]
executionEscapedChars =
  [ ( '0', '\NUL' ) -- null (actually just the octal escaped character \0)
  , ( 'a', '\a'   ) -- audible bell
  , ( 'b', '\b'   ) -- backspace
  , ( 'n', '\n'   ) -- line feed - new line
  , ( 'r', '\r'   ) -- carriage return
  ]

hexCodeUnitChar, octalCodeUnitChar :: TokenParser Runtime.CharValue
hexCodeUnitChar = do
  void $ char 'x'
  digs <- many1 (digitInBase False BaseHex)
  let codeUnit = readInBase BaseHex digs
  return $ Runtime.charValueFromCodeUnit codeUnit
octalCodeUnitChar = do
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
    codeUnit = readInBase BaseOct digs
  return $ Runtime.charValueFromCodeUnit codeUnit

universalEscapedChar :: TokenParser Runtime.CharValue
universalEscapedChar = do
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
     -> return $ Runtime.charValueFromCodePoint codePoint

{-------------------------------------------------------------------------------
  Parser for string literals

  Reference: <https://en.cppreference.com/w/c/language/string_literal>
-------------------------------------------------------------------------------}

-- | Re-parse a string literal.
parseLiteralString :: TokenParser [Runtime.CharValue]
parseLiteralString = do
  let forbidden = [ '\"' ]
  prefix <- parseCharPrefix
  void $ char '\"'
  cs <- many $ choice [ nonEscapedChar forbidden, escapedChar ]
  void $ char '\"'
  case prefix of
    Just {} ->
      -- TODO: support other prefixes.
      unexpected "unsupported string literal prefix"
    Nothing ->
      return cs
