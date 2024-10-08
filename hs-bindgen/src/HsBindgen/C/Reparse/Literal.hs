module HsBindgen.C.Reparse.Literal (
    reparseLiteralInteger
  ) where

import Data.Char (toLower, ord)
import Data.Maybe (mapMaybe)
import Text.Parsec

import HsBindgen.C.Reparse.Infra
import HsBindgen.Util.Parsec

{-------------------------------------------------------------------------------
  Parser for integer literals

  Reference: <https://en.cppreference.com/w/cpp/language/integer_literal>
-------------------------------------------------------------------------------}

data IntSuffix =
    IntSuffixUnsigned
  | IntSuffixLong
  | IntSuffixLongLong
  | IntSuffixSize
  deriving stock (Show)

intSuffix :: TokenParser IntSuffix
intSuffix = choice [
      IntSuffixUnsigned <$ caseInsensitive' "u"
    , IntSuffixLongLong <$ caseInsensitive' "ll"
    , IntSuffixLong     <$ caseInsensitive' "l"
    , IntSuffixSize     <$ caseInsensitive' "z"
    ]

reparseLiteralInteger :: TokenParser Integer
reparseLiteralInteger = do
    -- TODO: Should we keep the suffices around?
    (b, ds, _suffixes) <- aux

    let multipliers :: [Integer]
        multipliers = iterate (* baseToInt b) 1

    return $ sum $ zipWith (*) (reverse $ mapMaybe getDigit ds) multipliers
  where
    aux :: TokenParser (Base, [Digit], [IntSuffix])
    aux = do
        b  <- base
        ds <- many1 $ digitInBase b
        ss <- many intSuffix
        return (b, ds, ss)

{-------------------------------------------------------------------------------
  Auxiliary: integer representations in different bases
-------------------------------------------------------------------------------}

data Base =
    BaseDec
  | BaseOct
  | BaseHex
  | BaseBin
  deriving stock (Show)

baseToInt :: Base -> Integer
baseToInt BaseDec = 10
baseToInt BaseOct = 8
baseToInt BaseBin = 2
baseToInt BaseHex = 16

-- | Digit in a given base
data Digit = Digit Integer | Separator
  deriving stock (Show)

getDigit :: Digit -> Maybe Integer
getDigit (Digit i) = Just i
getDigit Separator = Nothing

base :: TokenParser Base
base = choice [
      BaseHex <$ caseInsensitive' "0x"
    , BaseBin <$ caseInsensitive' "0b"
    , BaseOct <$ caseInsensitive' "0"
    , BaseDec <$ lookAhead (satisfy $ \c -> c >= '1' && c <= '9')
    ]

-- | Parse digit in the given base
--
-- Returns the value of the digit, of 'Nothing' for single quotes
-- (which are allowed as a separator between digits).
digitInBase :: Base -> TokenParser Digit
digitInBase = satisfyWith . (. toLower) . aux
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

    aux _ '\'' = Just $ Separator
    aux _ _    = Nothing

    relativeTo :: Char -> Char -> Integer
    relativeTo c r = fromIntegral $ ord c - ord r
