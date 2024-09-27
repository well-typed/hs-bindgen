module HsBindgen.C.AST.Literal (
    Literal(..)
  , parseLiteralInt
  ) where

import Data.Char (toLower, ord)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Parsec
import Text.Show.Pretty (PrettyVal)

import HsBindgen.Util.Parsec

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Literal
data Literal a = Literal {
      -- | The representation of the literal in the original source
      --
      -- We include this to generate better bindings and better documentation.
      -- For example, flags specified in hexadecimal would become quite
      -- unreadable in decimal.
      literalText  :: Text

      -- | The (parsed) valeu of the literal
    , literalValue :: a
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

parseLiteral :: Parser a -> Text -> Either ParseError (Literal a)
parseLiteral p src = parse (Literal src <$> p <* eof) "" src

parseLiteralInt :: Text -> Either ParseError (Literal Integer)
parseLiteralInt = parseLiteral parseIntLitAux

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

parseIntSuffix :: Parser IntSuffix
parseIntSuffix = choice [
      IntSuffixUnsigned <$ caseInsensitive' "u"
    , IntSuffixLongLong <$ caseInsensitive' "ll"
    , IntSuffixLong     <$ caseInsensitive' "l"
    , IntSuffixSize     <$ caseInsensitive' "z"
    ]

parseIntLitAux :: Parser Integer
parseIntLitAux = do
    -- TODO: Should we keep the suffices around?
    (base, digits, _suffixes) <- aux

    let multipliers :: [Integer]
        multipliers = iterate (* baseToInt base) 1

    return $ sum $ zipWith (*) (reverse $ mapMaybe getDigit digits) multipliers
  where
    aux :: Parser (Base, [Digit], [IntSuffix])
    aux = do
        base     <- parseBase
        digits   <- many1 $ parseDigitInBase base
        suffixes <- many parseIntSuffix
        return (base, digits, suffixes)

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

parseBase :: Parser Base
parseBase = choice [
      BaseHex <$ caseInsensitive' "0x"
    , BaseBin <$ caseInsensitive' "0b"
    , BaseOct <$ caseInsensitive' "0"
    , BaseDec <$ lookAhead (satisfy $ \c -> c >= '1' && c <= '9')
    ]

-- | Parse digit in the given base
--
-- Returns the value of the digit, of 'Nothing' for single quotes
-- (which are allowed as a separator between digits).
parseDigitInBase :: Base -> Parser Digit
parseDigitInBase = satisfyWith . (. toLower) . aux
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

{-------------------------------------------------------------------------------
  Auxiliary: parsec
-------------------------------------------------------------------------------}

type Parser = Parsec Text ()

