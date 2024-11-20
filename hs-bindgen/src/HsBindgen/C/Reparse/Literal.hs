module HsBindgen.C.Reparse.Literal (
    IntSuffix(..)
  , reparseLiteralInteger
  , reparseLiteralFloating
  ) where

import Data.Char (toLower, ord)
import Data.Scientific qualified as Scientific
import Text.Parsec
import Text.Show.Pretty (PrettyVal)
import GHC.Generics

import HsBindgen.Imports
import HsBindgen.C.Reparse.Infra
import HsBindgen.Util.Parsec
import HsBindgen.C.AST.Type

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
  deriving anyclass PrettyVal

intSuffix :: TokenParser IntSuffix
intSuffix = choice [
      IntSuffixUnsigned <$ caseInsensitive' "u"
    , IntSuffixLongLong <$ caseInsensitive' "ll"
    , IntSuffixLong     <$ caseInsensitive' "l"
    , IntSuffixSize     <$ caseInsensitive' "z"
    ]

reparseLiteralInteger :: TokenParser (Integer, PrimIntType)
reparseLiteralInteger = do
    (b, ds, suffixes) <- aux

    let val = readInBase b ds

        ty = case suffixes of
          [] -> PrimInt Signed
          _  ->
            let sign = if any ( == IntSuffixUnsigned ) suffixes
                       then Unsigned
                       else Signed
                long     = any ( == IntSuffixLong ) suffixes
                longlong = any ( == IntSuffixLongLong ) suffixes
             in
                if | longlong
                   -> PrimLongLong sign
                   | long
                   -> PrimLong sign
                   | otherwise
                   -> PrimInt sign

    return (val, ty)
  where
    aux :: TokenParser (Base, [Digit], [IntSuffix])
    aux = do
        b  <- base
        ds <- many1 $ digitInBase b
        ss <- many intSuffix
        return (b, ds, ss)

readInBase :: Base -> [Digit] -> Integer
readInBase b ds =
  let
    multipliers = iterate (* baseToInt b) 1
  in
    sum $ zipWith (*) (reverse $ mapMaybe getDigit ds) multipliers

{-------------------------------------------------------------------------------
  Parser for floating-point literals

  Reference: <https://en.cppreference.com/w/cpp/language/floating_literal>
-------------------------------------------------------------------------------}


reparseLiteralFloating :: TokenParser (Float, Double, PrimFloatType)
reparseLiteralFloating = do

  b     <- option BaseDec (BaseHex <$ caseInsensitive' "0x")
  as    <- many (digitInBase b)
  mbXs  <- optionMaybe $ do { void (char '.') ; many (digitInBase b) }
  mbExp <-
    case b of
      -- Exponent is non-optional with hexadecimal base
      BaseHex -> Just <$> parseExponent b
      _       -> optionMaybe (parseExponent b)

  if
    | Nothing <- mbXs
    , Nothing <- mbExp
    -> fail $ "cannot parse floating-point value: expected either '.' or '" ++ exponentText b ++ "'"
    | null as
    , case mbXs of { Nothing -> True; Just [] -> True; _ -> False }
    -> fail $ "cannot parse floating-point value without any digits"
    | otherwise
    -> do ty <- choice
            [ PrimFloat      <$ caseInsensitive' "f"
            , PrimLongDouble <$ caseInsensitive' "l"
            , pure PrimDouble
            ]
          let m :: Integer
              m = readInBase b (as ++ fromMaybe [] mbXs)
              e :: Int
              e = fromMaybe 0 mbExp - maybe 0 length mbXs
          return (fromScientific m e, fromScientific m e, ty)

fromScientific :: forall a. RealFloat a => Integer -> Int -> a
fromScientific m e = Scientific.toRealFloat $ Scientific.scientific m e

parseExponent :: Base -> TokenParser Int
parseExponent b = do
  void (caseInsensitive' $ exponentText b)
  s <- parseSign
  ds <- many digit
  return $ applySign s ( read ds )

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
