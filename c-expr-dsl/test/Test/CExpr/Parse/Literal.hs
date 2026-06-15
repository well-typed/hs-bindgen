-- | Unit tests for character and string literal parsing
--
-- These exercise 'C.Expr.Parse.Literal.parseLiteralChar' and
-- 'C.Expr.Parse.Literal.parseLiteralString' through the public macro parser
-- ('C.Expr.Parse.parseMacro'): a @CXToken_Literal@ token whose spelling is the
-- full literal (quotes included) is fed to the parser, and the resulting
-- 'CharLiteral' / 'StringLiteral' is inspected.
--
-- The aim is to cover every kind of character and string literal we accept (and
-- to pin down those we reject), independently of the @hs-bindgen@ integration
-- tests in @macro_strings.h@.
--
-- References:
--
--  * <https://en.cppreference.com/w/c/language/character_constant>
--  * <https://en.cppreference.com/w/c/language/string_literal>
--  * <https://en.cppreference.com/w/c/language/escape>
module Test.CExpr.Parse.Literal (tests) where

import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as Text
import Foreign.C (CChar)
import Test.Tasty
import Test.Tasty.HUnit

import C.Expr.Syntax

import Clang.CStandard
import Clang.HighLevel.Types

import Test.CExpr.Parse.Infra

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Parse.Literal" [
      testWithCStd cStd | cStd <- [minBound .. maxBound :: CStandard]
    ]

testWithCStd :: CStandard -> TestTree
testWithCStd cStd = testGroup (show cStd) [
      testGroup "char: ordinary"          $ tests_charOrdinary   std
    , testGroup "char: simple escapes"    $ tests_charEscapes    std
    , testGroup "char: numeric escapes"   $ tests_charNumeric    std
    , testGroup "char: universal names"   $ tests_charUniversal  std
    , testGroup "char: rejected"          $ tests_charRejected   std
    , testGroup "char: digit quirk"       $ tests_charDigitQuirk std
    , testGroup "string: ordinary"        $ tests_strOrdinary    std
    , testGroup "string: simple escapes"  $ tests_strEscapes     std
    , testGroup "string: numeric escapes" $ tests_strNumeric     std
    , testGroup "string: universal names" $ tests_strUniversal   std
    , testGroup "string: embedded NUL"    $ tests_strEmbeddedNul std
    , testGroup "string: rejected"        $ tests_strRejected    std
    , testGroup "prefixes: rejected"      $ tests_prefixes       std
    ]
  where
    std = ClangCStandard cStd DisableGnu

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | A fixed macro name token used in all tests.
macroNameTok :: Token TokenSpelling
macroNameTok = ident "FOO"

-- | Extract a character literal from a parsed object-like macro body.
getCharLit :: Either e Macro -> Maybe CharLiteral
getCharLit (Right Macro{macroExpr}) = case macroExpr of
    Term (Literal (ValueLit (ValueChar c))) -> Just c
    _                                        -> Nothing
getCharLit _ = Nothing

-- | Extract a string literal from a parsed object-like macro body.
getStrLit :: Either e Macro -> Maybe StringLiteral
getStrLit (Right Macro{macroExpr}) = case macroExpr of
    Term (Literal (ValueLit (ValueString s))) -> Just s
    _                                          -> Nothing
getStrLit _ = Nothing

-- | Extract an integer value from a parsed object-like macro body.
getIntVal :: Either e Macro -> Maybe Integer
getIntVal (Right Macro{macroExpr}) = case macroExpr of
    Term (Literal (ValueLit (ValueInt i))) -> Just (integerLiteralValue i)
    _                                       -> Nothing
getIntVal _ = Nothing

-- | A successful character literal: the spelling parses to the given value,
-- and the original source text is preserved.
charCase :: ClangCStandard -> Text -> CChar -> TestTree
charCase cStd spelling val =
    testCase (Text.unpack spelling) $
      getCharLit (checkMacro cStd [macroNameTok, lit spelling])
        @?= Just (CharLiteral val)

-- | A successful string literal: the spelling parses to the given decoded
-- value, and the original source text is preserved.
strCase :: ClangCStandard -> Text -> ByteString -> TestTree
strCase cStd spelling val =
    testCase (Text.unpack spelling) $
      getStrLit (checkMacro cStd [macroNameTok, lit spelling])
        @?= Just (StringLiteral val)

-- | A literal spelling we reject (the whole macro fails to parse).
failCase :: ClangCStandard -> Text -> TestTree
failCase cStd spelling =
    testCase (Text.unpack spelling) $
      assertBool "expected parse failure" $
        isLeft (checkMacro cStd [macroNameTok, lit spelling])

{-------------------------------------------------------------------------------
  Characters: ordinary (unescaped) source characters
-------------------------------------------------------------------------------}

tests_charOrdinary :: ClangCStandard -> [TestTree]
tests_charOrdinary cStd = [
      charCase cStd "'a'" 97
    , charCase cStd "'Z'" 90
    , charCase cStd "' '" 32
      -- NB: a digit character such as @'0'@ is *not* here: it is currently
      -- mis-parsed as an integer literal (see "char: digit quirk" below).
      -- A double quote needs no escaping inside a character literal.
    , charCase cStd "'\"'" 34
    ]

{-------------------------------------------------------------------------------
  Characters: simple (named) escape sequences
-------------------------------------------------------------------------------}

tests_charEscapes :: ClangCStandard -> [TestTree]
tests_charEscapes cStd = [
      -- Basic source escapes.
      charCase cStd "'\\t'"  9    -- '\t'
    , charCase cStd "'\\v'"  11   -- '\v'
    , charCase cStd "'\\f'"  12   -- '\f'
    , charCase cStd "'\\''"  39   -- '\''
    , charCase cStd "'\\\"'" 34   -- '"'
    , charCase cStd "'\\?'"  63   -- '?'
    , charCase cStd "'\\\\'" 92   -- '\\'
      -- Execution escapes.
    , charCase cStd "'\\0'"  0    -- '\NUL' (octal escape; also in "char: numeric escapes")
    , charCase cStd "'\\a'"  7    -- '\a'
    , charCase cStd "'\\b'"  8    -- '\b'
    , charCase cStd "'\\n'"  10   -- '\n'
    , charCase cStd "'\\r'"  13   -- '\r'
    ]

{-------------------------------------------------------------------------------
  Characters: numeric (octal / hex) escape sequences
-------------------------------------------------------------------------------}

tests_charNumeric :: ClangCStandard -> [TestTree]
tests_charNumeric cStd = [
      -- Octal: \123 == 0o123 == 83 == 'S'.
      charCase cStd "'\\123'" 83
      -- Octal terminates at three digits / first non-octal digit.
    , charCase cStd "'\\0'"   0
      -- Octal at the single-byte boundary: \377 == 255.
    , charCase cStd "'\\377'" 255
      -- Hex: \x53 == 83 == 'S'.
    , charCase cStd "'\\x53'" 83
      -- Hex at the single-byte boundary: \xFF == 255.
    , charCase cStd "'\\xFF'" 255
    ]

{-------------------------------------------------------------------------------
  Characters: universal character names
-------------------------------------------------------------------------------}

tests_charUniversal :: ClangCStandard -> [TestTree]
tests_charUniversal cStd = [
      -- 4-digit \u form: code point > 0xFF, rejected.
      failCase cStd "'\\u3042'"      -- 'あ' (U+3042)
      -- 8-digit \U form outside the BMP, rejected.
    , failCase cStd "'\\U0001F600'" -- '😀' (U+1F600)
      -- The three basic characters explicitly allowed as universal names;
      -- their code points fit in a byte.
    , charCase cStd "'\\u0024'" 36  -- '$'
    , charCase cStd "'\\u0040'" 64  -- '@'
    , charCase cStd "'\\u0060'" 96  -- '`'
    ]

{-------------------------------------------------------------------------------
  Characters: rejected
-------------------------------------------------------------------------------}

tests_charRejected :: ClangCStandard -> [TestTree]
tests_charRejected cStd = [
      -- Multi-character constants have an implementation-defined value.
      failCase cStd "'ab'"
      -- A sequence of single-byte numeric escapes is still multi-character.
    , failCase cStd "'\\xE3\\x81\\x82'"
      -- Numeric escapes that do not fit in a single byte.
    , failCase cStd "'\\777'"   -- octal 511
    , failCase cStd "'\\xABC'"  -- hex 0xABC
      -- A raw multi-byte source character whose code point exceeds a byte.
    , failCase cStd "'\12354'"  -- 'あ' (U+3042)
      -- NB: the empty character literal @''@ is *not* rejected here: it is
      -- currently mis-parsed as the integer 0 (see "char: digit quirk" below).
      -- Universal names may not denote basic characters...
    , failCase cStd "'\\u0041'" -- 'A'
      -- ...nor surrogate code points.
    , failCase cStd "'\\uD800'"
      -- Unknown escape letters are not valid in standard C.
    , failCase cStd "'\\j'"    -- unrecognised escape
    , failCase cStd "'\\e'"    -- GCC extension for ESC, not standard C
      -- \x with no following hex digit is malformed.
    , failCase cStd "'\\x'"
      -- The digits 8 and 9 are not valid octal digits; \8 and \9 are not
      -- named escapes either.
    , failCase cStd "'\\8'"
    , failCase cStd "'\\9'"
      -- \u requires exactly 4 hex digits; \U requires exactly 8.
    , failCase cStd "'\\u004'"     -- only 3 digits
    , failCase cStd "'\\U01F600'"  -- only 6 digits
      -- UCN control characters below U+00A0 (other than U+0024/0040/0060) are
      -- forbidden.
    , failCase cStd "'\\u0001'"
    ]

{-------------------------------------------------------------------------------
  Characters: digit quirk (KNOWN BUG)

  A character literal whose content looks like a number is currently *not*
  parsed as a character at all: it is swallowed by the integer-literal parser.
  Two things conspire:

   * 'C.Expr.Parse.Literal.digitInBase' accepts the single quote @'@ as a C
     digit separator in *any* position (including leading and trailing),
     whereas C only permits separators *between* digits.
   * In 'C.Expr.Parse.Expr' the integer alternative is tried before the
     character alternative, so a @CXToken_Literal@ spelled @'<digits>'@ is
     consumed by the integer parser, the surrounding quotes treated as
     separators.

  As a result @'0'@ yields the integer 0 (not the character whose value is 48),
  and invalid forms such as @''@, @'12'@ and @'1'2'@ parse successfully instead
  of being rejected.

  These tests pin the current (incorrect) behaviour so that a future fix is
  forced to revisit them; the expectations below should then move into
  'tests_charOrdinary' / 'tests_charRejected'.

  TODO: fix the parser (restrict separator positions and/or try the character
  parser before the integer parser).
-------------------------------------------------------------------------------}

tests_charDigitQuirk :: ClangCStandard -> [TestTree]
tests_charDigitQuirk cStd = [
      -- Should be the character '0' (value 48); currently the integer 0.
      quirk "'0'"   0
      -- Should be the character '5' (value 53); currently the integer 5.
    , quirk "'5'"   5
      -- Should be rejected (multi-character); currently the integer 12.
    , quirk "'12'"  12
      -- Should be rejected (garbage); currently the integer 12.
    , quirk "'1'2'" 12
      -- Should be rejected (empty); currently the integer 0.
    , quirk "''"    0
    ]
  where
    quirk :: Text -> Integer -> TestTree
    quirk spelling val =
      testCase (Text.unpack spelling) $
        getIntVal (checkMacro cStd [macroNameTok, lit spelling]) @?= Just val

{-------------------------------------------------------------------------------
  Strings: ordinary (unescaped) source characters
-------------------------------------------------------------------------------}

tests_strOrdinary :: ClangCStandard -> [TestTree]
tests_strOrdinary cStd = [
      strCase cStd "\"a\""           "a"
    , strCase cStd "\"abc\""         "abc"
    , strCase cStd "\"hello world\"" "hello world"
      -- The empty string is a valid (zero-length) literal.
    , strCase cStd "\"\""            ""
      -- A single quote needs no escaping inside a string literal.
    , strCase cStd "\"'\""           "'"
      -- A raw multi-byte source character decodes to that Unicode code point.
    , strCase cStd "\"\12354\""      "\xE3\x81\x82"  -- "あ" (U+3042, UTF-8)
    ]

{-------------------------------------------------------------------------------
  Strings: simple (named) escape sequences
-------------------------------------------------------------------------------}

tests_strEscapes :: ClangCStandard -> [TestTree]
tests_strEscapes cStd = [
      strCase cStd "\"\\t\""  "\t"
    , strCase cStd "\"\\n\""  "\n"
    , strCase cStd "\"\\r\""  "\r"
    , strCase cStd "\"\\v\""  "\v"
    , strCase cStd "\"\\f\""  "\f"
    , strCase cStd "\"\\a\""  "\a"   -- BEL (0x07)
    , strCase cStd "\"\\b\""  "\b"   -- BS  (0x08)
    , strCase cStd "\"\\'\""  "'"
    , strCase cStd "\"\\\"\"" "\""
    , strCase cStd "\"\\?\""  "?"
    , strCase cStd "\"\\\\\"" "\\"
    ]

{-------------------------------------------------------------------------------
  Strings: numeric (octal / hex) escape sequences
-------------------------------------------------------------------------------}

tests_strNumeric :: ClangCStandard -> [TestTree]
tests_strNumeric cStd = [
      -- Octal / hex single bytes.
      strCase cStd "\"\\123\"" "S"  -- 0x53
    , strCase cStd "\"\\x53\"" "S"
      -- A run of single-byte numeric escapes (all < 0x80, valid UTF-8).
    , strCase cStd "\"\\1\\2\\3\\4\\5\\6\"" "\1\2\3\4\5\6"
      -- Hex escapes of UTF-8 bytes: together they decode to one code point.
    , strCase cStd "\"\\xE3\\x81\\x82\"" "\xE3\x81\x82"  -- "あ" (U+3042, UTF-8)
      -- \777 == 0o777 == 0x01FF: two raw bytes [0x01, 0xFF]. These are not
      -- valid UTF-8, but ByteString accepts arbitrary bytes.
    , strCase cStd "\"\\777\"" "\x01\xFF"
      -- Zero-value numeric escapes must each produce a single null byte.
    , strCase cStd "\"\\0\""   "\NUL"    -- single-digit octal zero
    , strCase cStd "\"\\00\""  "\NUL"    -- two-digit octal zero
    , strCase cStd "\"\\000\"" "\NUL"    -- three-digit octal zero
    , strCase cStd "\"\\x0\""  "\NUL"    -- single-digit hex zero
    , strCase cStd "\"\\x00\"" "\NUL"    -- two-digit hex zero
      -- An oversized hex escape contributes its value as big-endian bytes.
      -- \xABC == 0xABC == 2748, big-endian: [0x0A, 0xBC].
    , strCase cStd "\"\\xABC\"" "\x0A\xBC"
    ]

{-------------------------------------------------------------------------------
  Strings: universal character names
-------------------------------------------------------------------------------}

tests_strUniversal :: ClangCStandard -> [TestTree]
tests_strUniversal cStd = [
      -- Universal names decode to their Unicode code point (UTF-8 encoded).
      strCase cStd "\"\\u3042\""     "\xE3\x81\x82"        -- "あ" (U+3042)
    , strCase cStd "\"\\U0001F600\"" "\xF0\x9F\x98\x80"    -- "😀" (U+1F600)
      -- Same basic-character / surrogate restrictions as character literals.
    , failCase cStd "\"\\u0041\""    -- 'A'
    , failCase cStd "\"\\uD800\""    -- surrogate
    ]

{-------------------------------------------------------------------------------
  Strings: rejected
-------------------------------------------------------------------------------}

tests_strRejected :: ClangCStandard -> [TestTree]
tests_strRejected cStd = [
      -- Unknown escape letters are not valid in standard C.
      failCase cStd "\"\\j\""
    , failCase cStd "\"\\e\""   -- GCC extension for ESC, not standard C
      -- \x with no following hex digit is malformed.
    , failCase cStd "\"\\x\""
      -- UCN control characters below U+00A0 (other than U+0024/0040/0060) are
      -- forbidden.
    , failCase cStd "\"\\u0001\""
    ]

{-------------------------------------------------------------------------------
  Strings: embedded NUL bytes
-------------------------------------------------------------------------------}

tests_strEmbeddedNul :: ClangCStandard -> [TestTree]
tests_strEmbeddedNul cStd = [
      strCase cStd "\"hij\\0\""       "hij\NUL"
    , strCase cStd "\"abc\\0def\\0g\"" "abc\NULdef\NULg"
    ]

{-------------------------------------------------------------------------------
  Prefixes: currently unsupported (rejected) for both characters and strings
-------------------------------------------------------------------------------}

tests_prefixes :: ClangCStandard -> [TestTree]
tests_prefixes cStd = [
      -- Character literal prefixes.
      failCase cStd "L'a'"
    , failCase cStd "u'a'"
    , failCase cStd "U'a'"
    , failCase cStd "u8'a'"
      -- String literal prefixes.
    , failCase cStd "L\"a\""
    , failCase cStd "u\"a\""
    , failCase cStd "U\"a\""
    , failCase cStd "u8\"a\""
    ]
