-- | @parsec@ infrastructure for parsing streams of @libclang@ tokens.
--
-- Intended for unqualified import.
module HsBindgen.Macro.Parse (
    -- * Parser type
    Parser
  , runParser
    -- * Dealing with individual tokens
  , token
  , identifier
  , identifierOrKeyword
  , isIdentifier
  , spelling
    -- * Punctuation
  , punctuation
  , parens
  , comma
    -- * Splitting macro definitions
  , splitMacro
  ) where

import Control.Monad (guard, unless)
import Data.Bifunctor (Bifunctor (first))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import Text.Parsec (ParseError, Parsec, SourcePos)
import Text.Parsec qualified as Parsec
import Text.Parsec.Pos (newPos)

import Clang.Enum.Simple (fromSimpleEnum)
import Clang.HighLevel.Types (MultiLoc (multiLocExpansion),
                              Range (rangeEnd, rangeStart),
                              SingleLoc (singleLocColumn, singleLocLine, singleLocPath),
                              Token (tokenExtent, tokenKind, tokenSpelling),
                              TokenSpelling (getTokenSpelling))
import Clang.LowLevel.Core (CXTokenKind (CXToken_Identifier, CXToken_Keyword, CXToken_Punctuation))
import Clang.Paths (getSourcePath)

import HsBindgen.Runtime.Macro qualified as RawMacro

import HsBindgen.Errors (panicPure)
import HsBindgen.Macro.Error (MacroParseError (..))

{-------------------------------------------------------------------------------
  Parser type
-------------------------------------------------------------------------------}

type Parser = Parsec [Token TokenSpelling] ()

runParser ::
     HasCallStack
  => Parser a
  -> [Token TokenSpelling]
  -> Either MacroParseError a
runParser p tokens =
    first unrecognized $ Parsec.runParser p () sourcePath tokens
  where
    sourcePath :: FilePath
    sourcePath =
        case tokens of
          []  -> panicPure "runParser: empty list"
          t:_ -> getSourcePath $ singleLocPath start
            where
              start :: SingleLoc
              start = rangeStart $ multiLocExpansion <$> tokenExtent t

    unrecognized :: ParseError -> MacroParseError
    unrecognized err = MacroParseError{
          macroParseError       = show err
        , macroParseErrorTokens = tokens
        }

{-------------------------------------------------------------------------------
  Dealing with individual tokens
-------------------------------------------------------------------------------}

token :: (Token TokenSpelling -> Maybe a) -> Parser a
token = Parsec.token tokenPretty tokenSourcePos
  where
    tokenPretty :: Token TokenSpelling -> String
    tokenPretty tok = concat [
          show $ Text.unpack tok.tokenSpelling.getTokenSpelling
        , " ("
        , show tok.tokenKind
        ,  ")"
        ]

    tokenSourcePos :: Token a -> SourcePos
    tokenSourcePos t =
        newPos
          (getSourcePath $ singleLocPath start)
          (singleLocLine start)
          (singleLocColumn start)
      where
        start :: SingleLoc
        start = rangeStart $ multiLocExpansion <$> tokenExtent t

tokenOfKind :: CXTokenKind -> (Text -> Maybe a) -> Parser a
tokenOfKind kind f = token $ \t ->
    if isOfKind kind t
      then f $ getTokenSpelling (tokenSpelling t)
      else Nothing

tokenOfKind' :: CXTokenKind -> (Text -> Bool) -> Parser ()
tokenOfKind' kind cmp = tokenOfKind kind (\actual -> guard $ cmp actual)

isOfKind :: CXTokenKind -> Token TokenSpelling -> Bool
isOfKind kind t = fromSimpleEnum (tokenKind t) == Right kind

-- | Is this token an identifier?
isIdentifier :: Token TokenSpelling -> Bool
isIdentifier = isOfKind CXToken_Identifier

-- | The spelling of a token
spelling :: Token TokenSpelling -> Text
spelling = getTokenSpelling . tokenSpelling

-- | Parse an identifier
--
-- Does not accept C keywords; use 'identifierOrKeyword' where a keyword is
-- valid.
identifier :: Parser (Token TokenSpelling)
identifier = token $ \t -> t <$ guard (isIdentifier t)

-- | Parse an identifier or a keyword
--
-- In later LLVMs (not in 14, surely in 16), @bool@ is classified as a keyword
-- rather than an identifier. We accept keywords so that macros such as
-- @#define bool int@ can be parsed. Even in C23 the meaning of @bool@ can be
-- overwritten (the macro takes precedence).
identifierOrKeyword :: Parser (Token TokenSpelling)
identifierOrKeyword = token $ \t ->
    t <$ guard (isIdentifier t || isOfKind CXToken_Keyword t)

{-------------------------------------------------------------------------------
  Punctuation
-------------------------------------------------------------------------------}

punctuation :: Text -> Parser ()
punctuation expected = tokenOfKind' CXToken_Punctuation $
    \actual -> Text.unpack expected == removeMultilines (Text.unpack actual)

parens :: Parser a -> Parser a
parens p = punctuation "(" *> p <* punctuation ")"

comma :: Parser ()
comma = punctuation ","

-- | Remove multiline characters from the string
--
-- Multiline characters are a pair of characters of the form "\\\n". These
-- characters are sometimes included in (punctuation) tokens. We should remove
-- multiline characters before comparing against a target string. For example,
-- we want @punctuation "("@ to match with a token that has spelling "\\\n(".
--
-- >>> removeMultilines "a\\\ngbe\\\n"
-- "agbe"
--
removeMultilines :: String -> String
removeMultilines = \case
    []     -> []
    (c:cs) -> go c cs
  where
    go prev []        = [prev]
    go '\\' ('\n':cs) = removeMultilines cs
    go prev (c   :cs) = prev : go c cs

{-------------------------------------------------------------------------------
  Splitting macro definitions
-------------------------------------------------------------------------------}

-- | Split a macro definition into its name, parameters and body
--
-- This is the /one/ language-independent macro parser: every macro definition
-- passes through it before any macro language sees it. The body is left
-- unparsed; interpreting it is the macro language's job.
--
-- The tokens are the tokens of the definition /excluding/ the @#define@ itself,
-- as reported by @libclang@ for a @CXCursor_MacroDefinition@ cursor. For
--
-- > #define ADD(x, y) x + y
--
-- the result is @Raw "ADD" (Params ["x", "y"] False) ["x", "+", "y"]@.
splitMacro ::
     HasCallStack
  => [Token TokenSpelling]
  -> Either MacroParseError (RawMacro.Raw (Token TokenSpelling))
splitMacro []     = Left MacroParseError {
      macroParseError       = "macro definition without a name"
    , macroParseErrorTokens = []
    }
splitMacro tokens = runParser (macroDefinition <* Parsec.eof) tokens

macroDefinition :: Parser (RawMacro.Raw (Token TokenSpelling))
macroDefinition = do
    name       <- identifierOrKeyword
    isFunction <- isFunctionLike (tokenExtent name)
    params     <- if isFunction then formalParams else pure RawMacro.NoParams
    body       <- Parsec.many Parsec.anyToken
    pure RawMacro.Raw {
        RawMacro.name   = name
      , RawMacro.params = params
      , RawMacro.body   = body
      }

-- | Is the macro definition function-like?
--
-- A macro definition is function-like if its name is followed immediately by a
-- @(@, without any whitespace in between; see 'lparen'. Otherwise it is
-- object-like.
--
-- @isFunctionLike@ does not consume input.
isFunctionLike ::
     -- | Source location of the macro definition's name
     Range MultiLoc
  -> Parser Bool
isFunctionLike nameRange =
    Parsec.lookAhead $
      Parsec.option False (True <$ Parsec.try (lparen nameRange))

formalParams :: Parser (RawMacro.Params (Token TokenSpelling))
formalParams = parens $ do
    names    <- Parsec.sepEndBy identifier comma
    variadic <- Parsec.option False (True <$ Parsec.try (punctuation "..."))
    pure $ RawMacro.Params names variadic

-- | Parse a @(@ not immediately preceded by white space
--
-- @lparen@ consumes input when it fails. Combine with @try@ if this is
-- undesirable.
--
-- NOTE: @lparen@ is defined in the C reference.
--
-- We used to not check whitespace, which was the source of a bug. See issue
-- #1903: <https://github.com/well-typed/hs-bindgen/issues/1903>
lparen :: Range MultiLoc -> Parser ()
lparen prevRange = do
    tok <- Parsec.lookAhead Parsec.anyToken
    punctuation "("
    unless (adjacentTo prevRange tok) $
      Parsec.unexpected "whitespace before lparen"

-- | Does the token start exactly where the given range ends?
adjacentTo :: Range MultiLoc -> Token TokenSpelling -> Bool
adjacentTo prevRange tok =
       prev.singleLocPath   == current.singleLocPath
    && prev.singleLocLine   == current.singleLocLine
    && prev.singleLocColumn == current.singleLocColumn
  where
    prev    = prevRange.rangeEnd.multiLocExpansion
    current = tok.tokenExtent.rangeStart.multiLocExpansion
