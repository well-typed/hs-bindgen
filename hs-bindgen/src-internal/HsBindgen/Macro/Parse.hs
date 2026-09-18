-- | @parsec@ infrastructure for parsing streams of @libclang@ tokens.
--
-- Intended for unqualified import.
module HsBindgen.Macro.Parse (
    -- * Parser type
    Parser
  , runParser
    -- * Dealing with individual tokens
  , token
  , identifierOrKeyword
  , isIdentifierOrKeyword
  , spelling
    -- * Punctuation
  , punctuation
  , parens
  , comma
  ) where

import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (first))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import Text.Parsec (ParseError, Parsec, SourcePos)
import Text.Parsec qualified as Parsec
import Text.Parsec.Pos (newPos)

import Clang.Enum.Simple (fromSimpleEnum)
import Clang.HighLevel.Types (MultiLoc (multiLocExpansion), Range (rangeStart),
                              SingleLoc (singleLocColumn, singleLocLine, singleLocPath),
                              Token (tokenExtent, tokenKind, tokenSpelling),
                              TokenSpelling (getTokenSpelling))
import Clang.LowLevel.Core (CXTokenKind (CXToken_Identifier, CXToken_Keyword, CXToken_Punctuation))
import Clang.Paths (getSourcePath)

import HsBindgen.Errors
import HsBindgen.Macro.Error (MacroParseError (..))

{-------------------------------------------------------------------------------
  Parser type
-------------------------------------------------------------------------------}

type Parser = Parsec [Token TokenSpelling] ()

-- | Run a parser on a stream of tokens
--
-- The token stream may be empty: a macro body can be empty (@#define FOO@), and
-- the source path is only used to label parse errors.
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
          macroParseError = show err
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

-- | Is this token a name?
--
-- See 'identifierOrKeyword' for why the two kinds are not told apart.
isIdentifierOrKeyword :: Token TokenSpelling -> Bool
isIdentifierOrKeyword t =
    isOfKind CXToken_Identifier t || isOfKind CXToken_Keyword t

-- | The spelling of a token
spelling :: Token TokenSpelling -> Text
spelling = getTokenSpelling . tokenSpelling

-- | Parse an identifier or a keyword
--
-- Wherever the preprocessor expects an identifier, a keyword will do: it sees
-- pp-tokens, and those know no keywords. Which spellings @libclang@ reports as
-- keywords is decided by the translation unit's language options, so @bool@ is
-- a keyword under C23 and an identifier under C17; that distinction must not
-- reach the macro grammar.
identifierOrKeyword :: Parser (Token TokenSpelling)
identifierOrKeyword = token $ \t -> t <$ guard (isIdentifierOrKeyword t)

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
