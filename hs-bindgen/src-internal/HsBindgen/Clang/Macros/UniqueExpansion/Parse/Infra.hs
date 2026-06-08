-- |
--
-- Intended for unqualified import.
module HsBindgen.Clang.Macros.UniqueExpansion.Parse.Infra (
    -- * Parser type
    Parser
  , runParser
    -- * Parse errors
  , MacroParseError(..)
    -- * Dealing with individual tokens
  , token
    -- * Punctuation
  , punctuation
  , parens
  , comma
    -- * Parse tokens
  , TokenParser
  , parseTokenOfKind
  ) where

import Control.Exception (Exception)
import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (first))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Text.Parsec (ParseError, Parsec, SourcePos)
import Text.Parsec qualified as Parsec
import Text.Parsec.Pos (newPos)

import Clang.Enum.Simple (fromSimpleEnum)
import Clang.HighLevel.Types (MultiLoc (multiLocExpansion), Range (rangeStart),
                              SingleLoc (singleLocColumn, singleLocLine, singleLocPath),
                              Token (tokenExtent, tokenKind, tokenSpelling),
                              TokenSpelling (getTokenSpelling))
import Clang.LowLevel.Core (CXTokenKind (CXToken_Punctuation))
import Clang.Paths (getSourcePath)

import HsBindgen.Errors (panicPure)

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
          reparseError       = show err
        , reparseErrorTokens = tokens
        }

{-------------------------------------------------------------------------------
  Parse errors
-------------------------------------------------------------------------------}

data MacroParseError = MacroParseError {
      reparseError       :: String
    , reparseErrorTokens :: [Token TokenSpelling]
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Exception)

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
    if fromSimpleEnum (tokenKind t) == Right kind
      then f $ getTokenSpelling (tokenSpelling t)
      else Nothing

tokenOfKind' :: CXTokenKind -> (Text -> Bool) -> Parser ()
tokenOfKind' kind cmp = tokenOfKind kind (\actual -> guard $ cmp actual)

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
  Parse individual tokens
-------------------------------------------------------------------------------}

type TokenParser = Parsec Text ()

parseTokenOfKind :: CXTokenKind -> TokenParser a -> Parser (Text, a)
parseTokenOfKind kind p = tokenOfKind kind $ \str -> fmap (str,) $
    either (const Nothing) Just $
      Parsec.parse (p <* Parsec.eof) "" str
