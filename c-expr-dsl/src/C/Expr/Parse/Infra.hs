{-# LANGUAGE OverloadedStrings #-}

-- | Infrastructure for reparsing
module C.Expr.Parse.Infra (
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

import Control.Exception
import Control.Monad
import Data.Bifunctor
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics
import GHC.Stack
import Text.Parsec hiding (runParser, token, tokens)
import Text.Parsec qualified as Parsec
import Text.Parsec.Pos

import Clang.Enum.Simple
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

{-------------------------------------------------------------------------------
  Parser type
-------------------------------------------------------------------------------}

type Parser = Parsec [Token TokenSpelling] ParserState

data ParserState = ParserState

initParserState :: ParserState
initParserState = ParserState

runParser ::
     HasCallStack
  => Parser a
  -> [Token TokenSpelling]
  -> Either MacroParseError a
runParser p tokens =
    first unrecognized $ Parsec.runParser p initParserState sourcePath tokens
  where
    sourcePath :: FilePath
    sourcePath =
        case tokens of
          []  -> error "reparseWith: empty list"
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
    tokenPretty Token{tokenKind, tokenSpelling} = concat [
          show $ Text.unpack (getTokenSpelling tokenSpelling)
        , " ("
        , show tokenKind
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

exact :: CXTokenKind -> Text -> Parser ()
exact kind expected = tokenOfKind kind (\actual -> guard $ expected == actual)

{-------------------------------------------------------------------------------
  Punctuation
-------------------------------------------------------------------------------}

punctuation :: Text -> Parser ()
punctuation = exact CXToken_Punctuation

parens :: Parser a -> Parser a
parens p = punctuation "(" *> p <* punctuation ")"

comma :: Parser ()
comma = punctuation ","

{-------------------------------------------------------------------------------
  Parse individual tokens
-------------------------------------------------------------------------------}

type TokenParser = Parsec Text ()

parseTokenOfKind :: CXTokenKind -> TokenParser a -> Parser (Text, a)
parseTokenOfKind kind p = tokenOfKind kind $ \str -> fmap (str,) $
    either (const Nothing) Just $
      Parsec.parse (p <* Parsec.eof) "" str
