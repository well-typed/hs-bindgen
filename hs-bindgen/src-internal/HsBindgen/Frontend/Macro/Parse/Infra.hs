-- | Infrastructure for reparsing
module HsBindgen.Frontend.Macro.Parse.Infra (
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

import Data.Text qualified as Text
import Text.Parsec hiding (token, tokens, runParser)
import Text.Parsec qualified as Parsec
import Text.Parsec.Pos

import Clang.Enum.Simple
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Util.Tracer (PrettyForTrace (..))
import Text.SimplePrettyPrint (hsep, textToCtxDoc, vcat, (><))

{-------------------------------------------------------------------------------
  Parser type
-------------------------------------------------------------------------------}

type Parser = Parsec [Token TokenSpelling] ParserState

data ParserState = ParserState

initParserState :: ParserState
initParserState = ParserState

runParser :: HasCallStack =>
     Parser a
  -> [Token TokenSpelling]
  -> Either MacroParseError a
runParser p tokens =
    first unrecognized $ Parsec.runParser p initParserState sourcePath tokens
  where
    sourcePath :: FilePath
    sourcePath =
        case tokens of
          []  -> panicPure "reparseWith: empty list"
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

instance PrettyForTrace MacroParseError where
  prettyForTrace MacroParseError{
          reparseError
        , reparseErrorTokens
        } = vcat [
        "Reparse error: " >< fromString reparseError
      , hsep $ map (textToCtxDoc . getTokenSpelling . tokenSpelling) reparseErrorTokens
      ]

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
