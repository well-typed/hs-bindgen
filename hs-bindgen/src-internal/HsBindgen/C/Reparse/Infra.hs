-- | Infrastructure for reparsing
module HsBindgen.C.Reparse.Infra (
    -- * Parser type
    Reparse
  , reparseWith
    -- * Parse errors
  , ReparseError(..)
    -- * Dealing with individual tokens
  , token
  , tokenOfKind
  , exact
  , keyword
  , identifier
    -- * Punctuation
  , punctuation
  , parens
  , braces
  , comma
    -- * Parse tokens
  , TokenParser
  , parseTokenOfKind
    -- * Auxiliary
  , anythingMatchingBrackets
  ) where

import Data.Text qualified as Text
import Text.Parsec hiding (token, tokens)
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

type Reparse = Parsec [Token TokenSpelling] ParserState

data ParserState = ParserState

initParserState :: ParserState
initParserState = ParserState

reparseWith :: HasCallStack =>
     Reparse a
  -> [Token TokenSpelling]
  -> Either ReparseError a
reparseWith p tokens =
    first unrecognized $ runParser p initParserState sourcePath tokens
  where
    sourcePath :: FilePath
    sourcePath =
        case tokens of
          []  -> panicPure "reparseWith: empty list"
          t:_ -> getSourcePath $ singleLocPath start
            where
              start :: SingleLoc
              start = rangeStart $ multiLocExpansion <$> tokenExtent t

    unrecognized :: ParseError -> ReparseError
    unrecognized err = ReparseError{
          reparseError       = show err
        , reparseErrorTokens = tokens
        }

{-------------------------------------------------------------------------------
  Parse errors
-------------------------------------------------------------------------------}

data ReparseError = ReparseError {
      reparseError       :: String
    , reparseErrorTokens :: [Token TokenSpelling]
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Exception)

instance PrettyForTrace ReparseError where
  prettyForTrace ReparseError{
          reparseError
        , reparseErrorTokens
        } = vcat [
        "Reparse error: " >< fromString reparseError
      , hsep $ map (textToCtxDoc . getTokenSpelling . tokenSpelling) reparseErrorTokens
      ]

{-------------------------------------------------------------------------------
  Dealing with individual tokens
-------------------------------------------------------------------------------}

token :: (Token TokenSpelling -> Maybe a) -> Reparse a
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

tokenOfKind :: CXTokenKind -> (Text -> Maybe a) -> Reparse a
tokenOfKind kind f = token $ \t ->
    if fromSimpleEnum (tokenKind t) == Right kind
      then f $ getTokenSpelling (tokenSpelling t)
      else Nothing

exact :: CXTokenKind -> Text -> Reparse ()
exact kind expected = tokenOfKind kind (\actual -> guard $ expected == actual)

keyword :: Text -> Reparse Text
keyword kw = kw <$ exact CXToken_Keyword kw

identifier :: Text -> Reparse Text
identifier i = i <$ exact CXToken_Identifier i

{-------------------------------------------------------------------------------
  Punctuation
-------------------------------------------------------------------------------}

punctuation :: Text -> Reparse ()
punctuation = exact CXToken_Punctuation

parens :: Reparse a -> Reparse a
parens p = punctuation "(" *> p <* punctuation ")"

braces :: Reparse a -> Reparse a
braces p = punctuation "{" *> p <* punctuation "}"

comma :: Reparse ()
comma = punctuation ","

{-------------------------------------------------------------------------------
  Parse individual tokens
-------------------------------------------------------------------------------}

type TokenParser = Parsec Text ()

parseTokenOfKind :: CXTokenKind -> TokenParser a -> Reparse (Text, a)
parseTokenOfKind kind p = tokenOfKind kind $ \str -> fmap (str,) $
    either (const Nothing) Just $
      Parsec.parse (p <* Parsec.eof) "" str

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Any sequence of tokens, as long as the brackets inside are matched
anythingMatchingBrackets :: [(Text, Text)] -> Reparse [Token TokenSpelling]
anythingMatchingBrackets brackets =
    concat <$> Parsec.many go
  where
    go :: Reparse [Token TokenSpelling]
    go = Parsec.choice [
          do (open, closer) <- token isOpener
             inside <- concat <$> Parsec.many go
             close  <- token $ isCloser closer
             return $ [open] ++ inside ++ [close]
        , (:[]) <$> token nonParens
        ]

    isOpener :: Token TokenSpelling -> Maybe (Token TokenSpelling, Text)
    isOpener t = do
        guard $ fromSimpleEnum (tokenKind t) == Right CXToken_Punctuation
        closer <- lookup (getTokenSpelling (tokenSpelling t)) brackets
        return (t, closer)

    isCloser :: Text -> Token TokenSpelling -> Maybe (Token TokenSpelling)
    isCloser closer t = do
        guard $ fromSimpleEnum (tokenKind t) == Right CXToken_Punctuation
        guard $ getTokenSpelling (tokenSpelling t) == closer
        return t

    nonParens :: Token TokenSpelling -> Maybe (Token TokenSpelling)
    nonParens t
      | fromSimpleEnum (tokenKind t) == Right CXToken_Punctuation
      , getTokenSpelling (tokenSpelling t) `elem` (map fst brackets ++ map snd brackets)
      = Nothing
      | otherwise
      = Just t

