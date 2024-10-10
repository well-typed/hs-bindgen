{-# LANGUAGE OverloadedStrings #-}

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
    -- * Punctuation
  , punctuation
  , parens
  , comma
    -- * Parse tokens
  , TokenParser
  , parseTokenOfKind
    -- * Auxiliary
  , anythingMatchingBrackets
  ) where

import Control.Exception (Exception)
import Control.Monad
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Text.Parsec hiding (token, tokens)
import Text.Parsec qualified as Parsec
import Text.Parsec.Pos
import Text.Show.Pretty (PrettyVal)

import HsBindgen.Clang.Core
import HsBindgen.Clang.Util.SourceLoc.Type
import HsBindgen.Clang.Util.Tokens
import HsBindgen.Patterns
import HsBindgen.Util.Tracer (PrettyLogMsg(..))
import Data.Bifunctor

{-------------------------------------------------------------------------------
  Parser type
-------------------------------------------------------------------------------}

type Reparse = Parsec [Token TokenSpelling] ParserState

data ParserState = ParserState

initParserState :: ParserState
initParserState = ParserState

reparseWith ::
     Reparse a
  -> [Token TokenSpelling]
  -> Either ReparseError a
reparseWith p tokens =
    first unrecognized $ runParser p initParserState sourcePath tokens
  where
    sourcePath :: FilePath
    sourcePath =
        case tokens of
          []  -> error "reparseWith: empty list"
          t:_ -> Text.unpack . getSourcePath $ singleLocPath start
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
  deriving anyclass (Exception, PrettyVal)

instance PrettyLogMsg ReparseError where
  prettyLogMsg ReparseError{
          reparseError
        , reparseErrorTokens
        } = unlines [
        reparseError
      , intercalate " " (
            map
              (Text.unpack . getTokenSpelling . tokenSpelling)
              reparseErrorTokens
          )
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
          (Text.unpack . getSourcePath $ singleLocPath start)
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

{-------------------------------------------------------------------------------
  Punctuation
-------------------------------------------------------------------------------}

punctuation :: Text -> Reparse ()
punctuation = exact CXToken_Punctuation

parens :: Reparse a -> Reparse a
parens p = punctuation "(" *> p <* punctuation ")"

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
anythingMatchingBrackets :: Reparse [Token TokenSpelling]
anythingMatchingBrackets =
    concat <$> Parsec.many go
  where
    go :: Reparse [Token TokenSpelling]
    go = Parsec.choice [
          do open   <- token isOpenParens
             inside <- concat <$> Parsec.many go
             close  <- token isCloseParens
             return $ [open] ++ inside ++ [close]
        , (:[]) <$> token nonParens
        ]

    isOpenParens :: Token TokenSpelling -> Maybe (Token TokenSpelling)
    isOpenParens t = do
        guard $ fromSimpleEnum (tokenKind t) == Right CXToken_Punctuation
        guard $ getTokenSpelling (tokenSpelling t) == "("
        return t

    isCloseParens :: Token TokenSpelling -> Maybe (Token TokenSpelling)
    isCloseParens t = do
        guard $ fromSimpleEnum (tokenKind t) == Right CXToken_Punctuation
        guard $ getTokenSpelling (tokenSpelling t) == ")"
        return t

    nonParens :: Token TokenSpelling -> Maybe (Token TokenSpelling)
    nonParens t =
        case (isOpenParens t, isCloseParens t) of
          (Nothing, Nothing) -> Just t
          _otherwise         -> Nothing
