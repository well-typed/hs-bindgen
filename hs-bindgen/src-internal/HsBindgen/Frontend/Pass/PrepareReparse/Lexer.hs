-- | Lexers for 'PostHeader'
--
-- This module is intended to be imported unqualified. It is also intended to
-- only be imported from within the "HsBindgen.Frontend.Pass.PrepareReparse"
-- module hierarchy.
--
-- > import HsBindgen.Frontend.Pass.PrepareReparse.Lexer
--
module HsBindgen.Frontend.Pass.PrepareReparse.Lexer (
    lex
  , Token (..)
  ) where

import Prelude hiding (lex)

import Control.Applicative (asum)
import Data.Char (isSpace)
import Text.Parsec qualified as P

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

lex :: String -> Either P.ParseError [Token]
lex = run tokenize

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

type Lexer = P.Parsec String ()

run :: Lexer a -> String -> Either P.ParseError a
run t = P.runParser (t <* P.eof) () "virtual.h"

{-------------------------------------------------------------------------------
  Tokenizer
-------------------------------------------------------------------------------}

data Token =
    BlockCommentOpen
  | BlockCommentClose
  | Text String
  deriving stock (Show, Eq)

tokenize :: Lexer [Token]
tokenize = P.spaces *> P.many tokenizeOne <* P.spaces

tokenizeOne :: Lexer Token
tokenizeOne = asum [
      P.try (BlockCommentOpen <$ P.string "/*" <* P.spaces)
    , P.try (BlockCommentClose <$ P.string "*/" <* P.spaces)
    , Text <$> P.many1 (P.satisfy (not . isSpace)) <* P.spaces
    ]
