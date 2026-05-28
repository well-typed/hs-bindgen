-- | Parsers for 'PostHeader'
--
-- This module is intended to be imported unqualified. It is also intended to
-- only be imported from within the "HsBindgen.Frontend.Pass.PrepareReparse"
-- module hierarchy.
--
-- > import HsBindgen.Frontend.Pass.PrepareReparse.Parser
--
module HsBindgen.Frontend.Pass.PrepareReparse.Parser (
    parse
  , Parse
  ) where


import Control.Applicative (asum)
import Data.List qualified as List
import Text.Parsec qualified as P

import HsBindgen.Frontend.Pass.PrepareReparse.AST (Decl (..),
                                                   PostHeader (PostHeader),
                                                   Tag (..), TagName (..),
                                                   TagType (..), Target (..))
import HsBindgen.Frontend.Pass.PrepareReparse.Lexer (Token (..))
import HsBindgen.Imports (void)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

parse :: Parse a => [Token] -> Either P.ParseError a
parse = run parseIt

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

type Parser = P.Parsec [Token] ()

run :: Parser a -> [Token] -> Either P.ParseError a
run p = P.runParser (p <* P.eof) () "virtual.h"

{-------------------------------------------------------------------------------
  Class
-------------------------------------------------------------------------------}

class Parse a where
  parseIt :: Parser a

{-------------------------------------------------------------------------------
  Instances:
-------------------------------------------------------------------------------}

instance Parse PostHeader where
  parseIt = PostHeader <$> parseIt

instance Parse [Target] where
  parseIt = P.many parseIt

instance Parse Target where
  parseIt = Target <$> parseIt <*> parseIt <* parseIt @Tag

instance Parse Decl where
  parseIt = Decl . conc <$> P.many anyText
    where
      conc ts = concat $ List.intersperse " " ts

instance Parse Tag where
  parseIt = blockComment $
      Tag <$> parseIt <*> parseIt

instance Parse TagType where
  parseIt = asum [
        P.try $ Field <$ text "field"
      , Function <$ text "function"
      , Typedef <$ text "typedef"
      , Variable <$ text "variable"
      ]

instance Parse TagName where
  parseIt = TagName <$> anyText

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

blockComment :: Parser a -> Parser a
blockComment = P.between blockCommentOpen blockCommentClose

blockCommentOpen :: Parser ()
blockCommentOpen = void $ satisfy (==BlockCommentOpen)

blockCommentClose :: Parser ()
blockCommentClose = void $ satisfy (==BlockCommentClose)

text :: String -> Parser String
text s1 = myToken $ \case
    Text s2 | s1 == s2 -> Just s1
    _                  -> Nothing

anyText :: Parser String
anyText = myToken $ \case
    Text s -> Just s
    _ -> Nothing

satisfy :: (Token -> Bool) -> Parser Token
satisfy p = myToken $ \x ->
    if p x then Just x else Nothing

myToken :: (Token -> Maybe a) -> Parser a
myToken f = P.tokenPrim showTok nextPos testTok
   where
     showTok x  = show x
     nextPos pos _x _ = P.incSourceColumn pos 1
     testTok x  = f x
