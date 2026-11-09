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
import Data.Text qualified as Text
import Text.Parsec qualified as P

import HsBindgen.Frontend.Pass.PrepareReparse.AST (Decl (..),
                                                   PostHeader (PostHeader),
                                                   Tag (..), TagType (..),
                                                   Target (..))
import HsBindgen.Frontend.Pass.PrepareReparse.Lexer (Token (..))
import HsBindgen.Imports (unless, void)
import HsBindgen.IR.C qualified as C

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
  parseIt = do
      open  <- parseIt
      decl  <- parseIt
      close <- parseIt
      unless (close == open) $
        P.unexpected $ "closing tag " ++ show close ++ " for " ++ show open
      pure $ Target open decl

instance Parse Decl where
  parseIt = Decl . conc <$> P.many anyText
    where
      conc ts = concat $ List.intersperse " " ts

instance Parse Tag where
  parseIt = blockComment $
      Tag <$> parseIt <*> parseIt

instance Parse TagType where
  parseIt = asum [
        Field    <$ text "field" <*> parseIt
      , Function <$ text "function"
      , Typedef  <$ text "typedef"
      , Variable <$ text "variable"
      ]

instance Parse C.ScopedName where
  parseIt = myToken $ \case
      Text s -> C.parseScopedName (Text.pack s)
      _      -> Nothing

-- | User-facing syntax, spanning one or two tokens (e.g., @struct \@foo@)
instance Parse C.DeclId where
  parseIt = do
      ts <- unwords <$> P.many1 anyText
      maybe (P.unexpected $ "declaration " ++ ts) pure $
        C.parseDeclId (Text.pack ts)

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
