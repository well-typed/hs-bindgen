-- |
--
-- Intended for unqualified import.
module HsBindgen.Macro.UniqueExpansion.Parse (
    parseInvocation
  ) where

import Text.Parsec (anyToken, choice, manyTill, try)

import Clang.HighLevel.Types (Token, TokenSpelling)

import HsBindgen.Macro.Parse (Parser, identifier, identifierOrKeyword,
                              punctuation, spelling)
import HsBindgen.Macro.UniqueExpansion.Types (Invocation (..), Name (Name))

{-------------------------------------------------------------------------------
  Invocation
-------------------------------------------------------------------------------}

parseInvocation :: Parser Invocation
parseInvocation = do
    macroName <- toName <$> identifierOrKeyword
    let
        functionLike :: Parser Invocation
        functionLike = do
          args <- parseArgs
          pure $ Invocation { name = macroName, args = args }

        objectLike :: Parser Invocation
        objectLike = do
          pure $ Invocation { name = macroName, args = [] }
    choice [try functionLike, objectLike]

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

parseArgs :: Parser [Name]
parseArgs = fmap concat $ do
    punctuation "("
    manyTill
      (choice [
          -- try to parse a name
          try ((:[]) . toName <$> try identifier)
          -- try to parse recursively inside nested matching parentheses
        , try parseArgs
          -- otherwise skip the next token
        , [] <$ anyToken
        ]
      )
      (try (punctuation ")"))

{-------------------------------------------------------------------------------
  Identifiers
-------------------------------------------------------------------------------}

toName :: Token TokenSpelling -> Name
toName = Name . spelling
