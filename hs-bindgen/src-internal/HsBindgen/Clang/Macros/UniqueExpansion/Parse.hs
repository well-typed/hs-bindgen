{-# LANGUAGE OverloadedRecordDot #-}

-- |
--
-- Intended for unqualified import.
module HsBindgen.Clang.Macros.UniqueExpansion.Parse (
    parseDefinition
  , parseInvocation
  ) where

import Control.Monad (guard, unless)
import Data.Maybe (catMaybes)
import Text.Parsec (anyToken, choice, lookAhead, many, manyTill, option, sepBy,
                    try, unexpected)

import Clang.Enum.Simple (fromSimpleEnum)
import Clang.HighLevel.Types (MultiLoc (multiLocExpansion),
                              Range (rangeEnd, rangeStart),
                              SingleLoc (singleLocColumn, singleLocLine, singleLocPath),
                              Token (tokenExtent, tokenKind, tokenSpelling),
                              TokenSpelling (getTokenSpelling))
import Clang.LowLevel.Core (CXTokenKind (CXToken_Identifier, CXToken_Keyword))

import HsBindgen.Clang.Macros.UniqueExpansion.Parse.Infra (Parser, comma,
                                                           parens, punctuation,
                                                           token)
import HsBindgen.Clang.Macros.UniqueExpansion.Types (Definition (..),
                                                     Invocation (..),
                                                     Name (Name), Var (..))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

parseDefinition :: Parser Definition
parseDefinition = do
    (macroLocRange, macroName) <- parseLocName
    isFunction <- isFunctionLike macroLocRange
    params <-
      if isFunction then parseParams else pure []
    macroBody <- parseBody params
    pure Definition {
        name = macroName
      , params = params
      , body = macroBody
      }

-- | Check if a macro definition is function-like. If not, then the definition
-- is object-like.
--
-- A macro definition is function-like if the macro definition's name is
-- followed immediately by a ( character without any whitespace in between. See
-- 'lparen'.
--
-- @isFunctionLike@ does not consume input.
isFunctionLike ::
     -- | Source location of the macro definition's name
     Range MultiLoc
  -> Parser Bool
isFunctionLike prevRange =
    lookAhead (option False (True <$ try (lparen prevRange)))

parseBody :: [Name] -> Parser [Var]
parseBody params = catMaybes <$> many (choice [parseVar, parseNonVar])
  where
    parseVar, parseNonVar :: Parser (Maybe Var)
    parseVar = Just . mkVar <$> parseName
    parseNonVar = Nothing <$ anyToken

    mkVar :: Name -> Var
    mkVar n
      | n `elem` params = LocalParam n
      | otherwise = FreeVar n

{-------------------------------------------------------------------------------
  Invocation
-------------------------------------------------------------------------------}

parseInvocation :: Parser Invocation
parseInvocation = do
    (_, macroName) <- parseLocName
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
  Parameters
-------------------------------------------------------------------------------}

parseParams :: Parser [Name]
parseParams = parens $ parseName `sepBy` comma

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

parseArgs :: Parser [Name]
parseArgs = fmap concat $ do
    punctuation "("
    manyTill
      (choice [
          -- try to parse a name
          try ((:[]) <$> try parseName)
          -- try to parse recursively inside nested matching parentheses
        , try parseArgs
          -- otherwise skip the next token
        , [] <$ anyToken
        ]
      )
      (try (punctuation ")"))

{-------------------------------------------------------------------------------
  Punctuation
-------------------------------------------------------------------------------}

-- | Parse a ( character not immediately preceded by white space
--
-- @lparen@ consumes input when it fails. Combine with @try@ if this is
-- undesirable.
--
-- NOTE: @lparen@ is defined in the C reference
--
-- We used to not check whitespace, which was the source of a bug. See issue
-- #1903: <https://github.com/well-typed/hs-bindgen/issues/1903>
lparen :: Range MultiLoc -> Parser ()
lparen prevRange = do
    tok <- anyToken
    let prev    = prevRange.rangeEnd.multiLocExpansion
        current = tok.tokenExtent.rangeStart.multiLocExpansion
        p       = prev.singleLocPath   == current.singleLocPath &&
                  prev.singleLocLine   == current.singleLocLine &&
                  prev.singleLocColumn == current.singleLocColumn
    unless p $ unexpected "whitespace before lparen"

{-------------------------------------------------------------------------------
  Identifiers
-------------------------------------------------------------------------------}

-- | Parse a name (identifier only)
--
-- Does not accept C keywords. Use 'parseLocName' when the token may be a
-- keyword (e.g. for macro names, where @#define bool int@ is valid C).
parseName :: Parser Name
parseName = token $ \t -> do
    let spelling = getTokenSpelling (tokenSpelling t)
    let ki = fromSimpleEnum (tokenKind t)
    guard $ ki == Right CXToken_Identifier
    return $ Name spelling

-- | Parse a name together with its source location
--
-- Accepts both identifiers and keywords. In later LLVMs (not in 14, surely in
-- 16), @bool@ is classified as a keyword rather than an identifier. We accept
-- keywords here so that macros such as @#define bool int@ can be parsed. Even
-- in C23 the meaning of @bool@ can be overwritten (the macro takes precedence).
parseLocName :: Parser (Range MultiLoc, Name)
parseLocName = token $ \t -> do
    let spelling = getTokenSpelling (tokenSpelling t)
    let ki = fromSimpleEnum (tokenKind t)
    guard $ ki == Right CXToken_Identifier || ki == Right CXToken_Keyword
    return (
        tokenExtent t
      , Name spelling
      )
