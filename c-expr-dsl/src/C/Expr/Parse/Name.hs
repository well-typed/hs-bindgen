-- | Names
module C.Expr.Parse.Name (
    parseName
  , parseLocName
  ) where

import Control.Monad

import C.Expr.Parse.Infra
import C.Expr.Syntax.Name

import Clang.Enum.Simple
import Clang.HighLevel.Types
import Clang.LowLevel.Core

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
parseLocName :: Parser (MultiLoc, Name)
parseLocName = token $ \t -> do
    let spelling = getTokenSpelling (tokenSpelling t)
    let ki = fromSimpleEnum (tokenKind t)
    guard $ ki == Right CXToken_Identifier || ki == Right CXToken_Keyword
    return (
        rangeStart $ tokenExtent t
      , Name spelling
      )
