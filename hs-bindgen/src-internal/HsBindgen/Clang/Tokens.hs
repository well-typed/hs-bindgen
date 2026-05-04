-- | Helpers for @libclang@ tokens
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Clang.Tokens qualified as Clang
module HsBindgen.Clang.Tokens (
    -- * Ordering
    isOrdered
    -- * Pretty-printing
  , prettyTokens
  , prettyToken
  ) where

import Data.List qualified as List
import Data.Text qualified as Text
import GHC.Show (showSpace)

import Clang.HighLevel.Types (MultiLoc (multiLocExpansion),
                              Range (rangeEnd, rangeStart),
                              SingleLoc (singleLocColumn, singleLocLine, singleLocPath),
                              Token (tokenExtent, tokenSpelling),
                              TokenSpelling (getTokenSpelling))

{-------------------------------------------------------------------------------
  Ordering
-------------------------------------------------------------------------------}

-- | Check that all tokens were obtained from the same file, and that they are
-- ordered by line and column.
isOrdered :: [Token TokenSpelling] -> Bool
isOrdered = \case
    [] -> True
    (t:ts) -> go (end t) ts
  where
    go :: SingleLoc -> [Token TokenSpelling] -> Bool
    go _prevEnd []    = True
    go prevEnd (t:ts) = and [
          prevEnd.singleLocPath == curStart.singleLocPath
        , prevEnd.singleLocLine <= curStart.singleLocLine
        , prevEnd.singleLocColumn <= curStart.singleLocColumn
        , go curEnd ts
        ]
      where
        curStart = start t
        curEnd = end t

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

-- | Pretty-print tokens
--
-- The implementation prints all tokens in order with a single space in between.
-- Any whitespace /inside/ tokens will be preserved. Tokens are at its core a
-- string of characters, so for example if a token has a newline character in
-- it, then the printed output will preserve that newline character.
prettyTokens :: [Token TokenSpelling] -> ShowS
prettyTokens tokens =
      foldr (.) id -- compose all ShowS value
    $ List.intersperse showSpace
    $ fmap prettyToken tokens

-- | Pretty-print a token
--
-- Any whitespace /inside/ the token will be preserved. Tokens are at its core a
-- string of characters, so for example if a token has a newline character in
-- it, then the printed output will preserve that newline character.
prettyToken :: Token TokenSpelling -> ShowS
prettyToken token = showString $ Text.unpack token.tokenSpelling.getTokenSpelling

{-------------------------------------------------------------------------------
  Location helpers
-------------------------------------------------------------------------------}

start :: Token TokenSpelling -> SingleLoc
start tok = tok.tokenExtent.rangeStart.multiLocExpansion

end :: Token TokenSpelling -> SingleLoc
end tok = tok.tokenExtent.rangeEnd.multiLocExpansion
