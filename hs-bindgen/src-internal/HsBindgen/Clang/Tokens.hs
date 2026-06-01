-- | Helpers for @libclang@ tokens
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Clang.Tokens qualified as Clang
module HsBindgen.Clang.Tokens (
    -- * Pretty-printing
    prettyTokens
  , prettyToken
  ) where

import Data.List qualified as List
import Data.Text qualified as Text
import GHC.Show (showSpace)

import Clang.HighLevel.Types (Token (tokenSpelling),
                              TokenSpelling (getTokenSpelling))

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
