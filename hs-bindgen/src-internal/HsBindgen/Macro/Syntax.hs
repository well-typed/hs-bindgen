-- | Macro definitions and invocations, as reported by @libclang@.
module HsBindgen.Macro.Syntax (
    MacroDefinition (..)
  , MacroInvocation (..)
    -- * Splitting macro definitions
  , splitMacro
  ) where

import Control.Monad (unless)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Text.Parsec qualified as Parsec

import Clang.HighLevel.Types (MultiLoc (multiLocExpansion),
                              Range (rangeEnd, rangeStart),
                              SingleLoc (singleLocColumn, singleLocLine, singleLocPath),
                              Token (tokenExtent), TokenSpelling)

import HsBindgen.Runtime.Macro qualified as RawMacro

import HsBindgen.Macro.Error (MacroParseError (..))
import HsBindgen.Macro.Parse

data MacroDefinition = MacroDefinition {
    name     :: Text
  , locRange :: Range MultiLoc
    -- | The definition, split into name, parameters and body
    --
    -- The split is language-independent and happens once, while parsing; see
    -- 'splitMacro'.
  , macro    :: Either MacroParseError (RawMacro.Raw (Token TokenSpelling))
  }

data MacroInvocation = MacroInvocation {
    name     :: Text
  , locRange :: Range MultiLoc
  , tokens   :: [Token TokenSpelling]
  }
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Splitting macro definitions
-------------------------------------------------------------------------------}

-- | Split a macro definition into its name, parameters and body
--
-- This is the /one/ language-independent macro parser: every macro definition
-- passes through it before any macro language sees it. The body is left
-- unparsed; interpreting it is the macro language's job.
--
-- The tokens are the tokens of the definition /excluding/ the @#define@ itself,
-- as reported by @libclang@ for a @CXCursor_MacroDefinition@ cursor. For
--
-- > #define ADD(x, y) x + y
--
-- the result is @Raw "ADD" (Params ["x", "y"] False) ["x", "+", "y"]@.
splitMacro ::
     HasCallStack
  => [Token TokenSpelling]
  -> Either MacroParseError (RawMacro.Raw (Token TokenSpelling))
splitMacro []     = Left MacroParseError {
      macroParseError       = "macro definition without a name"
    , macroParseErrorTokens = []
    }
splitMacro tokens = runParser (macroDefinition <* Parsec.eof) tokens

macroDefinition :: Parser (RawMacro.Raw (Token TokenSpelling))
macroDefinition = do
    name       <- identifierOrKeyword
    isFunction <- isFunctionLike (tokenExtent name)
    params     <- if isFunction then formalParams else pure RawMacro.NoParams
    body       <- Parsec.many Parsec.anyToken
    pure RawMacro.Raw {
        RawMacro.name   = name
      , RawMacro.params = params
      , RawMacro.body   = body
      }

-- | Is the macro definition function-like?
--
-- A macro definition is function-like if its name is followed immediately by a
-- @(@, without any whitespace in between; see 'lparen'. Otherwise it is
-- object-like.
--
-- @isFunctionLike@ does not consume input.
isFunctionLike ::
     -- | Source location of the macro definition's name
     Range MultiLoc
  -> Parser Bool
isFunctionLike nameRange =
    Parsec.lookAhead $
      Parsec.option False (True <$ Parsec.try (lparen nameRange))

-- | Parse the parameter list of a function-like macro
--
-- The accepted forms follow the C grammar (C23 6.10.1), plus the GNU named
-- variadic extension:
--
-- > F()        Params []     False
-- > F(x, y)    Params [x, y] False
-- > F(...)     Params []     True
-- > F(x, ...)  Params [x]    True
-- > F(x...)    Params [x]    True   -- GNU
--
-- A trailing comma (@F(x,)@) is rejected, as @clang@ rejects it.
--
-- Parameter names are identifiers only, never keywords, even though the macro
-- /name/ may be a keyword (see 'identifierOrKeyword'): a macro definition can
-- give a new meaning to @bool@, but it cannot use @bool@ as a parameter.
formalParams :: Parser (RawMacro.Params (Token TokenSpelling))
formalParams = parens $ do
    names    <- Parsec.option [] namedParams
    variadic <- variadicSuffix
    pure $ RawMacro.Params names variadic
  where
    -- One or more comma-separated names. The separator is wrapped in 'try' so
    -- that the comma of @F(x, ...)@ is left for 'variadicSuffix'.
    namedParams :: Parser [Token TokenSpelling]
    namedParams =
        (:) <$> identifier <*> Parsec.many (Parsec.try (comma *> identifier))

    -- A comma that is not followed by @...@ is a trailing comma; it consumes
    -- input and so fails the whole parameter list rather than backtracking.
    variadicSuffix :: Parser Bool
    variadicSuffix = Parsec.choice [
          True <$ (comma *> ellipsis)
        , True <$ ellipsis
        , pure False
        ]

    ellipsis :: Parser ()
    ellipsis = punctuation "..."

-- | Parse a @(@ not immediately preceded by white space
--
-- @lparen@ consumes input when it fails. Combine with @try@ if this is
-- undesirable.
--
-- NOTE: @lparen@ is defined in the C reference.
--
-- We used to not check whitespace, which was the source of a bug. See issue
-- #1903: <https://github.com/well-typed/hs-bindgen/issues/1903>
lparen :: Range MultiLoc -> Parser ()
lparen prevRange = do
    tok <- Parsec.lookAhead Parsec.anyToken
    punctuation "("
    unless (adjacentTo prevRange tok) $
      Parsec.unexpected "whitespace before lparen"

-- | Does the token start exactly where the given range ends?
adjacentTo :: Range MultiLoc -> Token TokenSpelling -> Bool
adjacentTo prevRange tok =
       prev.singleLocPath   == current.singleLocPath
    && prev.singleLocLine   == current.singleLocLine
    && prev.singleLocColumn == current.singleLocColumn
  where
    prev, current :: SingleLoc
    prev    = prevRange.rangeEnd.multiLocExpansion
    current = tok.tokenExtent.rangeStart.multiLocExpansion
