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
                              Range (rangeEnd, rangeStart), RealPath,
                              SingleLoc (singleLocColumn, singleLocLine, singleLocPath),
                              SourcePath, Token (tokenExtent), TokenSpelling)

import HsBindgen.Runtime.Macro qualified as Runtime.Macro

import HsBindgen.Macro.Error (MacroParseError)
import HsBindgen.Macro.Parse

data MacroDefinition = MacroDefinition {
    name     :: Text
  , locRange :: Range (MultiLoc RealPath)
    -- | The definition, split into name, parameters and body
    --
    -- The split is language-independent and happens once, while parsing; see
    -- 'splitMacro'.
  , macro    :: Either MacroParseError (Runtime.Macro.Raw (Token SourcePath TokenSpelling))
  }

data MacroInvocation = MacroInvocation {
    name     :: Text
  , locRange :: Range (MultiLoc RealPath)
  , tokens   :: [Token SourcePath TokenSpelling]
  }
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Splitting macro definitions
-------------------------------------------------------------------------------}

-- | Split a macro definition into its name, parameters and body
--
-- This is the /one/ language-independent macro parser in `hs-bindgen`: every
-- macro definition passes through it before the used macro language parses the
-- macro body. Here, we do not parse the body.
--
-- The tokens are the tokens of the definition /excluding/ the @#define@ itself,
-- as reported by @libclang@ for a @CXCursor_MacroDefinition@ cursor. For
--
-- > #define ADD(x, y) x + y
--
-- the result is @Raw "ADD" (Params ["x", "y"] False) ["x", "+", "y"]@.
splitMacro ::
     HasCallStack
  => [Token SourcePath TokenSpelling]
  -> Either MacroParseError (Runtime.Macro.Raw (Token SourcePath TokenSpelling))
splitMacro = runParser (macroDefinition <* Parsec.eof)

macroDefinition :: Parser (Runtime.Macro.Raw (Token SourcePath TokenSpelling))
macroDefinition = do
    name       <- identifierOrKeyword
    isFunction <- isFunctionLike (tokenExtent name)
    params     <- if isFunction then formalParams else pure Runtime.Macro.NoParams
    body       <- Parsec.many Parsec.anyToken
    pure Runtime.Macro.Raw {
        Runtime.Macro.name   = name
      , Runtime.Macro.params = params
      , Runtime.Macro.body   = body
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
     Range (MultiLoc SourcePath)
  -> Parser Bool
isFunctionLike nameRange =
    Parsec.lookAhead $
      Parsec.option False (True <$ Parsec.try (lparen nameRange))

-- | Parse the parameter list of a function-like macro
--
-- The accepted forms follow the C grammar (C23 6.10.1), plus the GNU named
-- variadic extension:
--
-- > F()        Params []     NotVariadic
-- > F(x, y)    Params [x, y] NotVariadic
-- > F(...)     Params []     Ellipsis
-- > F(x, ...)  Params [x]    Ellipsis
-- > F(x...)    Params []     (NamedEllipsis x)       -- GNU
-- > F(x, y...) Params [x]    (NamedEllipsis y)       -- GNU
--
-- An @...@ that is not preceded by a comma binds to the name before it (GNU form).
--
-- A trailing comma (@F(x,)@) is /rejected/ (similar to @clang@).
--
-- We do not implement additional logic to reject repeated parameter names
-- (@F(x, x)@), and /accepted/ them, although C23 6.10.1p6 forbids them.
-- @clang@ rejects such a definition, so no cursor for it ever reaches us.
--
-- A parameter name may be a keyword, just as the macro /name/ may be (see
-- 'identifierOrKeyword'). The preprocessor sees pp-tokens, which know no
-- keywords, so @clang@ accepts @#define F(bool) bool@ even in C23, where
-- @bool@ is one.
formalParams :: Parser (Runtime.Macro.Params (Token SourcePath TokenSpelling))
formalParams = parens $ do
    names <- Parsec.option [] namedParams
    Parsec.choice [
        -- A comma that is not followed by @...@ is a trailing comma; it
        -- consumes input and so fails the whole parameter list rather than
        -- backtracking into the alternatives below.
        Runtime.Macro.Params names Runtime.Macro.Ellipsis <$ (comma *> ellipsis)
      , namedEllipsis names <$ ellipsis
      , pure $ Runtime.Macro.Params names Runtime.Macro.NotVariadic
      ]
  where
    -- One or more comma-separated names. The separator is wrapped in 'try' so
    -- that the comma of @F(x, ...)@ is left for the variadic suffix.
    namedParams :: Parser [Token SourcePath TokenSpelling]
    namedParams =
            (:)
        <$> identifierOrKeyword
        <*> Parsec.many (Parsec.try (comma *> identifierOrKeyword))

    -- @F(...)@ has no name for the ellipsis to bind to.
    namedEllipsis ::
         [Token SourcePath TokenSpelling]
      -> Runtime.Macro.Params (Token SourcePath TokenSpelling)
    namedEllipsis names = case reverse names of
        []   -> Runtime.Macro.Params [] Runtime.Macro.Ellipsis
        n:ns -> Runtime.Macro.Params (reverse ns) (Runtime.Macro.NamedEllipsis n)

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
lparen :: Range (MultiLoc SourcePath) -> Parser ()
lparen prevRange = do
    tok <- Parsec.lookAhead Parsec.anyToken
    punctuation "("
    unless (adjacentTo prevRange tok) $
      Parsec.unexpected "whitespace before lparen"

-- | Does the token start exactly where the given range ends?
adjacentTo :: Range (MultiLoc SourcePath) -> Token SourcePath TokenSpelling -> Bool
adjacentTo prevRange tok =
       prev.singleLocPath   == current.singleLocPath
    && prev.singleLocLine   == current.singleLocLine
    && prev.singleLocColumn == current.singleLocColumn
  where
    prev, current :: SingleLoc SourcePath
    prev    = prevRange.rangeEnd.multiLocExpansion
    current = tok.tokenExtent.rangeStart.multiLocExpansion
