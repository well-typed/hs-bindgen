{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Raw macros: C macros kept as their token spelling, untyped.
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.Macro qualified as Macro
--
-- Generated code uses the qualifier @Macro@. Inside @hs-bindgen@ itself that
-- qualifier is taken by the macro-language interface, so this module is
-- qualified as @RawMacro@ there.
module HsBindgen.Runtime.Macro (
    -- * Type
    Raw (..)
  , Params (..)
  , Variadic (..)
    -- * Construction
  , objectLike
  , functionLike
  , variadic
  , variadicNamed
    -- * Rendering
  , render
  ) where

import Data.Text (Text)
import Data.Text qualified as Text

{-------------------------------------------------------------------------------
  Type
-------------------------------------------------------------------------------}

-- | A macro that was not typechecked; only its token spellings are known.
--
-- The name is part of the value so that 'render' can produce a definition
-- rather than just a body.
--
-- @a@ is the representation of a single token.
--
-- Generated code uses @'Raw' 'Text'@.
data Raw a = Raw {
      name   :: a
    , params :: Params a
    , body   :: [a]
    }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | The parameter list of a macro.
data Params a =
    -- | Object-like macro: no parameter list at all.
    --
    -- Note that this differs from @'Params' [] 'NotVariadic'@, the empty
    -- parameter list of @#define NOW() 0@.
    NoParams
    -- | Function-like macro: the named parameters, and how the list ends.
  | Params [a] (Variadic a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | How the parameter list of a function-like macro ends.
--
-- Variadic macros are technically a C99 feature, but @libclang@ has backported
-- them to C89 as well. We follow @libclang@ behaviour here, and support
-- variadic macros regardless of the C standard that is configured (C89 is the
-- first standard).
--
-- <https://clang.llvm.org/docs/LanguageExtensions.html#language-extensions-back-ported-to-previous-standards>
data Variadic a =
    -- | @#define F(x, y)@: the macro takes exactly its named parameters.
    NotVariadic
    -- | @#define F(x, ...)@: the trailing arguments are @__VA_ARGS__@.
  | Ellipsis
    -- | @#define F(x, args...)@: the trailing arguments are named.
    --
    -- This is the GNU named-variadic extension; the name replaces
    -- @__VA_ARGS__@ in the body. It is a distinct constructor because it is
    -- /not/ equivalent to 'Ellipsis' with the name as its last parameter:
    -- @#define F(args...) g(args)@ passes every argument to @g@, whereas
    -- @#define F(args, ...) g(args)@ passes only the first.
    --
    -- <https://gcc.gnu.org/onlinedocs/cpp/Variadic-Macros.html>
  | NamedEllipsis a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Construct an object-like macro from its name and body token spellings.
objectLike :: String -> [String] -> Raw Text
objectLike name body = Raw {
      name   = Text.pack name
    , params = NoParams
    , body   = map Text.pack body
    }

-- | Construct a function-like macro from its name, parameter names, and body
-- token spellings.
functionLike :: String -> [String] -> [String] -> Raw Text
functionLike name params body =
    mkFunctionLike name params NotVariadic body

-- | Like 'functionLike', but for a macro whose parameter list ends in @...@.
variadic :: String -> [String] -> [String] -> Raw Text
variadic name params body =
    mkFunctionLike name params Ellipsis body

-- | Like 'variadic', but for the GNU named-variadic form: the third argument
-- is the name that stands for the trailing arguments.
--
-- @#define LOG(fmt, args...) printf(fmt, args)@ is
--
-- > variadicNamed "LOG" ["fmt"] "args"
-- >   ["printf", "(", "fmt", ",", "args", ")"]
variadicNamed :: String -> [String] -> String -> [String] -> Raw Text
variadicNamed name params ellipsisName body =
    mkFunctionLike name params (NamedEllipsis (Text.pack ellipsisName)) body

mkFunctionLike :: String -> [String] -> Variadic Text -> [String] -> Raw Text
mkFunctionLike name params variadicity body = Raw {
      name   = Text.pack name
    , params = Params (map Text.pack params) variadicity
    , body   = map Text.pack body
    }

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

-- | Render a macro as a @#define@ directive.
--
-- >>> render (functionLike "ADD" ["x", "y"] ["x", "+", "y"])
-- "#define ADD(x, y) x + y"
--
-- Whitespace is not stored, so the result is canonical: tokens are separated by
-- a single space, parameters by a comma and a space. @#define ADD(x,y) x+y@
-- renders as above.
render :: Raw Text -> Text
render raw =
    "#define " <> raw.name <> renderParams raw.params <> renderBody raw.body

renderParams :: Params Text -> Text
renderParams NoParams = ""
renderParams (Params names variadicity) =
    "(" <> Text.intercalate ", " (names ++ renderVariadic variadicity) <> ")"

-- | Render the end of a parameter list, as the names that follow the named
-- parameters.
renderVariadic :: Variadic Text -> [Text]
renderVariadic = \case
    NotVariadic      -> []
    Ellipsis         -> ["..."]
    NamedEllipsis nm -> [nm <> "..."]

-- | Render a body, including the space separating it from what precedes it.
--
-- An empty body renders as the empty text, so that @#define FOO@ does not gain
-- a trailing space.
renderBody :: [Text] -> Text
renderBody [] = ""
renderBody ts = " " <> Text.unwords ts
