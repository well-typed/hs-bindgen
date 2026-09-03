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
-- qualifier is taken by "HsBindgen.Macro.Interface", so this module is
-- qualified as @RawMacro@ there.
module HsBindgen.Runtime.Macro (
    -- * Type
    Raw (..)
  , Params (..)
    -- * Construction
  , objectLike
  , functionLike
  , variadicFunctionLike
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
    -- Note that this differs from @'Params' [] 'False'@, the empty parameter
    -- list of @#define NOW() 0@.
    NoParams
    -- | Function-like macro: parameter names, and whether the list ends in
    -- @...@.
    --
    -- Variadic macros are technically a C99 feature, but @libclang@ has
    -- backported them to C89 as well. We follow @libclang@ behaviour here, and
    -- support variadic macros regardless of the C standard that is configured
    -- (C89 is the first standard).
    --
    -- <https://clang.llvm.org/docs/LanguageExtensions.html#language-extensions-back-ported-to-previous-standards>
  | Params [a] Bool
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
functionLike name params body = Raw {
      name   = Text.pack name
    , params = Params (map Text.pack params) False
    , body   = map Text.pack body
    }

-- | Like 'functionLike', but for a macro whose parameter list ends in @...@.
variadicFunctionLike :: String -> [String] -> [String] -> Raw Text
variadicFunctionLike name params body = Raw {
      name   = Text.pack name
    , params = Params (map Text.pack params) True
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
--
-- The GNU named-variadic form @#define F(args...)@ renders in the C99 form
-- @#define F(args, ...)@.
render :: Raw Text -> Text
render raw = "#define " <> raw.name <> renderParams raw.params <> renderBody raw.body

renderParams :: Params Text -> Text
renderParams NoParams = ""
renderParams (Params params variadic) =
    "(" <> Text.intercalate ", " (params ++ ["..." | variadic]) <> ")"

-- | Render a body, including the space separating it from what precedes it.
--
-- An empty body renders as the empty text, so that @#define FOO@ does not gain
-- a trailing space.
renderBody :: [Text] -> Text
renderBody [] = ""
renderBody ts = " " <> Text.unwords ts
