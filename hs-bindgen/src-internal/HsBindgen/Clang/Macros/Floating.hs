module HsBindgen.Clang.Macros.Floating (
    invocationCanFloatInwards
  ) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

import HsBindgen.Clang.Macros (MacroDefinition (..))

-- | Check whether a macro invocation can be fully floated inwards
--
-- /Floating inwards/ means that a macro invocation is moved into deeper macro
-- scopes (e.g., down the source file).
--
-- /Fully/ floating downwards means that a macro invocation is moved to the
-- deepest scope (e.g., bottom of the source file).
--
-- Macro invocations can be floated inwards as long as the reference to the
-- macro name is not captured by a new macro definition. By extension, macro
-- invocations can be fully floated inwards as long as the macro name is not
-- captured by any new macro definition.
--
-- TODO <?>: for simplicity we have picked a temporary approximation of this
-- property. All macro invocations are considered not inwards-floatable as long
-- as there is a macro name that has multiple definitions. The goal is to write
-- a better approximation later.
--
-- === Examples
--
-- ==== Inwards-floatable
--
-- For example, if we have this code:
--
-- > #define A int
-- > A x;
-- > #define B char
--
-- Then we can float the invocation of @A@ inwards like so, without changing
-- what that invocation expands to:
--
-- > #define A int
-- > #define B char
-- > A x;
--
-- ==== Not inwards-floatable
--
-- For example, if we have this code:
--
-- > #define A int
-- > A x;
-- > #undef A
-- > #define A char
--
-- Then we can /not/ float the invocation of @A@ inwards like so, without
-- changing what that invocation expands to:
--
-- > #define A int
-- > #undef A
-- > #define A char
-- > A x;
--
invocationCanFloatInwards ::
     -- | In-scope macro definitions
     [MacroDefinition]
  -> Bool
invocationCanFloatInwards = go Set.empty
  where
    go :: Set Text -> [MacroDefinition] -> Bool
    go _seen []
      = False
    go seen  (x:xs)
      | x.name `Set.member` seen
      = True
      | otherwise
      = go (x.name `Set.insert` seen) xs

