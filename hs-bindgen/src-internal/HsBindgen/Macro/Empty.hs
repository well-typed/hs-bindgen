-- | The 'Empty' macro language does not recognize any macro.
--
-- Then, @hs-bindgen@ does not reparse anything, and uses the macro expansions
-- provided by Clang.
--
-- Intended for unqualified import.
module HsBindgen.Macro.Empty (
    Empty -- opaque
  , empty -- opaque
  ) where

import Data.Map qualified as Map

import HsBindgen.Macro.Error
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro

data Empty

data VoidMacro a
  deriving stock (Functor, Foldable, Traversable, Show, Eq)

absurdMacro :: VoidMacro a -> b
absurdMacro m = case m of {}

instance Macro.HasTypes Empty where
  type Parsed           Empty = VoidMacro
  type TypecheckedType  Empty = VoidMacro
  type TypecheckedValue Empty = VoidMacro

empty :: Macro.Lang Empty
empty = Macro.Lang
  { parse          = \_ -> Left $ MacroParseError "Empty does not parse any macros"
  , resolve        = \_ -> absurdMacro . (.unwrap)
  , typecheck      = \case
                       []    -> Map.empty
                       (x:_) -> absurdMacro x.macro
  , translateType  = absurdMacro
  , translateValue = \_ -> absurdMacro
  }
