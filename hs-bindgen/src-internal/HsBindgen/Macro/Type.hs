-- | Pluggable macro-language types
--
-- This module defines the 'HasMacroTypes' typeclass, defining types for parsing
-- and typechecking macros. Instances of 'HasMacroTypes' can be provided by
-- separate packages. The default instance uses @c-expr-dsl@, and is defined in
-- the user-facing @hs-bindgen@ library.
module HsBindgen.Macro.Type (
    -- * Typeclass
    HasMacroTypes(..)
    -- * Macro language type parameter
  , Flip(..)
  , flipF
  , flipM
  ) where

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Typeclass
-------------------------------------------------------------------------------}

-- | Types for parsing and typechecking macro bodies
class (
    Show (ParsedMacroBody l)
  , Eq   (ParsedMacroBody l)
  , Functor     (TypecheckedMacroTypeBody  l)
  , Foldable    (TypecheckedMacroTypeBody  l)
  , Traversable (TypecheckedMacroTypeBody  l)
  , forall var. (Show var, Eq var) => Show (TypecheckedMacroTypeBody  l var)
  , forall var. (Show var, Eq var) => Eq   (TypecheckedMacroTypeBody  l var)
  , Functor     (TypecheckedMacroValueBody l)
  , Foldable    (TypecheckedMacroValueBody l)
  , Traversable (TypecheckedMacroValueBody l)
  , forall var. (Show var, Eq var) => Show (TypecheckedMacroValueBody l var)
  , forall var. (Show var, Eq var) => Eq   (TypecheckedMacroValueBody l var)
  ) => HasMacroTypes (l :: Star) where

  -- | Body of parsed (not yet typechecked) macro.
  data ParsedMacroBody l :: Star

  -- | Body of typechecked type macro. Parameterized by the variable type.
  data TypecheckedMacroTypeBody  l :: Star -> Star

  -- | Body of typechecked value macro. Parameterized by the variable type.
  data TypecheckedMacroValueBody l :: Star -> Star

{-------------------------------------------------------------------------------
  Macro language type parameter
-------------------------------------------------------------------------------}

newtype Flip f a l = Flip { unflip :: f l a }

flipF :: (Flip f n a -> Flip f n b) -> f a n -> f b n
flipF f x =
  let (Flip x') = f (Flip x)
  in  x'

flipM :: Monad m => (Flip f n a -> m (Flip f n b)) -> f a n -> m (f b n)
flipM f x = do
  (Flip x') <- f (Flip x)
  pure x'
