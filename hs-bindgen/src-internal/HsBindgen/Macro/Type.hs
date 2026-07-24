-- | Pluggable macro-language types
--
-- This module defines the macro 'HasTypes' typeclass, defining types for
-- parsing and typechecking macros. Instances of 'HasTypes' can be provided by
-- separate packages. The default instance uses @c-expr-dsl@, and is defined in
-- the user-facing @hs-bindgen@ library.
--
-- Intended for qualified import.
--
-- @
-- import HsBindgen.Macro.Type qualified as Macro
-- @
module HsBindgen.Macro.Type (
    -- * Typeclass
    HasTypes(..)
  ) where

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Typeclass
-------------------------------------------------------------------------------}

-- | Types for parsing and typechecking macros
--
-- Initially, we store the 'ParsedMacro' in the AST. The 'ParsedMacro'
-- is parameterized by an annotation:
--
-- 1. After parse, the annotation is @()@ (see 'Macro.Unresolved').
--
-- 2. After name resolution, the annotation is the 'C.DeclId' (see
--    'Macro.Resolved').
--
-- After we typecheck the macros, we store the 'TypecheckedMacroType' or
-- 'TypecheckedMacroValue' in the AST. These are parameterized by a variable:
--
-- 1. After typechecking that variable is instantiated to 'DeclId' (which is the
--    annotation of the 'ParsedMacro' prior to typechecking).
--
-- 2. After name mangling, the variable becomes a 'C.DeclIdPair'.
class (
    forall ann. (Show ann, Eq ann) => ValidParsed l ann
  , Functor     (TypecheckedType  l)
  , Foldable    (TypecheckedType  l)
  , Traversable (TypecheckedType  l)
  , forall var. (Show var, Eq var) => ValidTypecheckedType l var
  , Functor     (TypecheckedValue l)
  , Foldable    (TypecheckedValue l)
  , Traversable (TypecheckedValue l)
  , forall var. (Show var, Eq var) => ValidTypecheckedValue l var
  ) => HasTypes (l :: Star) where

  -- | Parsed (not yet typechecked) macro. Parameterized by the
  --   annotation type.
  type Parsed l :: Star -> Star

  -- | Typechecked type macro. Parameterized by the variable type.
  type TypecheckedType  l :: Star -> Star

  -- | Typechecked value macro. Parameterized by the variable type.
  type TypecheckedValue l :: Star -> Star

class ( Show (Parsed l ann)
      , Eq   (Parsed l ann)
      ) => ValidParsed l ann

instance ( Show (Parsed l ann)
         , Eq   (Parsed l ann)
         ) => ValidParsed l ann

class ( Show (TypecheckedType l var)
      , Eq   (TypecheckedType l var)
      ) => ValidTypecheckedType l var

instance ( Show (TypecheckedType l var)
         , Eq   (TypecheckedType l var)
         ) => ValidTypecheckedType l var

class ( Show (TypecheckedValue l var)
      , Eq   (TypecheckedValue l var)
      ) => ValidTypecheckedValue l var

instance ( Show (TypecheckedValue l var)
         , Eq   (TypecheckedValue l var)
         ) => ValidTypecheckedValue l var
