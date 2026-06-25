-- | Pluggable macro-language interface
--
-- This module defines the macro language 'Lang' record type, providing the
-- macro-language implementation (parsing, typechecking, translation). Values of
-- the macro language 'Lang' can be provided by separate packages. The default
-- value uses @c-expr-dsl@, and is defined in the user-facing @hs-bindgen@
-- library as 'HsBindgen.Macro.cExprLang'.
--
-- Intended for qualified import.
--
-- @
-- import HsBindgen.Macro.Interface qualified as Macro
-- @
module HsBindgen.Macro.Interface (
    -- * Record
    Lang(..)
    -- * Typecheck result
  , TypecheckResult(..)
  ) where

import Clang.HighLevel.Types

import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.SHs.AST.Expr
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Error
import HsBindgen.Macro.Type qualified as Macro

{-------------------------------------------------------------------------------
  Record
-------------------------------------------------------------------------------}

-- | A macro language: provides parsing, typechecking and translation of C macro
-- bodies.
--
-- The C standard (and any other configuration) is fixed when the macro 'Lang'
-- is constructed; the interface itself is configuration-free.
data Lang (l :: Star) = Lang {
    -- | Parse a single macro from @libclang@ tokens.
    parse ::
         [Token TokenSpelling]
      -> Either MacroParseError (Macro.Unresolved l)

  , resolve ::
         Set C.DeclId
         -- ^ Set of declaration IDs in scope.
      -> Macro.Unresolved l
      -> Either MacroResolutionError (Macro.Resolved l)

    -- | Dependencies of a parsed (not yet typechecked) macro.
    --
    -- The caller supplies name resolvers; the implementation walks the macro
    -- AST.
  , parsedDeps ::
         Macro.Resolved l
      -> [(C.ValOrRef, C.DeclId)]

    -- | Batch-typecheck a sequence of macros.
  , typecheck ::
         [Macro.Resolved l]
      -> Map Text (TypecheckResult l)

    -- | Get dependencies of a typechecked type-like macro body.
  , typecheckedTypeDeps ::
         Macro.TypecheckedType l C.DeclId
      -> [(C.ValOrRef, C.DeclId)]

    -- | Translate a checked type-like macro body to an 'HsType'.
    --
    -- The caller supplies a function to translate variables to 'HsType'.
  , translateType ::
         Macro.TypecheckedType l Hs.Type
      -> Hs.Type

    -- | Translate a checked value-macro to a 'Binding'.
  , translateValue ::
         Hs.Name Hs.NsVar
         -- ^ Exported binding name
      -> Macro.TypecheckedValue l Hs.TermName
      -> Maybe HsDoc.Comment
      -> Binding
  }

{-------------------------------------------------------------------------------
  Typecheck result
-------------------------------------------------------------------------------}

-- | Result of typechecking a single macro.
--
-- @l@ is the macro-language tag, @var@ is the variable type used to represent
-- macro dependencies.
data TypecheckResult l
  = TypecheckType  (Macro.TypecheckedType  l C.DeclId)
  | TypecheckValue (Macro.TypecheckedValue l C.DeclId)
  | TypecheckError MacroTypecheckError

deriving stock instance Macro.HasTypes l => Show (TypecheckResult l)
deriving stock instance Macro.HasTypes l => Eq   (TypecheckResult l)

