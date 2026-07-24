-- | Pluggable macro-language interface
--
-- This module defines the macro language 'Lang' record type, providing the
-- macro-language implementation (parsing, name resolution, typechecking,
-- translation). Values of the macro language 'Lang' can be provided by separate
-- packages. The default value uses @c-expr-dsl@, and is defined in the
-- user-facing @hs-bindgen@ library as 'HsBindgen.Macro.cExpr'.
--
-- Intended for qualified import.
--
-- @
-- import HsBindgen.Macro.Interface qualified as Macro
-- @
module HsBindgen.Macro.Interface (
    -- * Record
    Lang(..)
    -- * Name resolution
  , Unresolved(..)
  , Resolved(..)
    -- * Typecheck result
  , TypecheckResult(..)
  ) where

import Clang.HighLevel.Types

import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.SHs.AST.Expr
import HsBindgen.Frontend.Analysis
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Error
import HsBindgen.Macro.Type

{-------------------------------------------------------------------------------
  Record
-------------------------------------------------------------------------------}

-- | A macro language: provides parsing, name resolution, typechecking and
-- translation of C macros.
--
-- The C standard (and any other configuration) is fixed when the macro 'Lang'
-- is constructed; the interface itself is configuration-free.
--
-- Note how the macro language resembles the stages of compilers: parse,
-- resolve, typecheck, translate.
data Lang (l :: Star) = Lang {
    -- | Parse a single macro from @libclang@ tokens.
    parse ::
         [Token TokenSpelling]
      -> Either MacroParseError (Unresolved l)

  , resolve ::
         Set C.DeclId
         -- ^ Set of declaration IDs in scope.
      -> Unresolved l
      -> Either MacroResolutionError (Resolved l)

    -- | Batch-typecheck a sequence of macros.
    --
    -- The resulting map must have keys for all resolved macros.
  , typecheck ::
         [Resolved l]
      -> Map Text (TypecheckResult l)

    -- | Translate a checked type-like macro to an 'HsType'.
    --
    -- The caller supplies a function to translate variables to 'HsType'.
  , translateType ::
         TypecheckedType l Hs.Type
      -> Hs.Type

    -- | Translate a checked value-macro to a 'Binding'.
  , translateValue ::
         Hs.Name Hs.NsVar
         -- ^ Exported binding name
      -> TypecheckedValue l Hs.TermName
      -> Maybe HsDoc.Comment
      -> Binding
  }

{-------------------------------------------------------------------------------
  Name resolution
-------------------------------------------------------------------------------}

newtype Unresolved l = Unresolved { unwrap :: Parsed l () }

deriving stock instance (HasTypes l) => Eq   (Unresolved l)
deriving stock instance (HasTypes l) => Show (Unresolved l)

data Resolved l = Resolved {
    -- | The macro, with references resolved to 'C.DeclId's.
    macro :: Parsed l C.DeclId
    -- | Dependencies of the macro, paired with what the dependent needs to know
    -- about each dependency (full shape vs. name only).
  , deps  :: [(C.DeclId, Dependency)]
  }
  deriving stock (Generic)

deriving stock instance (HasTypes l) => Eq   (Resolved l)
deriving stock instance (HasTypes l) => Show (Resolved l)

{-------------------------------------------------------------------------------
  Typecheck result
-------------------------------------------------------------------------------}

-- | Result of typechecking a single macro.
--
-- @l@ is the macro-language tag.
data TypecheckResult l
  = TypecheckType  (TypecheckedType l C.DeclId)
  | TypecheckValue (TypecheckedValue l C.DeclId)
  | TypecheckError MacroTypecheckError

deriving stock instance HasTypes l => Show (TypecheckResult l)
deriving stock instance HasTypes l => Eq   (TypecheckResult l)
