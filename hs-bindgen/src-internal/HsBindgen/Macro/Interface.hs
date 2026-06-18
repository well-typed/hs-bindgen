-- | Pluggable macro-language interface
--
-- This module defines the 'MacroLang' record type, providing the macro-language
-- implementation (parsing, typechecking, translation). Values of 'MacroLang'
-- can be provided by separate packages. The default value uses @c-expr-dsl@,
-- and is defined in the user-facing @hs-bindgen@ library as
-- 'HsBindgen.Macro.cExprLang'.
module HsBindgen.Macro.Interface (
    -- * Record
    MacroLang(..)
    -- * Typecheck result
  , MacroTypecheckResult(..)
    -- * Errors
  , MacroTypecheckError(..)
  , MacroLangParseError(..)
  , MacroLangTypecheckError(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types

import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.SHs.AST.Expr
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Type
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Record
-------------------------------------------------------------------------------}

-- | A macro language: provides parsing, typechecking and translation of C macro
-- bodies.
--
-- The C standard (and any other configuration) is fixed when the 'MacroLang'
-- is constructed; the interface itself is configuration-free.
data MacroLang (l :: Star) = MacroLang {
    -- | Parse a single macro from @libclang@ tokens.
    parseMacroBody ::
         [Token TokenSpelling]
      -> Either MacroLangParseError (ParsedMacroBody l)

    -- | Dependencies of a parsed (not yet typechecked) macro.
    --
    -- The caller supplies name resolvers; the implementation walks the macro
    -- AST.
  , parsedMacroDeps ::
         Set C.DeclId
         -- ^ Declarations in scope
      -> ParsedMacroBody l
      -> [(C.ValOrRef, C.DeclId)]

    -- | Batch-typecheck a sequence of macros.
  , typecheckMacroBodies ::
         Set C.DeclId
         -- ^ Declarations in scope
      -> [ParsedMacroBody l]
      -> Map Text (MacroTypecheckResult l)

    -- | Get dependencies of a typechecked type-like macro body.
  , typecheckedMacroTypeDeps ::
         TypecheckedMacroTypeBody l C.DeclId
      -> [(C.ValOrRef, C.DeclId)]

    -- | Translate a checked type-like macro body to an 'HsType'.
    --
    -- The caller supplies a function to translate variables to 'HsType'.
  , translateMacroType ::
         TypecheckedMacroTypeBody l HsType
      -> HsType

    -- | Translate a checked value-macro to a 'Binding'.
  , translateMacroValue ::
         Hs.Name Hs.NsVar
         -- ^ Exported binding name
      -> TypecheckedMacroValueBody l Hs.TermName
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
data MacroTypecheckResult l
  = MacroTypecheckType  (TypecheckedMacroTypeBody  l C.DeclId)
  | MacroTypecheckValue (TypecheckedMacroValueBody l C.DeclId)
  | MacroTypecheckError MacroTypecheckError

deriving stock instance HasMacroTypes l => Show (MacroTypecheckResult l)
deriving stock instance HasMacroTypes l => Eq   (MacroTypecheckResult l)

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data MacroTypecheckError =
    MacroTypecheckTypecheckError       MacroLangTypecheckError
  | MacroTypecheckUnresolvedTaggedType C.DeclId
  deriving stock (Show, Eq)

instance PrettyForTrace MacroTypecheckError where
  prettyForTrace = \case
      MacroTypecheckTypecheckError err -> PP.hsep [
          "Failed to typecheck macro:"
        , PP.string err.macroTypecheckError
        ]
      MacroTypecheckUnresolvedTaggedType declId -> PP.hsep [
          "Macro type references unknown tagged type:"
        , prettyForTrace declId
        ]

instance IsTrace Level MacroTypecheckError where
  getDefaultLogLevel = \case
    MacroTypecheckTypecheckError{}       -> Info
    MacroTypecheckUnresolvedTaggedType{} -> Warning
  getSource          = const HsBindgen
  getTraceId         = const "macro-typecheck"

-- | An opaque parse error from the macro-language backend.
newtype MacroLangParseError = MacroLangParseError { macroParseError :: String }
  deriving stock (Eq, Show, Generic)

-- | An opaque typecheck error from the macro-language backend.
newtype MacroLangTypecheckError = MacroLangTypecheckError { macroTypecheckError :: String }
  deriving stock (Eq, Show, Generic)
