module HsBindgen.Macro (
    -- * Type
    CExpr
  , HasMacroTypes(..)
    -- * Macro language
  , cExprLang
  , MacroLang(..)
  ) where

import Clang.CStandard

import HsBindgen.Macro.CExpr
import HsBindgen.Macro.Interface
import HsBindgen.Macro.Parse
import HsBindgen.Macro.Translation.Type
import HsBindgen.Macro.Translation.Value
import HsBindgen.Macro.Type
import HsBindgen.Macro.Typecheck

-- | Default macro language implementation backed by @c-expr-dsl@.
--
-- The C standard is fixed at construction time; the resulting 'MacroLang' is
-- configuration-free.
cExprLang :: ClangCStandard -> MacroLang CExpr
cExprLang cStd = MacroLang
  { parseMacroBody           = parseMacroBody cStd
  , parsedMacroDeps          = parsedMacroDeps
  , typecheckMacroBodies     = typecheckMacroBodies
  , typecheckedMacroTypeDeps = typecheckedMacroTypeDeps
  , translateMacroType       = translateMacroType
  , translateMacroValue      = translateMacroValue
  }

