-- | The macro instance of @hs-bindgen@ is based on @c-expr-dsl@.
--
-- Intended for qualified import.
--
-- @
-- import HsBindgen.Macro (CExpr)
-- import HsBindgen.Macro qualified as Macro
-- @
module HsBindgen.Macro (
    -- * Type
    CExpr
  , Macro.HasTypes(..)
    -- * Macro language
  , cExpr
  , Macro.Lang(..)
  ) where

import Clang.CStandard

import HsBindgen.Macro.CExpr
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Parse
import HsBindgen.Macro.Resolution
import HsBindgen.Macro.Translation.Type
import HsBindgen.Macro.Translation.Value
import HsBindgen.Macro.Type qualified as Macro
import HsBindgen.Macro.Typecheck

-- | Default macro language implementation backed by @c-expr-dsl@.
--
-- The C standard is fixed at construction time; the resulting macro
-- 'Macro.Lang' is configuration-free.

cExpr :: ClangCStandard -> Macro.Lang CExpr
cExpr cStd = Macro.Lang
  { parse               = parseMacro cStd
  , resolve             = resolveMacro
  , typecheck           = typecheckMacros
  , translateType       = translateMacroType
  , translateValue      = translateMacroValue
  }

