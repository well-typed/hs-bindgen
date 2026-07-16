{-# OPTIONS_HADDOCK hide #-}

-- | The macro instance of @hs-bindgen@ is based on @c-expr-dsl@.
--
-- Intended for qualified import.
--
-- @
-- import HsBindgen.Internal.Macro (CExpr)
-- import HsBindgen.Internal.Macro qualified as Macro
-- @
module HsBindgen.Internal.Macro (
    -- * Type
    CExpr
  , Macro.HasTypes(..)
    -- * Macro language
  , cExpr
  , Macro.Lang(..)
  ) where

import Clang.CStandard

import HsBindgen.Internal.Macro.CExpr
import HsBindgen.Internal.Macro.Parse
import HsBindgen.Internal.Macro.Resolution
import HsBindgen.Internal.Macro.Translation.Type
import HsBindgen.Internal.Macro.Translation.Value
import HsBindgen.Internal.Macro.Typecheck
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro

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

