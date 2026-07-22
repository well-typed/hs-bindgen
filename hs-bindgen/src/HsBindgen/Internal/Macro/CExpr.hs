{-# OPTIONS_HADDOCK hide #-}

-- | The macro instance of @hs-bindgen@ is based on @c-expr-dsl@.
--
-- Intended for unqualified import.
module HsBindgen.Internal.Macro.CExpr (
    -- * Type
    CExpr -- opaque
    -- * Macro language
  , cExpr -- opaque
  ) where

import Clang.CStandard

import HsBindgen.Internal.Macro.CExpr.Parse
import HsBindgen.Internal.Macro.CExpr.Resolution
import HsBindgen.Internal.Macro.CExpr.Translation.Type
import HsBindgen.Internal.Macro.CExpr.Translation.Value
import HsBindgen.Internal.Macro.CExpr.Type
import HsBindgen.Internal.Macro.CExpr.Typecheck
import HsBindgen.Macro.Interface qualified as Macro

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

