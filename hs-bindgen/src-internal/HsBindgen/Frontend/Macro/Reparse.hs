-- | Reparse parts of the C input
--
-- We mostly rely on @libclang@ to parse C for us, but in some cases we need
-- to reparse parts of the input, mostly to deal with macros.
--
-- Intended for unqualified import.
module HsBindgen.Frontend.Macro.Reparse (
    -- * Definition
    Reparse
  , ReparseError(..)
  , reparseWith
    -- * Specific parsers
  , reparseFieldDecl
  , reparseFunDecl
  , reparseTypedef
  , reparseMacro
  ) where

import HsBindgen.Frontend.Macro.Reparse.Decl
import HsBindgen.Frontend.Macro.Reparse.Infra
import HsBindgen.Frontend.Macro.Reparse.Macro
