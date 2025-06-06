-- | Reparse parts of the C input
--
-- We mostly rely on @libclang@ to parse C for us, but in some cases we need
-- to reparse parts of the input, mostly to deal with macros.
--
-- Intended for unqualified import.
module HsBindgen.C.Reparse (
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

import HsBindgen.C.Reparse.Decl
import HsBindgen.C.Reparse.Infra
import HsBindgen.C.Reparse.Macro
