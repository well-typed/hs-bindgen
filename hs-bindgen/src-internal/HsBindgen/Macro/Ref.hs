-- | C name references used for dependency tracking in macros.
--
-- Intended unqualified import.
module HsBindgen.Macro.Ref (
    Ref(..)
  ) where

import HsBindgen.Frontend.Naming (CTagKind)
import HsBindgen.Imports

-- | A reference to a C name: either a bare identifier or a tagged type.
--
-- Used for dependency tracking in parsed macros
-- ('HsBindgen.Macro.Handler.Parsed') and in type expressions
-- ('HsBindgen.Macro.Type.Expr').
data Ref =
    -- | Bare name (typedef, variable, or macro reference).
    NameRef Text
    -- | Tagged type reference (e.g. @struct Foo@, @union Bar@).
  | TaggedRef CTagKind Text
  deriving stock (Show, Eq, Ord)
