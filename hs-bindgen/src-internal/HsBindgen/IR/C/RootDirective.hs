-- | Root header directives
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.C" should be used.
--
-- Within @HsBindgen.IR@, all modules aside from "HsBindgen.IR.C" should import
-- this module qualified for consistency.
--
-- > import HsBindgen.IR.C.RootDirective qualified as C
module HsBindgen.IR.C.RootDirective (
    -- * RootDirective
    RootDirective(..)
  , UncheckedRootDirective
  , hashIncludeArgsOf
    -- * Rendering
  , renderRootDirectives
  ) where

import HsBindgen.Imports
import HsBindgen.IR.C.HashDefine (HashDefine, hashDefineToDirective)
import HsBindgen.IR.C.HashIncludeArg (HashIncludeArg (..),
                                      UncheckedHashIncludeArg)

{-------------------------------------------------------------------------------
  RootDirective
-------------------------------------------------------------------------------}

-- | A directive of the root header
--
-- The user states @#include@s and @#define@s as a single ordered list, because
-- the order matters: a @#define@ only affects the headers included after it.
--
-- The parameter is the @#include@ argument, which has an unchecked (as given by
-- the user) and a checked form; @#define@s need no checking.
data RootDirective arg =
    DirectiveHashInclude arg
  | DirectiveHashDefine  HashDefine
  deriving stock (Show, Eq, Generic, Functor, Foldable, Traversable)

-- | Root directive with an unchecked @#include@ argument
type UncheckedRootDirective = RootDirective UncheckedHashIncludeArg

-- | The @#include@ arguments, in order, discarding the @#define@s
--
-- For consumers that genuinely only care about the headers.
hashIncludeArgsOf :: [RootDirective arg] -> [arg]
hashIncludeArgsOf = concatMap toList

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

-- | Render root directives, one per line, in order
--
-- This is the single source of truth for the C source seen by /all/ C stages:
-- the root header parsed by @libclang@, the generated CAPI wrapper source
-- compiled by GHC, and the generated C test source.
renderRootDirectives :: [RootDirective HashIncludeArg] -> String
renderRootDirectives = unlines . map render
  where
    render :: RootDirective HashIncludeArg -> String
    render = \case
      DirectiveHashInclude arg -> "#include <" ++ arg.path ++ ">"
      DirectiveHashDefine  d   -> hashDefineToDirective d
