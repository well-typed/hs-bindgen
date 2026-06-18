-- | C AST
--
-- From outside the @HsBindgen.IR@ hierarchy, this module should always be used,
-- /not/ submodules.  Submodules need to be used within the @HsBindgen.IR@
-- hierarchy to avoid import cycles.
--
-- Intended for qualified import.
--
-- > import HsBindgen.IR.C qualified as C
module HsBindgen.IR.C (
    module HsBindgen.IR.C.Decl
  , module HsBindgen.IR.C.HashIncludeArg
  , module HsBindgen.IR.C.LocationInfo
  , module HsBindgen.IR.C.Naming
  , module HsBindgen.IR.C.PrettyPrinter
  , module HsBindgen.IR.C.Type
  ) where

import HsBindgen.IR.C.Decl
import HsBindgen.IR.C.HashIncludeArg
import HsBindgen.IR.C.LocationInfo
import HsBindgen.IR.C.Naming
import HsBindgen.IR.C.PrettyPrinter
import HsBindgen.IR.C.Type
