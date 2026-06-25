-- | Hs AST
--
-- From outside the @HsBindgen.IR@ hierarchy, this module should always be used,
-- /not/ submodules.  Submodules may be used within the @HsBindgen.IR@ hierarchy
-- to avoid import cycles.
--
-- Intended for qualified import.
--
-- > import HsBindgen.IR.Hs qualified as Hs
module HsBindgen.IR.Hs (
    module HsBindgen.IR.Hs.Type
  ) where

import HsBindgen.IR.Hs.Type
