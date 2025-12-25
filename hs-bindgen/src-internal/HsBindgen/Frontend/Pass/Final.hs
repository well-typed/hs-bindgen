-- | Working with the frontend AST after the final pass
--
-- Intended for unqualified import.
module HsBindgen.Frontend.Pass.Final (
    Final
  ) where

import HsBindgen.Frontend.Pass.MangleNames.IsPass

-- | Final frontend pass
--
-- Backend passes should refer to 'Final' instead of the actual name of the
-- final pass, so that if we add more passes, the backend is unaffected.
type Final = MangleNames

