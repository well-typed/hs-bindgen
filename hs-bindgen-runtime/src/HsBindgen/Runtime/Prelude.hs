-- | User-facing convenience module, re-exporting definitions required to _use_
--   the generated bindings.
module HsBindgen.Runtime.Prelude (
    -- * CEnum
    CEnum(..)
  , SequentialCEnum(..)
  , AsCEnum(..)
  , AsSequentialCEnum(..)

  , module HsBindgen.Runtime.Marshal

    -- * Read-only pointers
  , PtrConst -- opaque
  ) where

import HsBindgen.Runtime.CEnum
import HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.PtrConst
