-- | User-facing convenience module, re-exporting definitions required to _use_
--   the generated bindings.
module HsBindgen.Runtime.Prelude (
    -- * CEnum
    CEnum(..)
  , SequentialCEnum(..)
  , AsCEnum(..)
  , AsSequentialCEnum(..)

  , module HsBindgen.Runtime.Marshal

    -- TODO: https://github.com/well-typed/hs-bindgen/pull/1563.
  , module HsBindgen.Runtime.ConstPtr
  ) where

import HsBindgen.Runtime.CEnum
import HsBindgen.Runtime.ConstPtr
import HsBindgen.Runtime.Marshal
