-- | User-facing convenience module, re-exporting definitions required to _use_
--   the generated bindings.
module HsBindgen.Runtime.Prelude (
  -- * C enumerations
    CEnum(..)
  , SequentialCEnum(..)
  , AsCEnum(..)
  , AsSequentialCEnum(..)

  -- * Function pointers and instances
  , ToFunPtr (..)
  , FromFunPtr (..)
  , withToFunPtr
  , module HsBindgen.Runtime.TH.Instances

  -- * C arrays
  -- ** Unknown size
  , IncompleteArray

  -- ** Known, constant size
  , ConstantArray

  -- * Marshaling and serialization
  , StaticSize(..)
  , ReadRaw(..)
  , WriteRaw(..)
  , EquivStorable(..)

    -- * Read-only pointers
  , PtrConst -- opaque
  ) where

import HsBindgen.Runtime.CEnum
import HsBindgen.Runtime.ConstantArray
import HsBindgen.Runtime.FunPtr
import HsBindgen.Runtime.IncompleteArray
import HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.PtrConst
import HsBindgen.Runtime.TH.Instances
