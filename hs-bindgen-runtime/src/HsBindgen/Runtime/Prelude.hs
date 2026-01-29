-- | User-facing convenience module, re-exporting definitions required to _use_
--   the generated bindings.
module HsBindgen.Runtime.Prelude (
    -- * C enumerations
    CEnum(..)
  , SequentialCEnum(..)
  , AsCEnum(..)
  , AsSequentialCEnum(..)

    -- * Fields and bit-fields
  , HasCField(..)
  , HasCBitfield(..)
  , BitfieldPtr (..)

    -- * Function pointers and instances
  , ToFunPtr (..)
  , FromFunPtr (..)
  , withFunPtr

    -- * Pointers
  , plusPtrElem

    -- * Arrays
  , ConstantArray   -- opaque
  , IncompleteArray -- opaque

    -- * Marshaling and serialization
  , StaticSize(..)
  , ReadRaw(..)
  , WriteRaw(..)
  , EquivStorable(..)

    -- * Read-only pointers
  , PtrConst -- type synonym or opaque, depending on version of @base@
  ) where

import HsBindgen.Runtime.BitfieldPtr
import HsBindgen.Runtime.CEnum
import HsBindgen.Runtime.ConstantArray
import HsBindgen.Runtime.FunPtr
import HsBindgen.Runtime.HasCBitfield
import HsBindgen.Runtime.HasCField
import HsBindgen.Runtime.IncompleteArray
import HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.Ptr
import HsBindgen.Runtime.PtrConst
