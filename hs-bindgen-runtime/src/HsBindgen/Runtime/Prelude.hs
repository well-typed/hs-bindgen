-- | Common definitions for interfacing with @hs-bindgen@ generated code.
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
  , safeCastFunPtr

    -- * Arrays
  , ConstantArray   -- opaque
  , IncompleteArray -- opaque
  , IsArray (Elem)

    -- * Marshaling and serialization
  , StaticSize(..)
  , ReadRaw(..)
  , WriteRaw(..)
  , EquivStorable(..)

    -- Blocks
  , Block(..)

    -- * Read-only pointers
  , PtrConst -- type synonym or opaque, depending on version of @base@
  ) where

import HsBindgen.Runtime.BitfieldPtr
import HsBindgen.Runtime.Block
import HsBindgen.Runtime.CEnum
import HsBindgen.Runtime.ConstantArray
import HsBindgen.Runtime.HasCBitfield
import HsBindgen.Runtime.HasCField
import HsBindgen.Runtime.IncompleteArray
import HsBindgen.Runtime.Internal.FunPtr
import HsBindgen.Runtime.Internal.Ptr
import HsBindgen.Runtime.IsArray
import HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.PtrConst
