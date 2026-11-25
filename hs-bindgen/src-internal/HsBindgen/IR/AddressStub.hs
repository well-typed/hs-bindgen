-- | The address stub of a global or function
--
-- Intended for unqualified import.
module HsBindgen.IR.AddressStub (
    AddressStub(..)
  ) where

import HsBindgen.IR.Hs qualified as Hs

-- | The address of a global or function, as its address stub returns it
--
-- An address stub is a C function returning the address; the backend binds to
-- it with a foreign import in 'IO'.
data AddressStub = AddressStub {
      -- | 'Hs.Ptr', 'Hs.PtrConst' or 'Hs.FunPtr'
      typ     :: Hs.Type
    , ffiType :: Hs.FFIType
    }
  deriving stock (Show, Eq)
