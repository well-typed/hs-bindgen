module HsBindgen.TestLib.RepZero (
    -- * RepZero
    RepZero(..)
  ) where

import Foreign.C qualified as FC

{-------------------------------------------------------------------------------
  RepZero
-------------------------------------------------------------------------------}

class RepZero a where
  repZero :: a

instance RepZero FC.CChar where
  repZero = FC.CChar 0

instance RepZero FC.CSChar where
  repZero = FC.CSChar 0

instance RepZero FC.CUChar where
  repZero = FC.CUChar 0

instance RepZero FC.CShort where
  repZero = FC.CShort 0

instance RepZero FC.CInt where
  repZero = FC.CInt 0

instance RepZero FC.CUInt where
  repZero = FC.CUInt 0

instance RepZero FC.CLong where
  repZero = FC.CLong 0

instance RepZero FC.CULong where
  repZero = FC.CULong 0

instance RepZero FC.CPtrdiff where
  repZero = FC.CPtrdiff 0

instance RepZero FC.CSize where
  repZero = FC.CSize 0

instance RepZero FC.CWchar where
  repZero = FC.CWchar 0

instance RepZero FC.CSigAtomic where
  repZero = FC.CSigAtomic 0

instance RepZero FC.CLLong where
  repZero = FC.CLLong 0

instance RepZero FC.CULLong where
  repZero = FC.CULLong 0

instance RepZero FC.CBool where
  repZero = FC.CBool 0

instance RepZero FC.CIntPtr where
  repZero = FC.CIntPtr 0

instance RepZero FC.CUIntPtr where
  repZero = FC.CUIntPtr 0

instance RepZero FC.CIntMax where
  repZero = FC.CIntMax 0

instance RepZero FC.CUIntMax where
  repZero = FC.CUIntMax 0

instance RepZero FC.CClock where
  repZero = FC.CClock 0

instance RepZero FC.CTime where
  repZero = FC.CTime 0

instance RepZero FC.CFloat where
  repZero = FC.CFloat 0

instance RepZero FC.CDouble where
  repZero = FC.CDouble 0
