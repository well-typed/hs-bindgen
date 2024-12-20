{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.ConstantArray

newtype CTriple = MkCTriple
  { unCTriple :: (HsBindgen.ConstantArray.ConstantArray 3) FC.CInt
  }

deriving newtype instance F.Storable CTriple
