{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC

newtype Myint = Myint
  { unMyint :: FC.CInt
  }

deriving newtype instance F.Storable Myint

newtype Intptr = Intptr
  { unIntptr :: F.Ptr FC.CInt
  }

deriving newtype instance F.Storable Intptr
