{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC

newtype CMyint = MkCMyint
  { unCMyint :: FC.CInt
  }

deriving newtype instance F.Storable CMyint

newtype CIntptr = MkCIntptr
  { unCIntptr :: F.Ptr FC.CInt
  }

deriving newtype instance F.Storable CIntptr
