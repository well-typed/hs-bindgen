{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RPM.Ps where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import Prelude (Eq, Ord, Show)

{-| __C declaration:__ @rpmpsi_s@

    __defined at:__ @rpm\/rpmps.h:20:16@

    __exported by:__ @rpm\/rpmps.h@
-}
data Rpmpsi_s

{-|

  > rpmps

  Problem set iterator

__C declaration:__ @rpmpsi@

__defined at:__ @rpm\/rpmps.h:20:27@

__exported by:__ @rpm\/rpmps.h@
-}
newtype Rpmpsi = Rpmpsi
  { un_Rpmpsi :: Ptr.Ptr Rpmpsi_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)
