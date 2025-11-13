{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

{-| __C declaration:__ @adiós@

    __defined at:__ @adios.h:7:13@

    __exported by:__ @adios.h@
-}
newtype Adio'0301s = Adio'0301s
  { un_Adio'0301s :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Adio'0301s) "un_Adio'0301s")
         ) => GHC.Records.HasField "un_Adio'0301s" (Ptr.Ptr Adio'0301s) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Adio'0301s")

instance HsBindgen.Runtime.HasCField.HasCField Adio'0301s "un_Adio'0301s" where

  type CFieldType Adio'0301s "un_Adio'0301s" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @数字@

    __defined at:__ @adios.h:12:13@

    __exported by:__ @adios.h@
-}
newtype C数字 = C数字
  { un_C数字 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType C数字) "un_C\25968\23383")
         ) => GHC.Records.HasField "un_C\25968\23383" (Ptr.Ptr C数字) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_C\25968\23383")

instance HsBindgen.Runtime.HasCField.HasCField C数字 "un_C\25968\23383" where

  type CFieldType C数字 "un_C\25968\23383" = FC.CInt

  offset# = \_ -> \_ -> 0
