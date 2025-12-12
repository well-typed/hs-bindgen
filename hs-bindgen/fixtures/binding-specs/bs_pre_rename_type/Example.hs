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
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.HasCField
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

{-| __C declaration:__ @sym@

    __defined at:__ @binding-specs\/bs_pre_rename_type.h:1:14@

    __exported by:__ @binding-specs\/bs_pre_rename_type.h@
-}
newtype MySym = MySym
  { un_MySym :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MySym) "un_MySym")
         ) => GHC.Records.HasField "un_MySym" (Ptr.Ptr MySym) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_MySym")

instance HsBindgen.Runtime.HasCField.HasCField MySym "un_MySym" where

  type CFieldType MySym "un_MySym" = FC.CChar

  offset# = \_ -> \_ -> 0
