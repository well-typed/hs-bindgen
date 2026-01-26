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

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.Array.KnownSize.Mutable
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Eq, Ord, Show)

{-| __C declaration:__ @triplet@

    __defined at:__ @documentation\/data_kind_pragma.h 3:13@

    __exported by:__ @documentation\/data_kind_pragma.h@
-}
newtype Triplet = Triplet
  { un_Triplet :: (HsBindgen.Runtime.Array.KnownSize.Mutable.Array 3) FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Triplet) "un_Triplet")
         ) => GHC.Records.HasField "un_Triplet" (Ptr.Ptr Triplet) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Triplet")

instance HsBindgen.Runtime.HasCField.HasCField Triplet "un_Triplet" where

  type CFieldType Triplet "un_Triplet" =
    (HsBindgen.Runtime.Array.KnownSize.Mutable.Array 3) FC.CInt

  offset# = \_ -> \_ -> 0
