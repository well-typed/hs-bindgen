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
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude (Eq, Show)

{-| __C declaration:__ @triplet@

    __defined at:__ @documentation\/data_kind_pragma.h 3:13@

    __exported by:__ @documentation\/data_kind_pragma.h@
-}
newtype Triplet = Triplet
  { unwrapTriplet :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Triplet) "unwrapTriplet")
         ) => GHC.Records.HasField "unwrapTriplet" (Ptr.Ptr Triplet) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTriplet")

instance HsBindgen.Runtime.HasCField.HasCField Triplet "unwrapTriplet" where

  type CFieldType Triplet "unwrapTriplet" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 0
