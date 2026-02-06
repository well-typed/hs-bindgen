{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct struct2@

    __defined at:__ @types\/special\/parse_failure_long_double.h 13:8@

    __exported by:__ @types\/special\/parse_failure_long_double.h@
-}
data Struct2 = Struct2
  { struct2_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/special\/parse_failure_long_double.h 14:7@

         __exported by:__ @types\/special\/parse_failure_long_double.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Struct2 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Struct2 where

  readRaw =
    \ptr0 ->
          pure Struct2
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"struct2_x") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Struct2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2 struct2_x2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"struct2_x") ptr0 struct2_x2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Struct2 instance F.Storable Struct2

instance HsBindgen.Runtime.HasCField.HasCField Struct2 "struct2_x" where

  type CFieldType Struct2 "struct2_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct2) "struct2_x")
         ) => GHC.Records.HasField "struct2_x" (Ptr.Ptr Struct2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"struct2_x")
