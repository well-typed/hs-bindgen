{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct S@

    __defined at:__ @functions\/heap_types\/struct_const_typedef.h 3:8@

    __exported by:__ @functions\/heap_types\/struct_const_typedef.h@
-}
data S = S
  { s_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @functions\/heap_types\/struct_const_typedef.h 4:7@

         __exported by:__ @functions\/heap_types\/struct_const_typedef.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize S where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s_x") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_x2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s_x") ptr0 s_x2

deriving via HsBindgen.Runtime.Marshal.EquivStorable S instance F.Storable S

instance HsBindgen.Runtime.HasCField.HasCField S "s_x" where

  type CFieldType S "s_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "s_x" (Ptr.Ptr S) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s_x")

{-| __C declaration:__ @T@

    __defined at:__ @functions\/heap_types\/struct_const_typedef.h 7:24@

    __exported by:__ @functions\/heap_types\/struct_const_typedef.h@
-}
newtype T = T
  { unwrapT :: S
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance GHC.Records.HasField "unwrapT" (Ptr.Ptr T) (Ptr.Ptr S) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapT")

instance HsBindgen.Runtime.HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" = S

  offset# = \_ -> \_ -> 0
