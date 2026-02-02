{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Array.Byte
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import qualified HsBindgen.Runtime.SizedByteArray
import HsBindgen.Runtime.TypeEquality (TyEq)

{-| __C declaration:__ @union MyUnion@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 4:7@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
newtype MyUnion = MyUnion
  { unwrapMyUnion :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance HsBindgen.Runtime.Marshal.StaticSize MyUnion

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance HsBindgen.Runtime.Marshal.ReadRaw MyUnion

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance HsBindgen.Runtime.Marshal.WriteRaw MyUnion

deriving via HsBindgen.Runtime.Marshal.EquivStorable MyUnion instance F.Storable MyUnion

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance Data.Primitive.Types.Prim MyUnion

{-|

  __See:__ 'set_myUnion_x'

__C declaration:__ @x@

__defined at:__ @binding-specs\/fun_arg\/macro\/union.h 4:21@

__exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
get_myUnion_x ::
     MyUnion
  -> FC.CInt
get_myUnion_x =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_myUnion_x'

-}
set_myUnion_x ::
     FC.CInt
  -> MyUnion
set_myUnion_x =
  HsBindgen.Runtime.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField MyUnion "myUnion_x" where

  type CFieldType MyUnion "myUnion_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MyUnion) "myUnion_x")
         ) => GHC.Records.HasField "myUnion_x" (Ptr.Ptr MyUnion) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"myUnion_x")

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 7:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
newtype A = A
  { unwrapA :: MyUnion
  }
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, Data.Primitive.Types.Prim)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "unwrapA")
         ) => GHC.Records.HasField "unwrapA" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapA")

instance HsBindgen.Runtime.HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = MyUnion

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 8:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, Data.Primitive.Types.Prim)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType B) "unwrapB")
         ) => GHC.Records.HasField "unwrapB" (Ptr.Ptr B) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapB")

instance HsBindgen.Runtime.HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0
