{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified M

{-| __C declaration:__ @union MyUnion@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/union.h 4:7@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/union.h@
-}
newtype MyUnion = MyUnion
  { unwrapMyUnion :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize MyUnion

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw MyUnion

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw MyUnion

deriving via Marshal.EquivStorable MyUnion instance RIP.Storable MyUnion

{-|

  __See:__ 'set_myUnion_x'

__C declaration:__ @x@

__defined at:__ @binding-specs\/fun_arg\/typedef\/union.h 4:21@

__exported by:__ @binding-specs\/fun_arg\/typedef\/union.h@
-}
get_myUnion_x ::
     MyUnion
  -> RIP.CInt
get_myUnion_x = RIP.getUnionPayload

{-|

  __See:__ 'get_myUnion_x'

-}
set_myUnion_x ::
     RIP.CInt
  -> MyUnion
set_myUnion_x = RIP.setUnionPayload

instance HasCField.HasCField MyUnion "myUnion_x" where

  type CFieldType MyUnion "myUnion_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "myUnion_x" (RIP.Ptr MyUnion) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"myUnion_x")

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/union.h 7:23@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/union.h@
-}
newtype A = A
  { unwrapA :: MyUnion
  }
  deriving stock (RIP.Generic)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) MyUnion
         ) => RIP.HasField "unwrapA" (RIP.Ptr A) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = MyUnion

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/union.h 8:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/union.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving stock (RIP.Generic)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance (((~) ty) A) => RIP.HasField "unwrapB" (RIP.Ptr B) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapB")

instance HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @E@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/union.h 21:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/union.h@
-}
newtype E = E
  { unwrapE :: M.C
  }
  deriving stock (RIP.Generic)

instance (((~) ty) M.C) => RIP.HasField "unwrapE" (RIP.Ptr E) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE")

instance HasCField.HasCField E "unwrapE" where

  type CFieldType E "unwrapE" = M.C

  offset# = \_ -> \_ -> 0
