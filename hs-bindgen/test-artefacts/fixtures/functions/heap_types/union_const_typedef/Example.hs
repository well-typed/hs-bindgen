{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.U(..)
    , Example.T(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Internal.Prelude.CompatHasField as RIP.CompatHasField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @union U@

    __defined at:__ @functions\/heap_types\/union_const_typedef.h 3:7@

    __exported by:__ @functions\/heap_types\/union_const_typedef.h@
-}
newtype U = U
  { unwrapU :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize U

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw U

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw U

deriving via Marshal.EquivStorable U instance RIP.Storable U

deriving via RIP.SizedByteArray 4 4 instance Union.IsUnion U

{-| __C declaration:__ @x@

    __defined at:__ @functions\/heap_types\/union_const_typedef.h 4:7@

    __exported by:__ @functions\/heap_types\/union_const_typedef.h@
-}
instance (ty ~ RIP.CInt) => RIP.HasField "u_x" U ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @functions\/heap_types\/union_const_typedef.h 4:7@

    __exported by:__ @functions\/heap_types\/union_const_typedef.h@
-}
instance (ty ~ RIP.CInt) => RIP.CompatHasField.HasField "u_x" U ty where

  hasField =
    \x0 -> (RIP.setUnionPayload, RIP.getField @"u_x" x0)

instance (ty ~ RIP.CInt) => RIP.HasField "u_x" (RIP.Ptr U) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"u_x")

instance HasCField.HasCField U "u_x" where

  type CFieldType U "u_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T@

    __defined at:__ @functions\/heap_types\/union_const_typedef.h 7:23@

    __exported by:__ @functions\/heap_types\/union_const_typedef.h@
-}
newtype T = T
  { unwrapT :: U
  }
  deriving stock (RIP.Generic)
  deriving newtype
    ( Union.IsUnion
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ U) => RIP.CompatHasField.HasField "unwrapT" T ty where

  hasField =
    \x0 ->
      (\y1 -> T {unwrapT = y1}, RIP.getField @"unwrapT" x0)

instance (ty ~ U) => RIP.HasField "unwrapT" (RIP.Ptr T) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT")

instance HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" = U

  offset# = \_ -> \_ -> 0
