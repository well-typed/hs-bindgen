{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.U(..)
    , Example.get_u_x
    , Example.set_u_x
    , Example.S(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @union U@

    __defined at:__ @attributes\/visibility\/edge-cases\/nested_types.h 3:49@

    __exported by:__ @attributes\/visibility\/edge-cases\/nested_types.h@
-}
newtype U = U
  { unwrapU :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize U

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw U

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw U

deriving via Marshal.EquivStorable U instance RIP.Storable U

{-|

    __See:__ 'set_u_x'

    __C declaration:__ @x@

    __defined at:__ @attributes\/visibility\/edge-cases\/nested_types.h 4:9@

    __exported by:__ @attributes\/visibility\/edge-cases\/nested_types.h@
-}
get_u_x ::
     U
  -> RIP.CInt
get_u_x = RIP.getUnionPayload

{-|

    __See:__ 'get_u_x'

-}
set_u_x ::
     RIP.CInt
  -> U
set_u_x = RIP.setUnionPayload

instance HasCField.HasCField U "u_x" where

  type CFieldType U "u_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "u_x" (RIP.Ptr U) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"u_x")

{-| __C declaration:__ @struct S@

    __defined at:__ @attributes\/visibility\/edge-cases\/nested_types.h 2:49@

    __exported by:__ @attributes\/visibility\/edge-cases\/nested_types.h@
-}
data S = S
  { s_y :: U
    {- ^ __C declaration:__ @y@

         __defined at:__ @attributes\/visibility\/edge-cases\/nested_types.h 5:5@

         __exported by:__ @attributes\/visibility\/edge-cases\/nested_types.h@
    -}
  }
  deriving stock (RIP.Generic)

instance Marshal.StaticSize S where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HasCField.readRaw (RIP.Proxy @"s_y") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_y2 ->
            HasCField.writeRaw (RIP.Proxy @"s_y") ptr0 s_y2

deriving via Marshal.EquivStorable S instance RIP.Storable S

instance HasCField.HasCField S "s_y" where

  type CFieldType S "s_y" = U

  offset# = \_ -> \_ -> 0

instance (((~) ty) U) => RIP.HasField "s_y" (RIP.Ptr S) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s_y")
