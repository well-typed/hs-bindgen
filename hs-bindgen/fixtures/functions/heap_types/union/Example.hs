{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
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

{-| __C declaration:__ @union U@

    __defined at:__ @functions\/heap_types\/union.h 3:7@

    __exported by:__ @functions\/heap_types\/union.h@
-}
newtype T = T
  { unwrapT :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize T

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw T

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw T

deriving via Marshal.EquivStorable T instance RIP.Storable T

{-|

  __See:__ 'set_t_x'

__C declaration:__ @x@

__defined at:__ @functions\/heap_types\/union.h 4:7@

__exported by:__ @functions\/heap_types\/union.h@
-}
get_t_x ::
     T
  -> RIP.CInt
get_t_x = RIP.getUnionPayload

{-|

  __See:__ 'get_t_x'

-}
set_t_x ::
     RIP.CInt
  -> T
set_t_x = RIP.setUnionPayload

instance HasCField.HasCField T "t_x" where

  type CFieldType T "t_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "t_x" (RIP.Ptr T) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"t_x")
