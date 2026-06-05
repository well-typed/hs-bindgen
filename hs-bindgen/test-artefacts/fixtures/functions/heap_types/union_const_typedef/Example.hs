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
    ( Example.T(..)
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
newtype T = T
  { unwrapT :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize T

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw T

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw T

deriving via Marshal.EquivStorable T instance RIP.Storable T

deriving via RIP.SizedByteArray 4 4 instance Union.IsUnion T

{-| __C declaration:__ @x@

    __defined at:__ @functions\/heap_types\/union_const_typedef.h 4:7@

    __exported by:__ @functions\/heap_types\/union_const_typedef.h@
-}
instance (ty ~ RIP.CInt) => RIP.HasField "t_x" T ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @functions\/heap_types\/union_const_typedef.h 4:7@

    __exported by:__ @functions\/heap_types\/union_const_typedef.h@
-}
instance (ty ~ RIP.CInt) => RIP.CompatHasField.HasField "t_x" T ty where

  hasField =
    \x0 -> (RIP.setUnionPayload, RIP.getField @"t_x" x0)

instance (ty ~ RIP.CInt) => RIP.HasField "t_x" (RIP.Ptr T) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"t_x")

instance HasCField.HasCField T "t_x" where

  type CFieldType T "t_x" = RIP.CInt

  offset# = \_ -> \_ -> 0
