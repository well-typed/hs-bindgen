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
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @union U@

    __defined at:__ @functions\/heap_types\/union_const_typedef.h 3:7@

    __exported by:__ @functions\/heap_types\/union_const_typedef.h@
-}
newtype T = T
  { unwrapT :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize T

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw T

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw T

deriving via Marshal.EquivStorable T instance BG.Storable T

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion T

{-| __C declaration:__ @x@

    __defined at:__ @functions\/heap_types\/union_const_typedef.h 4:7@

    __exported by:__ @functions\/heap_types\/union_const_typedef.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "t_x" T ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @functions\/heap_types\/union_const_typedef.h 4:7@

    __exported by:__ @functions\/heap_types\/union_const_typedef.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "t_x" T ty where

  hasField =
    \x0 -> (BG.setUnionPayload, BG.getField @"t_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "t_x" (BG.Ptr T) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t_x")

instance HasCField.HasCField T "t_x" where

  type CFieldType T "t_x" = BG.CInt

  offset# = \_ -> \_ -> 0
