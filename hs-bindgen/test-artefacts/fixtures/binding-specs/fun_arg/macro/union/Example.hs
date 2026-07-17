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
    ( Example.MyUnion(..)
    , Example.A(..)
    , Example.B(..)
    , Example.E(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union
import qualified M

{-| __C declaration:__ @union MyUnion@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 5:7@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
newtype MyUnion = MyUnion
  { unwrapMyUnion :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize MyUnion

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw MyUnion

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw MyUnion

deriving via Marshal.EquivStorable MyUnion instance BG.Storable MyUnion

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion MyUnion

{-| __C declaration:__ @x@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 5:21@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "myUnion_x" MyUnion ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 5:21@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "myUnion_x" MyUnion ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"myUnion_x" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "myUnion_x" (BG.Ptr MyUnion) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"myUnion_x")

instance HasCField.HasCField MyUnion "myUnion_x" where

  type CFieldType MyUnion "myUnion_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro A@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 9:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
newtype A = A
  { unwrapA :: MyUnion
  }
  deriving stock (BG.Generic)
  deriving newtype
    ( Union.IsUnion
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ MyUnion) => BG.CompatHasField.HasField "unwrapA" A ty where

  hasField =
    \x0 ->
      (\y1 -> A {unwrapA = y1}, BG.getField @"unwrapA" x0)

instance (ty ~ MyUnion) => BG.HasField "unwrapA" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = MyUnion

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 10:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving stock (BG.Generic)
  deriving newtype
    ( Union.IsUnion
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ A) => BG.CompatHasField.HasField "unwrapB" B ty where

  hasField =
    \x0 ->
      (\y1 -> B {unwrapB = y1}, BG.getField @"unwrapB" x0)

instance (ty ~ A) => BG.HasField "unwrapB" (BG.Ptr B) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapB")

instance HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro E@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 31:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
newtype E = E
  { unwrapE :: M.C
  }
  deriving stock (BG.Generic)

instance (ty ~ M.C) => BG.CompatHasField.HasField "unwrapE" E ty where

  hasField =
    \x0 ->
      (\y1 -> E {unwrapE = y1}, BG.getField @"unwrapE" x0)

instance (ty ~ M.C) => BG.HasField "unwrapE" (BG.Ptr E) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapE")

instance HasCField.HasCField E "unwrapE" where

  type CFieldType E "unwrapE" = M.C

  offset# = \_ -> \_ -> 0
