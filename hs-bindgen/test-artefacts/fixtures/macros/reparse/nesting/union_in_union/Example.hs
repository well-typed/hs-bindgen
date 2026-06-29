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
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.MyInt(..)
    , Example.T1_x(..)
    , Example.T1(..)
    , Example.T2(..)
    , Example.T2_x(..)
    , Example.T3(..)
    , Example.T3_x(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Internal.Prelude.CompatHasField as RIP.CompatHasField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @macro MyInt@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 1:9@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
newtype MyInt = MyInt
  { unwrapMyInt :: RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.Bitfield
    , RIP.Bits
    , Bounded
    , Enum
    , RIP.FiniteBits
    , RIP.HasFFIType
    , Integral
    , RIP.Ix
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "unwrapMyInt" MyInt ty where

  hasField =
    \x0 ->
      (\y1 ->
         MyInt {unwrapMyInt = y1}, RIP.getField @"unwrapMyInt" x0)

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "unwrapMyInt" (RIP.Ptr MyInt) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMyInt")

instance HasCField.HasCField MyInt "unwrapMyInt" where

  type CFieldType MyInt "unwrapMyInt" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@T1_x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 3:9@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
newtype T1_x = T1_x
  { unwrapT1_x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize T1_x

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw T1_x

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw T1_x

deriving via Marshal.EquivStorable T1_x instance RIP.Storable T1_x

deriving via RIP.SizedByteArray 4 4 instance Union.IsUnion T1_x

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 3:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
instance (ty ~ MyInt) => RIP.HasField "t1_x_x" T1_x ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 3:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
instance (ty ~ MyInt) => RIP.CompatHasField.HasField "t1_x_x" T1_x ty where

  hasField =
    \x0 ->
      (RIP.setUnionPayload, RIP.getField @"t1_x_x" x0)

instance (ty ~ MyInt) => RIP.HasField "t1_x_x" (RIP.Ptr T1_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"t1_x_x")

instance HasCField.HasCField T1_x "t1_x_x" where

  type CFieldType T1_x "t1_x_x" = MyInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@T1@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 3:1@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
newtype T1 = T1
  { unwrapT1 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize T1

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw T1

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw T1

deriving via Marshal.EquivStorable T1 instance RIP.Storable T1

deriving via RIP.SizedByteArray 4 4 instance Union.IsUnion T1

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 3:31@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
instance (ty ~ T1_x) => RIP.HasField "t1_x" T1 ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 3:31@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
instance (ty ~ T1_x) => RIP.CompatHasField.HasField "t1_x" T1 ty where

  hasField =
    \x0 -> (RIP.setUnionPayload, RIP.getField @"t1_x" x0)

instance (ty ~ T1_x) => RIP.HasField "t1_x" (RIP.Ptr T1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"t1_x")

instance HasCField.HasCField T1 "t1_x" where

  type CFieldType T1 "t1_x" = T1_x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@T2@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 4:1@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
newtype T2 = T2
  { unwrapT2 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 8 8 instance Marshal.StaticSize T2

deriving via RIP.SizedByteArray 8 8 instance Marshal.ReadRaw T2

deriving via RIP.SizedByteArray 8 8 instance Marshal.WriteRaw T2

deriving via Marshal.EquivStorable T2 instance RIP.Storable T2

deriving via RIP.SizedByteArray 8 8 instance Union.IsUnion T2

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 4:31@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
instance (ty ~ RIP.Ptr T2_x) => RIP.HasField "t2_x" T2 ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 4:31@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
instance (ty ~ RIP.Ptr T2_x) => RIP.CompatHasField.HasField "t2_x" T2 ty where

  hasField =
    \x0 -> (RIP.setUnionPayload, RIP.getField @"t2_x" x0)

instance ( ty ~ RIP.Ptr T2_x
         ) => RIP.HasField "t2_x" (RIP.Ptr T2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"t2_x")

instance HasCField.HasCField T2 "t2_x" where

  type CFieldType T2 "t2_x" = RIP.Ptr T2_x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@T2_x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 4:9@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
newtype T2_x = T2_x
  { unwrapT2_x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize T2_x

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw T2_x

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw T2_x

deriving via Marshal.EquivStorable T2_x instance RIP.Storable T2_x

deriving via RIP.SizedByteArray 4 4 instance Union.IsUnion T2_x

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 4:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
instance (ty ~ MyInt) => RIP.HasField "t2_x_x" T2_x ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 4:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
instance (ty ~ MyInt) => RIP.CompatHasField.HasField "t2_x_x" T2_x ty where

  hasField =
    \x0 ->
      (RIP.setUnionPayload, RIP.getField @"t2_x_x" x0)

instance (ty ~ MyInt) => RIP.HasField "t2_x_x" (RIP.Ptr T2_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"t2_x_x")

instance HasCField.HasCField T2_x "t2_x_x" where

  type CFieldType T2_x "t2_x_x" = MyInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@T3@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 5:1@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
newtype T3 = T3
  { unwrapT3 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 8 8 instance Marshal.StaticSize T3

deriving via RIP.SizedByteArray 8 8 instance Marshal.ReadRaw T3

deriving via RIP.SizedByteArray 8 8 instance Marshal.WriteRaw T3

deriving via Marshal.EquivStorable T3 instance RIP.Storable T3

deriving via RIP.SizedByteArray 8 8 instance Union.IsUnion T3

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 5:31@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
instance (ty ~ RIP.Ptr (RIP.Ptr T3_x)) => RIP.HasField "t3_x" T3 ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 5:31@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
instance ( ty ~ RIP.Ptr (RIP.Ptr T3_x)
         ) => RIP.CompatHasField.HasField "t3_x" T3 ty where

  hasField =
    \x0 -> (RIP.setUnionPayload, RIP.getField @"t3_x" x0)

instance ( ty ~ RIP.Ptr (RIP.Ptr T3_x)
         ) => RIP.HasField "t3_x" (RIP.Ptr T3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"t3_x")

instance HasCField.HasCField T3 "t3_x" where

  type CFieldType T3 "t3_x" = RIP.Ptr (RIP.Ptr T3_x)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@T3_x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 5:9@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
newtype T3_x = T3_x
  { unwrapT3_x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize T3_x

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw T3_x

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw T3_x

deriving via Marshal.EquivStorable T3_x instance RIP.Storable T3_x

deriving via RIP.SizedByteArray 4 4 instance Union.IsUnion T3_x

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 5:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
instance (ty ~ MyInt) => RIP.HasField "t3_x_x" T3_x ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 5:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
instance (ty ~ MyInt) => RIP.CompatHasField.HasField "t3_x_x" T3_x ty where

  hasField =
    \x0 ->
      (RIP.setUnionPayload, RIP.getField @"t3_x_x" x0)

instance (ty ~ MyInt) => RIP.HasField "t3_x_x" (RIP.Ptr T3_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"t3_x_x")

instance HasCField.HasCField T3_x "t3_x_x" where

  type CFieldType T3_x "t3_x_x" = MyInt

  offset# = \_ -> \_ -> 0
