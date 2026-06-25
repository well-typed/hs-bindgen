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
    , Example.T1(..)
    , Example.get_t1_x
    , Example.set_t1_x
    , Example.T2_Aux(..)
    , Example.get_t2_Aux_x
    , Example.set_t2_Aux_x
    , Example.T2(..)
    , Example.T3_Aux(..)
    , Example.get_t3_Aux_x
    , Example.set_t3_Aux_x
    , Example.T3(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Internal.Prelude.CompatHasField as RIP.CompatHasField
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @macro MyInt@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 1:9@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
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
         ) => RIP.HasField "unwrapMyInt" (RIP.Ptr MyInt) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMyInt")

instance HasCField.HasCField MyInt "unwrapMyInt" where

  type CFieldType MyInt "unwrapMyInt" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "unwrapMyInt" MyInt ty where

  hasField =
    \x0 ->
      (\y1 ->
         MyInt {unwrapMyInt = y1}, RIP.getField @"unwrapMyInt" x0)

{-| __C declaration:__ @union T1@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 3:9@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
-}
newtype T1 = T1
  { unwrapT1 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize T1

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw T1

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw T1

deriving via Marshal.EquivStorable T1 instance RIP.Storable T1

{-|

    __See:__ 'set_t1_x'

    __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 3:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
-}
get_t1_x ::
     T1
  -> MyInt
get_t1_x = RIP.getUnionPayload

{-|

    __See:__ 'get_t1_x'

-}
set_t1_x ::
     MyInt
  -> T1
set_t1_x = RIP.setUnionPayload

instance HasCField.HasCField T1 "t1_x" where

  type CFieldType T1 "t1_x" = MyInt

  offset# = \_ -> \_ -> 0

instance (ty ~ MyInt) => RIP.HasField "t1_x" (RIP.Ptr T1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"t1_x")

{-| __C declaration:__ @union \@T2_Aux@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 4:9@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
-}
newtype T2_Aux = T2_Aux
  { unwrapT2_Aux :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize T2_Aux

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw T2_Aux

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw T2_Aux

deriving via Marshal.EquivStorable T2_Aux instance RIP.Storable T2_Aux

{-|

    __See:__ 'set_t2_Aux_x'

    __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 4:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
-}
get_t2_Aux_x ::
     T2_Aux
  -> MyInt
get_t2_Aux_x = RIP.getUnionPayload

{-|

    __See:__ 'get_t2_Aux_x'

-}
set_t2_Aux_x ::
     MyInt
  -> T2_Aux
set_t2_Aux_x = RIP.setUnionPayload

instance HasCField.HasCField T2_Aux "t2_Aux_x" where

  type CFieldType T2_Aux "t2_Aux_x" = MyInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ MyInt
         ) => RIP.HasField "t2_Aux_x" (RIP.Ptr T2_Aux) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"t2_Aux_x")

{-| __C declaration:__ @T2@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 4:31@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
-}
newtype T2 = T2
  { unwrapT2 :: RIP.Ptr T2_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ RIP.Ptr T2_Aux
         ) => RIP.HasField "unwrapT2" (RIP.Ptr T2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT2")

instance HasCField.HasCField T2 "unwrapT2" where

  type CFieldType T2 "unwrapT2" = RIP.Ptr T2_Aux

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr T2_Aux
         ) => RIP.CompatHasField.HasField "unwrapT2" T2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         T2 {unwrapT2 = y1}, RIP.getField @"unwrapT2" x0)

{-| __C declaration:__ @union \@T3_Aux@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 5:9@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
-}
newtype T3_Aux = T3_Aux
  { unwrapT3_Aux :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize T3_Aux

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw T3_Aux

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw T3_Aux

deriving via Marshal.EquivStorable T3_Aux instance RIP.Storable T3_Aux

{-|

    __See:__ 'set_t3_Aux_x'

    __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 5:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
-}
get_t3_Aux_x ::
     T3_Aux
  -> MyInt
get_t3_Aux_x = RIP.getUnionPayload

{-|

    __See:__ 'get_t3_Aux_x'

-}
set_t3_Aux_x ::
     MyInt
  -> T3_Aux
set_t3_Aux_x = RIP.setUnionPayload

instance HasCField.HasCField T3_Aux "t3_Aux_x" where

  type CFieldType T3_Aux "t3_Aux_x" = MyInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ MyInt
         ) => RIP.HasField "t3_Aux_x" (RIP.Ptr T3_Aux) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"t3_Aux_x")

{-| __C declaration:__ @T3@

    __defined at:__ @macros\/reparse\/nesting\/union_in_typedef.h 5:31@

    __exported by:__ @macros\/reparse\/nesting\/union_in_typedef.h@
-}
newtype T3 = T3
  { unwrapT3 :: RIP.Ptr (RIP.Ptr T3_Aux)
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ RIP.Ptr (RIP.Ptr T3_Aux)
         ) => RIP.HasField "unwrapT3" (RIP.Ptr T3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT3")

instance HasCField.HasCField T3 "unwrapT3" where

  type CFieldType T3 "unwrapT3" =
    RIP.Ptr (RIP.Ptr T3_Aux)

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr (RIP.Ptr T3_Aux)
         ) => RIP.CompatHasField.HasField "unwrapT3" T3 ty where

  hasField =
    \x0 ->
      (\y1 ->
         T3 {unwrapT3 = y1}, RIP.getField @"unwrapT3" x0)
