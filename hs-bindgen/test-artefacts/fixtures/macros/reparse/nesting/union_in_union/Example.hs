{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.MyInt(..)
    , Example.T1_x(..)
    , Example.get_t1_x_x
    , Example.set_t1_x_x
    , Example.T1(..)
    , Example.get_t1_x
    , Example.set_t1_x
    , Example.T2(..)
    , Example.get_t2_x
    , Example.set_t2_x
    , Example.T2_x(..)
    , Example.get_t2_x_x
    , Example.set_t2_x_x
    , Example.T3(..)
    , Example.get_t3_x
    , Example.set_t3_x
    , Example.T3_x(..)
    , Example.get_t3_x_x
    , Example.set_t3_x_x
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

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

instance RIP.HasField "unwrapMyInt" (RIP.Ptr MyInt) (RIP.Ptr RIP.CInt) where

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

{-|

    __See:__ 'set_t1_x_x'

    __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 3:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
get_t1_x_x ::
     T1_x
  -> MyInt
get_t1_x_x = RIP.getUnionPayload

{-|

    __See:__ 'get_t1_x_x'

-}
set_t1_x_x ::
     MyInt
  -> T1_x
set_t1_x_x = RIP.setUnionPayload

instance HasCField.HasCField T1_x "t1_x_x" where

  type CFieldType T1_x "t1_x_x" = MyInt

  offset# = \_ -> \_ -> 0

instance RIP.HasField "t1_x_x" (RIP.Ptr T1_x) (RIP.Ptr MyInt) where

  getField = HasCField.fromPtr (RIP.Proxy @"t1_x_x")

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

{-|

    __See:__ 'set_t1_x'

    __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 3:31@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
get_t1_x ::
     T1
  -> T1_x
get_t1_x = RIP.getUnionPayload

{-|

    __See:__ 'get_t1_x'

-}
set_t1_x ::
     T1_x
  -> T1
set_t1_x = RIP.setUnionPayload

instance HasCField.HasCField T1 "t1_x" where

  type CFieldType T1 "t1_x" = T1_x

  offset# = \_ -> \_ -> 0

instance RIP.HasField "t1_x" (RIP.Ptr T1) (RIP.Ptr T1_x) where

  getField = HasCField.fromPtr (RIP.Proxy @"t1_x")

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

{-|

    __See:__ 'set_t2_x'

    __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 4:31@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
get_t2_x ::
     T2
  -> RIP.Ptr T2_x
get_t2_x = RIP.getUnionPayload

{-|

    __See:__ 'get_t2_x'

-}
set_t2_x ::
     RIP.Ptr T2_x
  -> T2
set_t2_x = RIP.setUnionPayload

instance HasCField.HasCField T2 "t2_x" where

  type CFieldType T2 "t2_x" = RIP.Ptr T2_x

  offset# = \_ -> \_ -> 0

instance RIP.HasField "t2_x" (RIP.Ptr T2) (RIP.Ptr (RIP.Ptr T2_x)) where

  getField = HasCField.fromPtr (RIP.Proxy @"t2_x")

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

{-|

    __See:__ 'set_t2_x_x'

    __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 4:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
get_t2_x_x ::
     T2_x
  -> MyInt
get_t2_x_x = RIP.getUnionPayload

{-|

    __See:__ 'get_t2_x_x'

-}
set_t2_x_x ::
     MyInt
  -> T2_x
set_t2_x_x = RIP.setUnionPayload

instance HasCField.HasCField T2_x "t2_x_x" where

  type CFieldType T2_x "t2_x_x" = MyInt

  offset# = \_ -> \_ -> 0

instance RIP.HasField "t2_x_x" (RIP.Ptr T2_x) (RIP.Ptr MyInt) where

  getField = HasCField.fromPtr (RIP.Proxy @"t2_x_x")

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

{-|

    __See:__ 'set_t3_x'

    __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 5:31@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
get_t3_x ::
     T3
  -> RIP.Ptr (RIP.Ptr T3_x)
get_t3_x = RIP.getUnionPayload

{-|

    __See:__ 'get_t3_x'

-}
set_t3_x ::
     RIP.Ptr (RIP.Ptr T3_x)
  -> T3
set_t3_x = RIP.setUnionPayload

instance HasCField.HasCField T3 "t3_x" where

  type CFieldType T3 "t3_x" = RIP.Ptr (RIP.Ptr T3_x)

  offset# = \_ -> \_ -> 0

instance RIP.HasField "t3_x" (RIP.Ptr T3) (RIP.Ptr (RIP.Ptr (RIP.Ptr T3_x))) where

  getField = HasCField.fromPtr (RIP.Proxy @"t3_x")

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

{-|

    __See:__ 'set_t3_x_x'

    __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 5:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
get_t3_x_x ::
     T3_x
  -> MyInt
get_t3_x_x = RIP.getUnionPayload

{-|

    __See:__ 'get_t3_x_x'

-}
set_t3_x_x ::
     MyInt
  -> T3_x
set_t3_x_x = RIP.setUnionPayload

instance HasCField.HasCField T3_x "t3_x_x" where

  type CFieldType T3_x "t3_x_x" = MyInt

  offset# = \_ -> \_ -> 0

instance RIP.HasField "t3_x_x" (RIP.Ptr T3_x) (RIP.Ptr MyInt) where

  getField = HasCField.fromPtr (RIP.Proxy @"t3_x_x")
