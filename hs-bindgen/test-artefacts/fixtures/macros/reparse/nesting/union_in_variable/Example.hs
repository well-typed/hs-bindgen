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
    , Example.G1(..)
    , Example.G2(..)
    , Example.G3(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Internal.Prelude.CompatHasField as RIP.CompatHasField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @macro MyInt@

    __defined at:__ @macros\/reparse\/nesting\/union_in_variable.h 1:9@

    __exported by:__ @macros\/reparse\/nesting\/union_in_variable.h@
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

{-| __C declaration:__ @union \@G1@

    __defined at:__ @macros\/reparse\/nesting\/union_in_variable.h 3:1@

    __exported by:__ @macros\/reparse\/nesting\/union_in_variable.h@
-}
newtype G1 = G1
  { unwrapG1 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize G1

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw G1

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw G1

deriving via Marshal.EquivStorable G1 instance RIP.Storable G1

deriving via RIP.SizedByteArray 4 4 instance Union.IsUnion G1

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_variable.h 3:15@

    __exported by:__ @macros\/reparse\/nesting\/union_in_variable.h@
-}
instance (ty ~ MyInt) => RIP.HasField "g1_x" G1 ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_variable.h 3:15@

    __exported by:__ @macros\/reparse\/nesting\/union_in_variable.h@
-}
instance (ty ~ MyInt) => RIP.CompatHasField.HasField "g1_x" G1 ty where

  hasField =
    \x0 -> (RIP.setUnionPayload, RIP.getField @"g1_x" x0)

instance (ty ~ MyInt) => RIP.HasField "g1_x" (RIP.Ptr G1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"g1_x")

instance HasCField.HasCField G1 "g1_x" where

  type CFieldType G1 "g1_x" = MyInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@G2@

    __defined at:__ @macros\/reparse\/nesting\/union_in_variable.h 4:1@

    __exported by:__ @macros\/reparse\/nesting\/union_in_variable.h@
-}
newtype G2 = G2
  { unwrapG2 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize G2

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw G2

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw G2

deriving via Marshal.EquivStorable G2 instance RIP.Storable G2

deriving via RIP.SizedByteArray 4 4 instance Union.IsUnion G2

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_variable.h 4:15@

    __exported by:__ @macros\/reparse\/nesting\/union_in_variable.h@
-}
instance (ty ~ MyInt) => RIP.HasField "g2_x" G2 ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_variable.h 4:15@

    __exported by:__ @macros\/reparse\/nesting\/union_in_variable.h@
-}
instance (ty ~ MyInt) => RIP.CompatHasField.HasField "g2_x" G2 ty where

  hasField =
    \x0 -> (RIP.setUnionPayload, RIP.getField @"g2_x" x0)

instance (ty ~ MyInt) => RIP.HasField "g2_x" (RIP.Ptr G2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"g2_x")

instance HasCField.HasCField G2 "g2_x" where

  type CFieldType G2 "g2_x" = MyInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@G3@

    __defined at:__ @macros\/reparse\/nesting\/union_in_variable.h 5:1@

    __exported by:__ @macros\/reparse\/nesting\/union_in_variable.h@
-}
newtype G3 = G3
  { unwrapG3 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize G3

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw G3

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw G3

deriving via Marshal.EquivStorable G3 instance RIP.Storable G3

deriving via RIP.SizedByteArray 4 4 instance Union.IsUnion G3

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_variable.h 5:15@

    __exported by:__ @macros\/reparse\/nesting\/union_in_variable.h@
-}
instance (ty ~ MyInt) => RIP.HasField "g3_x" G3 ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_variable.h 5:15@

    __exported by:__ @macros\/reparse\/nesting\/union_in_variable.h@
-}
instance (ty ~ MyInt) => RIP.CompatHasField.HasField "g3_x" G3 ty where

  hasField =
    \x0 -> (RIP.setUnionPayload, RIP.getField @"g3_x" x0)

instance (ty ~ MyInt) => RIP.HasField "g3_x" (RIP.Ptr G3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"g3_x")

instance HasCField.HasCField G3 "g3_x" where

  type CFieldType G3 "g3_x" = MyInt

  offset# = \_ -> \_ -> 0
