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
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @macro MyInt@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 1:9@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
-}
newtype MyInt = MyInt
  { unwrapMyInt :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "unwrapMyInt" MyInt ty where

  hasField =
    \x0 ->
      (\y1 ->
         MyInt {unwrapMyInt = y1}, BG.getField @"unwrapMyInt" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapMyInt" (BG.Ptr MyInt) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMyInt")

instance HasCField.HasCField MyInt "unwrapMyInt" where

  type CFieldType MyInt "unwrapMyInt" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@T1_x@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 3:9@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
-}
data T1_x = T1_x
  { t1_x_x :: MyInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 3:24@

         __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize T1_x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T1_x where

  readRaw =
    \ptr0 ->
          pure T1_x
      <*> HasCField.readRaw (BG.Proxy @"t1_x_x") ptr0

instance Marshal.WriteRaw T1_x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T1_x t1_x_x2 ->
            HasCField.writeRaw (BG.Proxy @"t1_x_x") ptr0 t1_x_x2

deriving via Marshal.EquivStorable T1_x instance BG.Storable T1_x

instance (ty ~ MyInt) => BG.CompatHasField.HasField "t1_x_x" T1_x ty where

  hasField =
    \x0 ->
      (\y1 -> T1_x {t1_x_x = y1}, BG.getField @"t1_x_x" x0)

instance (ty ~ MyInt) => BG.HasField "t1_x_x" (BG.Ptr T1_x) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t1_x_x")

instance HasCField.HasCField T1_x "t1_x_x" where

  type CFieldType T1_x "t1_x_x" = MyInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@T1@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 3:1@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
-}
newtype T1 = T1
  { unwrapT1 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize T1

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw T1

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw T1

deriving via Marshal.EquivStorable T1 instance BG.Storable T1

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion T1

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 3:32@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
-}
instance (ty ~ T1_x) => BG.HasField "t1_x" T1 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 3:32@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
-}
instance (ty ~ T1_x) => BG.CompatHasField.HasField "t1_x" T1 ty where

  hasField =
    \x0 -> (BG.setUnionPayload, BG.getField @"t1_x" x0)

instance (ty ~ T1_x) => BG.HasField "t1_x" (BG.Ptr T1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t1_x")

instance HasCField.HasCField T1 "t1_x" where

  type CFieldType T1 "t1_x" = T1_x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@T2@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 4:1@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
-}
newtype T2 = T2
  { unwrapT2 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 8 8 instance Marshal.StaticSize T2

deriving via BG.SizedByteArray 8 8 instance Marshal.ReadRaw T2

deriving via BG.SizedByteArray 8 8 instance Marshal.WriteRaw T2

deriving via Marshal.EquivStorable T2 instance BG.Storable T2

deriving via BG.SizedByteArray 8 8 instance Union.IsUnion T2

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 4:32@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
-}
instance (ty ~ BG.Ptr T2_x) => BG.HasField "t2_x" T2 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 4:32@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
-}
instance (ty ~ BG.Ptr T2_x) => BG.CompatHasField.HasField "t2_x" T2 ty where

  hasField =
    \x0 -> (BG.setUnionPayload, BG.getField @"t2_x" x0)

instance (ty ~ BG.Ptr T2_x) => BG.HasField "t2_x" (BG.Ptr T2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t2_x")

instance HasCField.HasCField T2 "t2_x" where

  type CFieldType T2 "t2_x" = BG.Ptr T2_x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@T2_x@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 4:9@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
-}
data T2_x = T2_x
  { t2_x_x :: MyInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 4:24@

         __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize T2_x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T2_x where

  readRaw =
    \ptr0 ->
          pure T2_x
      <*> HasCField.readRaw (BG.Proxy @"t2_x_x") ptr0

instance Marshal.WriteRaw T2_x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T2_x t2_x_x2 ->
            HasCField.writeRaw (BG.Proxy @"t2_x_x") ptr0 t2_x_x2

deriving via Marshal.EquivStorable T2_x instance BG.Storable T2_x

instance (ty ~ MyInt) => BG.CompatHasField.HasField "t2_x_x" T2_x ty where

  hasField =
    \x0 ->
      (\y1 -> T2_x {t2_x_x = y1}, BG.getField @"t2_x_x" x0)

instance (ty ~ MyInt) => BG.HasField "t2_x_x" (BG.Ptr T2_x) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t2_x_x")

instance HasCField.HasCField T2_x "t2_x_x" where

  type CFieldType T2_x "t2_x_x" = MyInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@T3@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 5:1@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
-}
newtype T3 = T3
  { unwrapT3 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 8 8 instance Marshal.StaticSize T3

deriving via BG.SizedByteArray 8 8 instance Marshal.ReadRaw T3

deriving via BG.SizedByteArray 8 8 instance Marshal.WriteRaw T3

deriving via Marshal.EquivStorable T3 instance BG.Storable T3

deriving via BG.SizedByteArray 8 8 instance Union.IsUnion T3

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 5:32@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
-}
instance (ty ~ BG.Ptr (BG.Ptr T3_x)) => BG.HasField "t3_x" T3 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 5:32@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
-}
instance ( ty ~ BG.Ptr (BG.Ptr T3_x)
         ) => BG.CompatHasField.HasField "t3_x" T3 ty where

  hasField =
    \x0 -> (BG.setUnionPayload, BG.getField @"t3_x" x0)

instance ( ty ~ BG.Ptr (BG.Ptr T3_x)
         ) => BG.HasField "t3_x" (BG.Ptr T3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t3_x")

instance HasCField.HasCField T3 "t3_x" where

  type CFieldType T3 "t3_x" = BG.Ptr (BG.Ptr T3_x)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@T3_x@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 5:9@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
-}
data T3_x = T3_x
  { t3_x_x :: MyInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 5:24@

         __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize T3_x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T3_x where

  readRaw =
    \ptr0 ->
          pure T3_x
      <*> HasCField.readRaw (BG.Proxy @"t3_x_x") ptr0

instance Marshal.WriteRaw T3_x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T3_x t3_x_x2 ->
            HasCField.writeRaw (BG.Proxy @"t3_x_x") ptr0 t3_x_x2

deriving via Marshal.EquivStorable T3_x instance BG.Storable T3_x

instance (ty ~ MyInt) => BG.CompatHasField.HasField "t3_x_x" T3_x ty where

  hasField =
    \x0 ->
      (\y1 -> T3_x {t3_x_x = y1}, BG.getField @"t3_x_x" x0)

instance (ty ~ MyInt) => BG.HasField "t3_x_x" (BG.Ptr T3_x) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t3_x_x")

instance HasCField.HasCField T3_x "t3_x_x" where

  type CFieldType T3_x "t3_x_x" = MyInt

  offset# = \_ -> \_ -> 0
