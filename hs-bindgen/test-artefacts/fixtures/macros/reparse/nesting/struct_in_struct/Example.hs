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
    , Example.T1(..)
    , Example.T2(..)
    , Example.T2_x(..)
    , Example.T3(..)
    , Example.T3_x(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @macro MyInt@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_struct.h 1:9@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_struct.h@
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

{-| __C declaration:__ @struct \@T1_x@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_struct.h 3:10@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_struct.h@
-}
data T1_x = T1_x
  { t1_x_x :: MyInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting\/struct_in_struct.h 3:25@

         __exported by:__ @macros\/reparse\/nesting\/struct_in_struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize T1_x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T1_x where

  readRaw =
    \ptr0 ->
          pure T1_x
      <*> HasCField.readRaw (RIP.Proxy @"t1_x_x") ptr0

instance Marshal.WriteRaw T1_x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T1_x t1_x_x2 ->
            HasCField.writeRaw (RIP.Proxy @"t1_x_x") ptr0 t1_x_x2

deriving via Marshal.EquivStorable T1_x instance RIP.Storable T1_x

instance HasCField.HasCField T1_x "t1_x_x" where

  type CFieldType T1_x "t1_x_x" = MyInt

  offset# = \_ -> \_ -> 0

instance RIP.HasField "t1_x_x" (RIP.Ptr T1_x) (RIP.Ptr MyInt) where

  getField = HasCField.fromPtr (RIP.Proxy @"t1_x_x")

{-| __C declaration:__ @struct \@T1@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_struct.h 3:1@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_struct.h@
-}
data T1 = T1
  { t1_x :: T1_x
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting\/struct_in_struct.h 3:33@

         __exported by:__ @macros\/reparse\/nesting\/struct_in_struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize T1 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T1 where

  readRaw =
    \ptr0 ->
          pure T1
      <*> HasCField.readRaw (RIP.Proxy @"t1_x") ptr0

instance Marshal.WriteRaw T1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T1 t1_x2 ->
            HasCField.writeRaw (RIP.Proxy @"t1_x") ptr0 t1_x2

deriving via Marshal.EquivStorable T1 instance RIP.Storable T1

instance HasCField.HasCField T1 "t1_x" where

  type CFieldType T1 "t1_x" = T1_x

  offset# = \_ -> \_ -> 0

instance RIP.HasField "t1_x" (RIP.Ptr T1) (RIP.Ptr T1_x) where

  getField = HasCField.fromPtr (RIP.Proxy @"t1_x")

{-| __C declaration:__ @struct \@T2@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_struct.h 4:1@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_struct.h@
-}
data T2 = T2
  { t2_x :: RIP.Ptr T2_x
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting\/struct_in_struct.h 4:33@

         __exported by:__ @macros\/reparse\/nesting\/struct_in_struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize T2 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw T2 where

  readRaw =
    \ptr0 ->
          pure T2
      <*> HasCField.readRaw (RIP.Proxy @"t2_x") ptr0

instance Marshal.WriteRaw T2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T2 t2_x2 ->
            HasCField.writeRaw (RIP.Proxy @"t2_x") ptr0 t2_x2

deriving via Marshal.EquivStorable T2 instance RIP.Storable T2

instance HasCField.HasCField T2 "t2_x" where

  type CFieldType T2 "t2_x" = RIP.Ptr T2_x

  offset# = \_ -> \_ -> 0

instance RIP.HasField "t2_x" (RIP.Ptr T2) (RIP.Ptr (RIP.Ptr T2_x)) where

  getField = HasCField.fromPtr (RIP.Proxy @"t2_x")

{-| __C declaration:__ @struct \@T2_x@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_struct.h 4:10@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_struct.h@
-}
data T2_x = T2_x
  { t2_x_x :: MyInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting\/struct_in_struct.h 4:25@

         __exported by:__ @macros\/reparse\/nesting\/struct_in_struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize T2_x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T2_x where

  readRaw =
    \ptr0 ->
          pure T2_x
      <*> HasCField.readRaw (RIP.Proxy @"t2_x_x") ptr0

instance Marshal.WriteRaw T2_x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T2_x t2_x_x2 ->
            HasCField.writeRaw (RIP.Proxy @"t2_x_x") ptr0 t2_x_x2

deriving via Marshal.EquivStorable T2_x instance RIP.Storable T2_x

instance HasCField.HasCField T2_x "t2_x_x" where

  type CFieldType T2_x "t2_x_x" = MyInt

  offset# = \_ -> \_ -> 0

instance RIP.HasField "t2_x_x" (RIP.Ptr T2_x) (RIP.Ptr MyInt) where

  getField = HasCField.fromPtr (RIP.Proxy @"t2_x_x")

{-| __C declaration:__ @struct \@T3@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_struct.h 5:1@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_struct.h@
-}
data T3 = T3
  { t3_x :: RIP.Ptr (RIP.Ptr T3_x)
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting\/struct_in_struct.h 5:33@

         __exported by:__ @macros\/reparse\/nesting\/struct_in_struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize T3 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw T3 where

  readRaw =
    \ptr0 ->
          pure T3
      <*> HasCField.readRaw (RIP.Proxy @"t3_x") ptr0

instance Marshal.WriteRaw T3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T3 t3_x2 ->
            HasCField.writeRaw (RIP.Proxy @"t3_x") ptr0 t3_x2

deriving via Marshal.EquivStorable T3 instance RIP.Storable T3

instance HasCField.HasCField T3 "t3_x" where

  type CFieldType T3 "t3_x" = RIP.Ptr (RIP.Ptr T3_x)

  offset# = \_ -> \_ -> 0

instance RIP.HasField "t3_x" (RIP.Ptr T3) (RIP.Ptr (RIP.Ptr (RIP.Ptr T3_x))) where

  getField = HasCField.fromPtr (RIP.Proxy @"t3_x")

{-| __C declaration:__ @struct \@T3_x@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_struct.h 5:10@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_struct.h@
-}
data T3_x = T3_x
  { t3_x_x :: MyInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting\/struct_in_struct.h 5:25@

         __exported by:__ @macros\/reparse\/nesting\/struct_in_struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize T3_x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T3_x where

  readRaw =
    \ptr0 ->
          pure T3_x
      <*> HasCField.readRaw (RIP.Proxy @"t3_x_x") ptr0

instance Marshal.WriteRaw T3_x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T3_x t3_x_x2 ->
            HasCField.writeRaw (RIP.Proxy @"t3_x_x") ptr0 t3_x_x2

deriving via Marshal.EquivStorable T3_x instance RIP.Storable T3_x

instance HasCField.HasCField T3_x "t3_x_x" where

  type CFieldType T3_x "t3_x_x" = MyInt

  offset# = \_ -> \_ -> 0

instance RIP.HasField "t3_x_x" (RIP.Ptr T3_x) (RIP.Ptr MyInt) where

  getField = HasCField.fromPtr (RIP.Proxy @"t3_x_x")
