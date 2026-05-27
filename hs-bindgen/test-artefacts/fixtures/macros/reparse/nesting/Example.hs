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
    , Example.T2(..)
    , Example.T3(..)
    , Example.T4(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @macro MyInt@

    __defined at:__ @macros\/reparse\/nesting.h 1:9@

    __exported by:__ @macros\/reparse\/nesting.h@
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

{-| __C declaration:__ @struct TS1@

    __defined at:__ @macros\/reparse\/nesting.h 5:16@

    __exported by:__ @macros\/reparse\/nesting.h@
-}
data T2 = T2
  { t2_x :: MyInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting.h 5:28@

         __exported by:__ @macros\/reparse\/nesting.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize T2 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

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

  type CFieldType T2 "t2_x" = MyInt

  offset# = \_ -> \_ -> 0

instance (ty ~ MyInt) => RIP.HasField "t2_x" (RIP.Ptr T2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"t2_x")

{-| __C declaration:__ @struct TS3@

    __defined at:__ @macros\/reparse\/nesting.h 6:16@

    __exported by:__ @macros\/reparse\/nesting.h@
-}
data T3 = T3
  { t3_x :: MyInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting.h 6:28@

         __exported by:__ @macros\/reparse\/nesting.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize T3 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

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

  type CFieldType T3 "t3_x" = MyInt

  offset# = \_ -> \_ -> 0

instance (ty ~ MyInt) => RIP.HasField "t3_x" (RIP.Ptr T3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"t3_x")

{-| __C declaration:__ @struct T4@

    __defined at:__ @macros\/reparse\/nesting.h 7:9@

    __exported by:__ @macros\/reparse\/nesting.h@
-}
data T4 = T4
  { t4_x :: MyInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting.h 7:28@

         __exported by:__ @macros\/reparse\/nesting.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize T4 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T4 where

  readRaw =
    \ptr0 ->
          pure T4
      <*> HasCField.readRaw (RIP.Proxy @"t4_x") ptr0

instance Marshal.WriteRaw T4 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T4 t4_x2 ->
            HasCField.writeRaw (RIP.Proxy @"t4_x") ptr0 t4_x2

deriving via Marshal.EquivStorable T4 instance RIP.Storable T4

instance HasCField.HasCField T4 "t4_x" where

  type CFieldType T4 "t4_x" = MyInt

  offset# = \_ -> \_ -> 0

instance (ty ~ MyInt) => RIP.HasField "t4_x" (RIP.Ptr T4) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"t4_x")
