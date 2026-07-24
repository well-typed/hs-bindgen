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
    ( Example.A(..)
    , Example.B(..)
    , Example.S_anon'anon'x_anon'x(..)
    , Example.S_anon'anon'x(..)
    , Example.S(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro A@

    __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 3:9@

    __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
-}
newtype A = A
  { unwrapA :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapA" A ty where

  hasField =
    \x0 ->
      (\y1 -> A {unwrapA = y1}, BG.getField @"unwrapA" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapA" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro B@

    __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 4:9@

    __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
-}
newtype B = B
  { unwrapB :: BG.CChar
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

instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "unwrapB" B ty where

  hasField =
    \x0 ->
      (\y1 -> B {unwrapB = y1}, BG.getField @"unwrapB" x0)

instance (ty ~ BG.CChar) => BG.HasField "unwrapB" (BG.Ptr B) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapB")

instance HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@S_anon\'anon\'x_anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 8:5@

    __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
-}
data S_anon'anon'x_anon'x = S_anon'anon'x_anon'x
  { s_anon'anon'x_anon'x_x :: A
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 9:9@

         __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S_anon'anon'x_anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S_anon'anon'x_anon'x where

  readRaw =
    \ptr0 ->
          pure S_anon'anon'x_anon'x
      <*> HasCField.readRaw (BG.Proxy @"s_anon'anon'x_anon'x_x") ptr0

instance Marshal.WriteRaw S_anon'anon'x_anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S_anon'anon'x_anon'x s_anon'anon'x_anon'x_x2 ->
            HasCField.writeRaw (BG.Proxy @"s_anon'anon'x_anon'x_x") ptr0 s_anon'anon'x_anon'x_x2

deriving via Marshal.EquivStorable S_anon'anon'x_anon'x instance BG.Storable S_anon'anon'x_anon'x

deriving via Struct.IsStructViaStorable S_anon'anon'x_anon'x instance Struct.IsStruct S_anon'anon'x_anon'x

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 9:9@

    __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
-}
instance ( ty ~ A
         ) => BG.CompatHasField.HasField "s_anon'anon'x_anon'x_x" S_anon'anon'x_anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S_anon'anon'x_anon'x {s_anon'anon'x_anon'x_x = y1}
      , BG.getField @"s_anon'anon'x_anon'x_x" x0
      )

instance ( ty ~ A
         ) => BG.HasField "s_anon'anon'x_anon'x_x" (BG.Ptr S_anon'anon'x_anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"s_anon'anon'x_anon'x_x")

instance HasCField.HasCField S_anon'anon'x_anon'x "s_anon'anon'x_anon'x_x" where

  type CFieldType S_anon'anon'x_anon'x "s_anon'anon'x_anon'x_x" =
    A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@S_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 7:3@

    __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
-}
data S_anon'anon'x = S_anon'anon'x
  { s_anon'anon'x_anon'x :: S_anon'anon'x_anon'x
    {- ^ __C declaration:__ @anon\'x@

         __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 8:5@

         __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
    -}
  , s_anon'anon'x_y :: B
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 11:7@

         __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S_anon'anon'x where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S_anon'anon'x where

  readRaw =
    \ptr0 ->
          pure S_anon'anon'x
      <*> HasCField.readRaw (BG.Proxy @"s_anon'anon'x_anon'x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s_anon'anon'x_y") ptr0

instance Marshal.WriteRaw S_anon'anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S_anon'anon'x s_anon'anon'x_anon'x2 s_anon'anon'x_y3 ->
               HasCField.writeRaw (BG.Proxy @"s_anon'anon'x_anon'x") ptr0 s_anon'anon'x_anon'x2
            >> HasCField.writeRaw (BG.Proxy @"s_anon'anon'x_y") ptr0 s_anon'anon'x_y3

deriving via Marshal.EquivStorable S_anon'anon'x instance BG.Storable S_anon'anon'x

deriving via Struct.IsStructViaStorable S_anon'anon'x instance Struct.IsStruct S_anon'anon'x

{-| __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 8:5@

    __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
-}
instance ( ty ~ S_anon'anon'x_anon'x
         ) => BG.CompatHasField.HasField "s_anon'anon'x_anon'x" S_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S_anon'anon'x {s_anon'anon'x_anon'x = y1, s_anon'anon'x_y = BG.getField @"s_anon'anon'x_y" x0}
      , BG.getField @"s_anon'anon'x_anon'x" x0
      )

instance ( ty ~ S_anon'anon'x_anon'x
         ) => BG.HasField "s_anon'anon'x_anon'x" (BG.Ptr S_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"s_anon'anon'x_anon'x")

instance HasCField.HasCField S_anon'anon'x "s_anon'anon'x_anon'x" where

  type CFieldType S_anon'anon'x "s_anon'anon'x_anon'x" =
    S_anon'anon'x_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 9:9@

    __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
-}
instance (ty ~ A) => BG.HasField "s_anon'anon'x_x" S_anon'anon'x ty where

  getField =
    \x0 ->
      BG.getField @"s_anon'anon'x_anon'x_x" (BG.getField @"s_anon'anon'x_anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 9:9@

    __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
-}
instance ( ty ~ A
         ) => BG.CompatHasField.HasField "s_anon'anon'x_x" S_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"s_anon'anon'x_anon'x" x0 (\z2 ->
                                                                      BG.CompatHasField.setField @"s_anon'anon'x_anon'x_x" z2 y1)
      , BG.getField @"s_anon'anon'x_x" x0
      )

instance ( ty ~ A
         ) => BG.HasField "s_anon'anon'x_x" (BG.Ptr S_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"s_anon'anon'x_x")

instance HasCField.HasCField S_anon'anon'x "s_anon'anon'x_x" where

  type CFieldType S_anon'anon'x "s_anon'anon'x_x" = A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 11:7@

    __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
-}
instance ( ty ~ B
         ) => BG.CompatHasField.HasField "s_anon'anon'x_y" S_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S_anon'anon'x { s_anon'anon'x_y = y1
                        , s_anon'anon'x_anon'x = BG.getField @"s_anon'anon'x_anon'x" x0
                        }
      , BG.getField @"s_anon'anon'x_y" x0
      )

instance ( ty ~ B
         ) => BG.HasField "s_anon'anon'x_y" (BG.Ptr S_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"s_anon'anon'x_y")

instance HasCField.HasCField S_anon'anon'x "s_anon'anon'x_y" where

  type CFieldType S_anon'anon'x "s_anon'anon'x_y" = B

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct S@

    __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 6:8@

    __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
-}
data S = S
  { s_anon'anon'x :: S_anon'anon'x
    {- ^ __C declaration:__ @anon\'anon\'x@

         __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 7:3@

         __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HasCField.readRaw (BG.Proxy @"s_anon'anon'x") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_anon'anon'x2 ->
            HasCField.writeRaw (BG.Proxy @"s_anon'anon'x") ptr0 s_anon'anon'x2

deriving via Marshal.EquivStorable S instance BG.Storable S

deriving via Struct.IsStructViaStorable S instance Struct.IsStruct S

{-| __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 7:3@

    __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
-}
instance ( ty ~ S_anon'anon'x
         ) => BG.CompatHasField.HasField "s_anon'anon'x" S ty where

  hasField =
    \x0 ->
      (\y1 ->
         S {s_anon'anon'x = y1}, BG.getField @"s_anon'anon'x" x0)

instance ( ty ~ S_anon'anon'x
         ) => BG.HasField "s_anon'anon'x" (BG.Ptr S) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"s_anon'anon'x")

instance HasCField.HasCField S "s_anon'anon'x" where

  type CFieldType S "s_anon'anon'x" = S_anon'anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 9:9@

    __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
-}
instance (ty ~ A) => BG.HasField "s_x" S ty where

  getField =
    \x0 ->
      BG.getField @"s_anon'anon'x_x" (BG.getField @"s_anon'anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 9:9@

    __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
-}
instance (ty ~ A) => BG.CompatHasField.HasField "s_x" S ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"s_anon'anon'x" x0 (\z2 ->
                                                               BG.CompatHasField.setField @"s_anon'anon'x_x" z2 y1)
      , BG.getField @"s_x" x0
      )

instance (ty ~ A) => BG.HasField "s_x" (BG.Ptr S) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s_x")

instance HasCField.HasCField S "s_x" where

  type CFieldType S "s_x" = A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 11:7@

    __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
-}
instance (ty ~ B) => BG.HasField "s_y" S ty where

  getField =
    \x0 ->
      BG.getField @"s_anon'anon'x_y" (BG.getField @"s_anon'anon'x" x0)

{-| __C declaration:__ @y@

    __defined at:__ @types\/anonymous\/edge-cases\/reparse.h 11:7@

    __exported by:__ @types\/anonymous\/edge-cases\/reparse.h@
-}
instance (ty ~ B) => BG.CompatHasField.HasField "s_y" S ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"s_anon'anon'x" x0 (\z2 ->
                                                               BG.CompatHasField.setField @"s_anon'anon'x_y" z2 y1)
      , BG.getField @"s_y" x0
      )

instance (ty ~ B) => BG.HasField "s_y" (BG.Ptr S) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s_y")

instance HasCField.HasCField S "s_y" where

  type CFieldType S "s_y" = B

  offset# = \_ -> \_ -> 4
