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
    ( Example.UU1_fieldY(..)
    , Example.UU1(..)
    , Example.UU2_fieldY(..)
    , Example.UU2(..)
    , Example.VV1_fieldA(..)
    , Example.VV1_fieldB(..)
    , Example.VV1(..)
    , Example.VV2_fieldA(..)
    , Example.VV2_fieldB(..)
    , Example.VV2(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct \@UU1_fieldY@

    __defined at:__ @macros\/wrong_source_location.h 19:1@

    __exported by:__ @macros\/wrong_source_location.h@
-}
data UU1_fieldY = UU1_fieldY
  { uU1_fieldY_fieldX :: RIP.CInt
    {- ^ __C declaration:__ @fieldX@

         __defined at:__ @macros\/wrong_source_location.h 19:1@

         __exported by:__ @macros\/wrong_source_location.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize UU1_fieldY where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw UU1_fieldY where

  readRaw =
    \ptr0 ->
          pure UU1_fieldY
      <*> HasCField.readRaw (RIP.Proxy @"uU1_fieldY_fieldX") ptr0

instance Marshal.WriteRaw UU1_fieldY where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          UU1_fieldY uU1_fieldY_fieldX2 ->
            HasCField.writeRaw (RIP.Proxy @"uU1_fieldY_fieldX") ptr0 uU1_fieldY_fieldX2

deriving via Marshal.EquivStorable UU1_fieldY instance RIP.Storable UU1_fieldY

instance HasCField.HasCField UU1_fieldY "uU1_fieldY_fieldX" where

  type CFieldType UU1_fieldY "uU1_fieldY_fieldX" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "uU1_fieldY_fieldX" (RIP.Ptr UU1_fieldY) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"uU1_fieldY_fieldX")

{-| __C declaration:__ @struct UU1@

    __defined at:__ @macros\/wrong_source_location.h 19:1@

    __exported by:__ @macros\/wrong_source_location.h@
-}
data UU1 = UU1
  { uU1_fieldY :: UU1_fieldY
    {- ^ __C declaration:__ @fieldY@

         __defined at:__ @macros\/wrong_source_location.h 19:1@

         __exported by:__ @macros\/wrong_source_location.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize UU1 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw UU1 where

  readRaw =
    \ptr0 ->
          pure UU1
      <*> HasCField.readRaw (RIP.Proxy @"uU1_fieldY") ptr0

instance Marshal.WriteRaw UU1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          UU1 uU1_fieldY2 ->
            HasCField.writeRaw (RIP.Proxy @"uU1_fieldY") ptr0 uU1_fieldY2

deriving via Marshal.EquivStorable UU1 instance RIP.Storable UU1

instance HasCField.HasCField UU1 "uU1_fieldY" where

  type CFieldType UU1 "uU1_fieldY" = UU1_fieldY

  offset# = \_ -> \_ -> 0

instance ( ty ~ UU1_fieldY
         ) => RIP.HasField "uU1_fieldY" (RIP.Ptr UU1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"uU1_fieldY")

{-| __C declaration:__ @struct \@UU2_fieldY@

    __defined at:__ @macros\/wrong_source_location.h 21:1@

    __exported by:__ @macros\/wrong_source_location.h@
-}
data UU2_fieldY = UU2_fieldY
  { uU2_fieldY_fieldX :: RIP.CInt
    {- ^ __C declaration:__ @fieldX@

         __defined at:__ @macros\/wrong_source_location.h 21:1@

         __exported by:__ @macros\/wrong_source_location.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize UU2_fieldY where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw UU2_fieldY where

  readRaw =
    \ptr0 ->
          pure UU2_fieldY
      <*> HasCField.readRaw (RIP.Proxy @"uU2_fieldY_fieldX") ptr0

instance Marshal.WriteRaw UU2_fieldY where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          UU2_fieldY uU2_fieldY_fieldX2 ->
            HasCField.writeRaw (RIP.Proxy @"uU2_fieldY_fieldX") ptr0 uU2_fieldY_fieldX2

deriving via Marshal.EquivStorable UU2_fieldY instance RIP.Storable UU2_fieldY

instance HasCField.HasCField UU2_fieldY "uU2_fieldY_fieldX" where

  type CFieldType UU2_fieldY "uU2_fieldY_fieldX" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "uU2_fieldY_fieldX" (RIP.Ptr UU2_fieldY) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"uU2_fieldY_fieldX")

{-| __C declaration:__ @struct UU2@

    __defined at:__ @macros\/wrong_source_location.h 21:1@

    __exported by:__ @macros\/wrong_source_location.h@
-}
data UU2 = UU2
  { uU2_fieldY :: UU2_fieldY
    {- ^ __C declaration:__ @fieldY@

         __defined at:__ @macros\/wrong_source_location.h 21:1@

         __exported by:__ @macros\/wrong_source_location.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize UU2 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw UU2 where

  readRaw =
    \ptr0 ->
          pure UU2
      <*> HasCField.readRaw (RIP.Proxy @"uU2_fieldY") ptr0

instance Marshal.WriteRaw UU2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          UU2 uU2_fieldY2 ->
            HasCField.writeRaw (RIP.Proxy @"uU2_fieldY") ptr0 uU2_fieldY2

deriving via Marshal.EquivStorable UU2 instance RIP.Storable UU2

instance HasCField.HasCField UU2 "uU2_fieldY" where

  type CFieldType UU2 "uU2_fieldY" = UU2_fieldY

  offset# = \_ -> \_ -> 0

instance ( ty ~ UU2_fieldY
         ) => RIP.HasField "uU2_fieldY" (RIP.Ptr UU2) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"uU2_fieldY")

{-| __C declaration:__ @struct \@VV1_fieldA@

    __defined at:__ @macros\/wrong_source_location.h 29:1@

    __exported by:__ @macros\/wrong_source_location.h@
-}
data VV1_fieldA = VV1_fieldA
  { vV1_fieldA_a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macros\/wrong_source_location.h 29:1@

         __exported by:__ @macros\/wrong_source_location.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize VV1_fieldA where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw VV1_fieldA where

  readRaw =
    \ptr0 ->
          pure VV1_fieldA
      <*> HasCField.readRaw (RIP.Proxy @"vV1_fieldA_a") ptr0

instance Marshal.WriteRaw VV1_fieldA where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          VV1_fieldA vV1_fieldA_a2 ->
            HasCField.writeRaw (RIP.Proxy @"vV1_fieldA_a") ptr0 vV1_fieldA_a2

deriving via Marshal.EquivStorable VV1_fieldA instance RIP.Storable VV1_fieldA

instance HasCField.HasCField VV1_fieldA "vV1_fieldA_a" where

  type CFieldType VV1_fieldA "vV1_fieldA_a" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "vV1_fieldA_a" (RIP.Ptr VV1_fieldA) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"vV1_fieldA_a")

{-| __C declaration:__ @struct \@VV1_fieldB@

    __defined at:__ @macros\/wrong_source_location.h 29:1@

    __exported by:__ @macros\/wrong_source_location.h@
-}
data VV1_fieldB = VV1_fieldB
  { vV1_fieldB_b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @macros\/wrong_source_location.h 29:1@

         __exported by:__ @macros\/wrong_source_location.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize VV1_fieldB where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw VV1_fieldB where

  readRaw =
    \ptr0 ->
          pure VV1_fieldB
      <*> HasCField.readRaw (RIP.Proxy @"vV1_fieldB_b") ptr0

instance Marshal.WriteRaw VV1_fieldB where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          VV1_fieldB vV1_fieldB_b2 ->
            HasCField.writeRaw (RIP.Proxy @"vV1_fieldB_b") ptr0 vV1_fieldB_b2

deriving via Marshal.EquivStorable VV1_fieldB instance RIP.Storable VV1_fieldB

instance HasCField.HasCField VV1_fieldB "vV1_fieldB_b" where

  type CFieldType VV1_fieldB "vV1_fieldB_b" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "vV1_fieldB_b" (RIP.Ptr VV1_fieldB) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"vV1_fieldB_b")

{-| __C declaration:__ @struct VV1@

    __defined at:__ @macros\/wrong_source_location.h 29:1@

    __exported by:__ @macros\/wrong_source_location.h@
-}
data VV1 = VV1
  { vV1_fieldA :: VV1_fieldA
    {- ^ __C declaration:__ @fieldA@

         __defined at:__ @macros\/wrong_source_location.h 29:1@

         __exported by:__ @macros\/wrong_source_location.h@
    -}
  , vV1_fieldB :: VV1_fieldB
    {- ^ __C declaration:__ @fieldB@

         __defined at:__ @macros\/wrong_source_location.h 29:1@

         __exported by:__ @macros\/wrong_source_location.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize VV1 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw VV1 where

  readRaw =
    \ptr0 ->
          pure VV1
      <*> HasCField.readRaw (RIP.Proxy @"vV1_fieldA") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"vV1_fieldB") ptr0

instance Marshal.WriteRaw VV1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          VV1 vV1_fieldA2 vV1_fieldB3 ->
               HasCField.writeRaw (RIP.Proxy @"vV1_fieldA") ptr0 vV1_fieldA2
            >> HasCField.writeRaw (RIP.Proxy @"vV1_fieldB") ptr0 vV1_fieldB3

deriving via Marshal.EquivStorable VV1 instance RIP.Storable VV1

instance HasCField.HasCField VV1 "vV1_fieldA" where

  type CFieldType VV1 "vV1_fieldA" = VV1_fieldA

  offset# = \_ -> \_ -> 0

instance ( ty ~ VV1_fieldA
         ) => RIP.HasField "vV1_fieldA" (RIP.Ptr VV1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"vV1_fieldA")

instance HasCField.HasCField VV1 "vV1_fieldB" where

  type CFieldType VV1 "vV1_fieldB" = VV1_fieldB

  offset# = \_ -> \_ -> 4

instance ( ty ~ VV1_fieldB
         ) => RIP.HasField "vV1_fieldB" (RIP.Ptr VV1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"vV1_fieldB")

{-| __C declaration:__ @struct \@VV2_fieldA@

    __defined at:__ @macros\/wrong_source_location.h 31:1@

    __exported by:__ @macros\/wrong_source_location.h@
-}
data VV2_fieldA = VV2_fieldA
  { vV2_fieldA_a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macros\/wrong_source_location.h 31:1@

         __exported by:__ @macros\/wrong_source_location.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize VV2_fieldA where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw VV2_fieldA where

  readRaw =
    \ptr0 ->
          pure VV2_fieldA
      <*> HasCField.readRaw (RIP.Proxy @"vV2_fieldA_a") ptr0

instance Marshal.WriteRaw VV2_fieldA where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          VV2_fieldA vV2_fieldA_a2 ->
            HasCField.writeRaw (RIP.Proxy @"vV2_fieldA_a") ptr0 vV2_fieldA_a2

deriving via Marshal.EquivStorable VV2_fieldA instance RIP.Storable VV2_fieldA

instance HasCField.HasCField VV2_fieldA "vV2_fieldA_a" where

  type CFieldType VV2_fieldA "vV2_fieldA_a" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "vV2_fieldA_a" (RIP.Ptr VV2_fieldA) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"vV2_fieldA_a")

{-| __C declaration:__ @struct \@VV2_fieldB@

    __defined at:__ @macros\/wrong_source_location.h 31:1@

    __exported by:__ @macros\/wrong_source_location.h@
-}
data VV2_fieldB = VV2_fieldB
  { vV2_fieldB_b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @macros\/wrong_source_location.h 31:1@

         __exported by:__ @macros\/wrong_source_location.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize VV2_fieldB where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw VV2_fieldB where

  readRaw =
    \ptr0 ->
          pure VV2_fieldB
      <*> HasCField.readRaw (RIP.Proxy @"vV2_fieldB_b") ptr0

instance Marshal.WriteRaw VV2_fieldB where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          VV2_fieldB vV2_fieldB_b2 ->
            HasCField.writeRaw (RIP.Proxy @"vV2_fieldB_b") ptr0 vV2_fieldB_b2

deriving via Marshal.EquivStorable VV2_fieldB instance RIP.Storable VV2_fieldB

instance HasCField.HasCField VV2_fieldB "vV2_fieldB_b" where

  type CFieldType VV2_fieldB "vV2_fieldB_b" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "vV2_fieldB_b" (RIP.Ptr VV2_fieldB) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"vV2_fieldB_b")

{-| __C declaration:__ @struct VV2@

    __defined at:__ @macros\/wrong_source_location.h 31:1@

    __exported by:__ @macros\/wrong_source_location.h@
-}
data VV2 = VV2
  { vV2_fieldA :: VV2_fieldA
    {- ^ __C declaration:__ @fieldA@

         __defined at:__ @macros\/wrong_source_location.h 31:1@

         __exported by:__ @macros\/wrong_source_location.h@
    -}
  , vV2_fieldB :: VV2_fieldB
    {- ^ __C declaration:__ @fieldB@

         __defined at:__ @macros\/wrong_source_location.h 31:1@

         __exported by:__ @macros\/wrong_source_location.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize VV2 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw VV2 where

  readRaw =
    \ptr0 ->
          pure VV2
      <*> HasCField.readRaw (RIP.Proxy @"vV2_fieldA") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"vV2_fieldB") ptr0

instance Marshal.WriteRaw VV2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          VV2 vV2_fieldA2 vV2_fieldB3 ->
               HasCField.writeRaw (RIP.Proxy @"vV2_fieldA") ptr0 vV2_fieldA2
            >> HasCField.writeRaw (RIP.Proxy @"vV2_fieldB") ptr0 vV2_fieldB3

deriving via Marshal.EquivStorable VV2 instance RIP.Storable VV2

instance HasCField.HasCField VV2 "vV2_fieldA" where

  type CFieldType VV2 "vV2_fieldA" = VV2_fieldA

  offset# = \_ -> \_ -> 0

instance ( ty ~ VV2_fieldA
         ) => RIP.HasField "vV2_fieldA" (RIP.Ptr VV2) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"vV2_fieldA")

instance HasCField.HasCField VV2 "vV2_fieldB" where

  type CFieldType VV2 "vV2_fieldB" = VV2_fieldB

  offset# = \_ -> \_ -> 4

instance ( ty ~ VV2_fieldB
         ) => RIP.HasField "vV2_fieldB" (RIP.Ptr VV2) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"vV2_fieldB")
