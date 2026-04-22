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
    ( Example.Outer1_fieldX(..)
    , Example.Outer1(..)
    , Example.get_outer1_fieldA
    , Example.set_outer1_fieldA
    , Example.get_outer1_fieldX
    , Example.set_outer1_fieldX
    , Example.get_outer1_fieldC
    , Example.set_outer1_fieldC
    , Example.Outer2_fieldB(..)
    , Example.Outer2(..)
    , Example.get_outer2_fieldA
    , Example.set_outer2_fieldA
    , Example.get_outer2_fieldB
    , Example.set_outer2_fieldB
    , Example.get_outer2_fieldC
    , Example.set_outer2_fieldC
    , Example.Inner3(..)
    , Example.Outer3(..)
    , Example.get_outer3_fieldA
    , Example.set_outer3_fieldA
    , Example.get_outer3_fieldB
    , Example.set_outer3_fieldB
    , Example.get_outer3_fieldC
    , Example.set_outer3_fieldC
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct \@outer1_fieldX@

    __defined at:__ @types\/anonymous\/struct_in_union.h 8:3@

    __exported by:__ @types\/anonymous\/struct_in_union.h@
-}
data Outer1_fieldX = Outer1_fieldX
  { outer1_fieldX_fieldX :: RIP.CInt
    {- ^ __C declaration:__ @fieldX@

         __defined at:__ @types\/anonymous\/struct_in_union.h 9:9@

         __exported by:__ @types\/anonymous\/struct_in_union.h@
    -}
  , outer1_fieldX_fieldY :: RIP.CInt
    {- ^ __C declaration:__ @fieldY@

         __defined at:__ @types\/anonymous\/struct_in_union.h 10:9@

         __exported by:__ @types\/anonymous\/struct_in_union.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Outer1_fieldX where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Outer1_fieldX where

  readRaw =
    \ptr0 ->
          pure Outer1_fieldX
      <*> HasCField.readRaw (RIP.Proxy @"outer1_fieldX_fieldX") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"outer1_fieldX_fieldY") ptr0

instance Marshal.WriteRaw Outer1_fieldX where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Outer1_fieldX outer1_fieldX_fieldX2 outer1_fieldX_fieldY3 ->
               HasCField.writeRaw (RIP.Proxy @"outer1_fieldX_fieldX") ptr0 outer1_fieldX_fieldX2
            >> HasCField.writeRaw (RIP.Proxy @"outer1_fieldX_fieldY") ptr0 outer1_fieldX_fieldY3

deriving via Marshal.EquivStorable Outer1_fieldX instance RIP.Storable Outer1_fieldX

instance HasCField.HasCField Outer1_fieldX "outer1_fieldX_fieldX" where

  type CFieldType Outer1_fieldX "outer1_fieldX_fieldX" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "outer1_fieldX_fieldX" (RIP.Ptr Outer1_fieldX) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer1_fieldX_fieldX")

instance HasCField.HasCField Outer1_fieldX "outer1_fieldX_fieldY" where

  type CFieldType Outer1_fieldX "outer1_fieldX_fieldY" =
    RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "outer1_fieldX_fieldY" (RIP.Ptr Outer1_fieldX) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer1_fieldX_fieldY")

{-| __C declaration:__ @union outer1@

    __defined at:__ @types\/anonymous\/struct_in_union.h 6:7@

    __exported by:__ @types\/anonymous\/struct_in_union.h@
-}
newtype Outer1 = Outer1
  { unwrapOuter1 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 8 4 instance Marshal.StaticSize Outer1

deriving via RIP.SizedByteArray 8 4 instance Marshal.ReadRaw Outer1

deriving via RIP.SizedByteArray 8 4 instance Marshal.WriteRaw Outer1

deriving via Marshal.EquivStorable Outer1 instance RIP.Storable Outer1

{-|

    __See:__ 'set_outer1_fieldA'

    __C declaration:__ @fieldA@

    __defined at:__ @types\/anonymous\/struct_in_union.h 7:8@

    __exported by:__ @types\/anonymous\/struct_in_union.h@
-}
get_outer1_fieldA ::
     Outer1
  -> RIP.CChar
get_outer1_fieldA = RIP.getUnionPayload

{-|

    __See:__ 'get_outer1_fieldA'

-}
set_outer1_fieldA ::
     RIP.CChar
  -> Outer1
set_outer1_fieldA = RIP.setUnionPayload

{-|

    __See:__ 'set_outer1_fieldX'

    __C declaration:__ @fieldX@

    __defined at:__ @types\/anonymous\/struct_in_union.h 8:3@

    __exported by:__ @types\/anonymous\/struct_in_union.h@
-}
get_outer1_fieldX ::
     Outer1
  -> Outer1_fieldX
get_outer1_fieldX = RIP.getUnionPayload

{-|

    __See:__ 'get_outer1_fieldX'

-}
set_outer1_fieldX ::
     Outer1_fieldX
  -> Outer1
set_outer1_fieldX = RIP.setUnionPayload

{-|

    __See:__ 'set_outer1_fieldC'

    __C declaration:__ @fieldC@

    __defined at:__ @types\/anonymous\/struct_in_union.h 12:7@

    __exported by:__ @types\/anonymous\/struct_in_union.h@
-}
get_outer1_fieldC ::
     Outer1
  -> RIP.CInt
get_outer1_fieldC = RIP.getUnionPayload

{-|

    __See:__ 'get_outer1_fieldC'

-}
set_outer1_fieldC ::
     RIP.CInt
  -> Outer1
set_outer1_fieldC = RIP.setUnionPayload

instance HasCField.HasCField Outer1 "outer1_fieldA" where

  type CFieldType Outer1 "outer1_fieldA" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CChar
         ) => RIP.HasField "outer1_fieldA" (RIP.Ptr Outer1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer1_fieldA")

instance HasCField.HasCField Outer1 "outer1_fieldX" where

  type CFieldType Outer1 "outer1_fieldX" =
    Outer1_fieldX

  offset# = \_ -> \_ -> 0

instance ( ty ~ Outer1_fieldX
         ) => RIP.HasField "outer1_fieldX" (RIP.Ptr Outer1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer1_fieldX")

instance HasCField.HasCField Outer1 "outer1_fieldC" where

  type CFieldType Outer1 "outer1_fieldC" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "outer1_fieldC" (RIP.Ptr Outer1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer1_fieldC")

{-| __C declaration:__ @struct \@outer2_fieldB@

    __defined at:__ @types\/anonymous\/struct_in_union.h 17:3@

    __exported by:__ @types\/anonymous\/struct_in_union.h@
-}
data Outer2_fieldB = Outer2_fieldB
  { outer2_fieldB_fieldX :: RIP.CInt
    {- ^ __C declaration:__ @fieldX@

         __defined at:__ @types\/anonymous\/struct_in_union.h 18:9@

         __exported by:__ @types\/anonymous\/struct_in_union.h@
    -}
  , outer2_fieldB_fieldY :: RIP.CInt
    {- ^ __C declaration:__ @fieldY@

         __defined at:__ @types\/anonymous\/struct_in_union.h 19:9@

         __exported by:__ @types\/anonymous\/struct_in_union.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Outer2_fieldB where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Outer2_fieldB where

  readRaw =
    \ptr0 ->
          pure Outer2_fieldB
      <*> HasCField.readRaw (RIP.Proxy @"outer2_fieldB_fieldX") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"outer2_fieldB_fieldY") ptr0

instance Marshal.WriteRaw Outer2_fieldB where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Outer2_fieldB outer2_fieldB_fieldX2 outer2_fieldB_fieldY3 ->
               HasCField.writeRaw (RIP.Proxy @"outer2_fieldB_fieldX") ptr0 outer2_fieldB_fieldX2
            >> HasCField.writeRaw (RIP.Proxy @"outer2_fieldB_fieldY") ptr0 outer2_fieldB_fieldY3

deriving via Marshal.EquivStorable Outer2_fieldB instance RIP.Storable Outer2_fieldB

instance HasCField.HasCField Outer2_fieldB "outer2_fieldB_fieldX" where

  type CFieldType Outer2_fieldB "outer2_fieldB_fieldX" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "outer2_fieldB_fieldX" (RIP.Ptr Outer2_fieldB) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer2_fieldB_fieldX")

instance HasCField.HasCField Outer2_fieldB "outer2_fieldB_fieldY" where

  type CFieldType Outer2_fieldB "outer2_fieldB_fieldY" =
    RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "outer2_fieldB_fieldY" (RIP.Ptr Outer2_fieldB) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer2_fieldB_fieldY")

{-| __C declaration:__ @union outer2@

    __defined at:__ @types\/anonymous\/struct_in_union.h 15:7@

    __exported by:__ @types\/anonymous\/struct_in_union.h@
-}
newtype Outer2 = Outer2
  { unwrapOuter2 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 8 4 instance Marshal.StaticSize Outer2

deriving via RIP.SizedByteArray 8 4 instance Marshal.ReadRaw Outer2

deriving via RIP.SizedByteArray 8 4 instance Marshal.WriteRaw Outer2

deriving via Marshal.EquivStorable Outer2 instance RIP.Storable Outer2

{-|

    __See:__ 'set_outer2_fieldA'

    __C declaration:__ @fieldA@

    __defined at:__ @types\/anonymous\/struct_in_union.h 16:8@

    __exported by:__ @types\/anonymous\/struct_in_union.h@
-}
get_outer2_fieldA ::
     Outer2
  -> RIP.CChar
get_outer2_fieldA = RIP.getUnionPayload

{-|

    __See:__ 'get_outer2_fieldA'

-}
set_outer2_fieldA ::
     RIP.CChar
  -> Outer2
set_outer2_fieldA = RIP.setUnionPayload

{-|

    __See:__ 'set_outer2_fieldB'

    __C declaration:__ @fieldB@

    __defined at:__ @types\/anonymous\/struct_in_union.h 20:5@

    __exported by:__ @types\/anonymous\/struct_in_union.h@
-}
get_outer2_fieldB ::
     Outer2
  -> Outer2_fieldB
get_outer2_fieldB = RIP.getUnionPayload

{-|

    __See:__ 'get_outer2_fieldB'

-}
set_outer2_fieldB ::
     Outer2_fieldB
  -> Outer2
set_outer2_fieldB = RIP.setUnionPayload

{-|

    __See:__ 'set_outer2_fieldC'

    __C declaration:__ @fieldC@

    __defined at:__ @types\/anonymous\/struct_in_union.h 21:7@

    __exported by:__ @types\/anonymous\/struct_in_union.h@
-}
get_outer2_fieldC ::
     Outer2
  -> RIP.CInt
get_outer2_fieldC = RIP.getUnionPayload

{-|

    __See:__ 'get_outer2_fieldC'

-}
set_outer2_fieldC ::
     RIP.CInt
  -> Outer2
set_outer2_fieldC = RIP.setUnionPayload

instance HasCField.HasCField Outer2 "outer2_fieldA" where

  type CFieldType Outer2 "outer2_fieldA" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CChar
         ) => RIP.HasField "outer2_fieldA" (RIP.Ptr Outer2) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer2_fieldA")

instance HasCField.HasCField Outer2 "outer2_fieldB" where

  type CFieldType Outer2 "outer2_fieldB" =
    Outer2_fieldB

  offset# = \_ -> \_ -> 0

instance ( ty ~ Outer2_fieldB
         ) => RIP.HasField "outer2_fieldB" (RIP.Ptr Outer2) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer2_fieldB")

instance HasCField.HasCField Outer2 "outer2_fieldC" where

  type CFieldType Outer2 "outer2_fieldC" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "outer2_fieldC" (RIP.Ptr Outer2) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer2_fieldC")

{-| __C declaration:__ @struct inner3@

    __defined at:__ @types\/anonymous\/struct_in_union.h 26:10@

    __exported by:__ @types\/anonymous\/struct_in_union.h@
-}
data Inner3 = Inner3
  { inner3_fieldX :: RIP.CInt
    {- ^ __C declaration:__ @fieldX@

         __defined at:__ @types\/anonymous\/struct_in_union.h 27:9@

         __exported by:__ @types\/anonymous\/struct_in_union.h@
    -}
  , inner3_fieldY :: RIP.CInt
    {- ^ __C declaration:__ @fieldY@

         __defined at:__ @types\/anonymous\/struct_in_union.h 28:9@

         __exported by:__ @types\/anonymous\/struct_in_union.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Inner3 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Inner3 where

  readRaw =
    \ptr0 ->
          pure Inner3
      <*> HasCField.readRaw (RIP.Proxy @"inner3_fieldX") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"inner3_fieldY") ptr0

instance Marshal.WriteRaw Inner3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Inner3 inner3_fieldX2 inner3_fieldY3 ->
               HasCField.writeRaw (RIP.Proxy @"inner3_fieldX") ptr0 inner3_fieldX2
            >> HasCField.writeRaw (RIP.Proxy @"inner3_fieldY") ptr0 inner3_fieldY3

deriving via Marshal.EquivStorable Inner3 instance RIP.Storable Inner3

instance HasCField.HasCField Inner3 "inner3_fieldX" where

  type CFieldType Inner3 "inner3_fieldX" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "inner3_fieldX" (RIP.Ptr Inner3) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"inner3_fieldX")

instance HasCField.HasCField Inner3 "inner3_fieldY" where

  type CFieldType Inner3 "inner3_fieldY" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "inner3_fieldY" (RIP.Ptr Inner3) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"inner3_fieldY")

{-| __C declaration:__ @union outer3@

    __defined at:__ @types\/anonymous\/struct_in_union.h 24:7@

    __exported by:__ @types\/anonymous\/struct_in_union.h@
-}
newtype Outer3 = Outer3
  { unwrapOuter3 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 8 4 instance Marshal.StaticSize Outer3

deriving via RIP.SizedByteArray 8 4 instance Marshal.ReadRaw Outer3

deriving via RIP.SizedByteArray 8 4 instance Marshal.WriteRaw Outer3

deriving via Marshal.EquivStorable Outer3 instance RIP.Storable Outer3

{-|

    __See:__ 'set_outer3_fieldA'

    __C declaration:__ @fieldA@

    __defined at:__ @types\/anonymous\/struct_in_union.h 25:8@

    __exported by:__ @types\/anonymous\/struct_in_union.h@
-}
get_outer3_fieldA ::
     Outer3
  -> RIP.CChar
get_outer3_fieldA = RIP.getUnionPayload

{-|

    __See:__ 'get_outer3_fieldA'

-}
set_outer3_fieldA ::
     RIP.CChar
  -> Outer3
set_outer3_fieldA = RIP.setUnionPayload

{-|

    __See:__ 'set_outer3_fieldB'

    __C declaration:__ @fieldB@

    __defined at:__ @types\/anonymous\/struct_in_union.h 29:5@

    __exported by:__ @types\/anonymous\/struct_in_union.h@
-}
get_outer3_fieldB ::
     Outer3
  -> Inner3
get_outer3_fieldB = RIP.getUnionPayload

{-|

    __See:__ 'get_outer3_fieldB'

-}
set_outer3_fieldB ::
     Inner3
  -> Outer3
set_outer3_fieldB = RIP.setUnionPayload

{-|

    __See:__ 'set_outer3_fieldC'

    __C declaration:__ @fieldC@

    __defined at:__ @types\/anonymous\/struct_in_union.h 30:7@

    __exported by:__ @types\/anonymous\/struct_in_union.h@
-}
get_outer3_fieldC ::
     Outer3
  -> RIP.CInt
get_outer3_fieldC = RIP.getUnionPayload

{-|

    __See:__ 'get_outer3_fieldC'

-}
set_outer3_fieldC ::
     RIP.CInt
  -> Outer3
set_outer3_fieldC = RIP.setUnionPayload

instance HasCField.HasCField Outer3 "outer3_fieldA" where

  type CFieldType Outer3 "outer3_fieldA" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CChar
         ) => RIP.HasField "outer3_fieldA" (RIP.Ptr Outer3) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer3_fieldA")

instance HasCField.HasCField Outer3 "outer3_fieldB" where

  type CFieldType Outer3 "outer3_fieldB" = Inner3

  offset# = \_ -> \_ -> 0

instance ( ty ~ Inner3
         ) => RIP.HasField "outer3_fieldB" (RIP.Ptr Outer3) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer3_fieldB")

instance HasCField.HasCField Outer3 "outer3_fieldC" where

  type CFieldType Outer3 "outer3_fieldC" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "outer3_fieldC" (RIP.Ptr Outer3) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer3_fieldC")
