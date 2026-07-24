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
    ( Example.Foo_8(..)
    , Example.Foo_16(..)
    , Example.Foo_32(..)
    , Example.Foo_64(..)
    , Example.Bar_8_8(..)
    , Example.Bar_8_16(..)
    , Example.Bar_8_32(..)
    , Example.Bar_8_64(..)
    , Example.Bar_16_16(..)
    , Example.Bar_16_32(..)
    , Example.Bar_16_64(..)
    , Example.Bar_32_32(..)
    , Example.Bar_32_64(..)
    , Example.Bar_64_64(..)
    )
  where

import qualified HsBindgen.Runtime.BitfieldPtr as BitfieldPtr
import qualified HsBindgen.Runtime.HasCBitfield as HasCBitfield
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @union foo_8@

    __defined at:__ @types\/unions\/bitfields.h 4:7@

    __exported by:__ @types\/unions\/bitfields.h@
-}
newtype Foo_8 = Foo_8
  { unwrapFoo_8 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 1 1 instance Marshal.StaticSize Foo_8

deriving via BG.SizedByteArray 1 1 instance Marshal.ReadRaw Foo_8

deriving via BG.SizedByteArray 1 1 instance Marshal.WriteRaw Foo_8

deriving via Marshal.EquivStorable Foo_8 instance BG.Storable Foo_8

deriving via BG.SizedByteArray 1 1 instance Union.IsUnion Foo_8

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 5:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CSChar) => BG.HasField "foo_8_a" Foo_8 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 5:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CSChar) => BG.CompatHasField.HasField "foo_8_a" Foo_8 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_8_a" x0)

instance ( ty ~ BG.CSChar
         ) => BG.HasField "foo_8_a" (BG.Ptr Foo_8) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_8_a")

instance HasCBitfield.HasCBitfield Foo_8 "foo_8_a" where

  type CBitfieldType Foo_8 "foo_8_a" = BG.CSChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 3

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 6:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CSChar) => BG.HasField "foo_8_b" Foo_8 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 6:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CSChar) => BG.CompatHasField.HasField "foo_8_b" Foo_8 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_8_b" x0)

instance ( ty ~ BG.CSChar
         ) => BG.HasField "foo_8_b" (BG.Ptr Foo_8) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_8_b")

instance HasCBitfield.HasCBitfield Foo_8 "foo_8_b" where

  type CBitfieldType Foo_8 "foo_8_b" = BG.CSChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 3

{-| __C declaration:__ @c@

    __defined at:__ @types\/unions\/bitfields.h 7:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CSChar) => BG.HasField "foo_8_c" Foo_8 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @c@

    __defined at:__ @types\/unions\/bitfields.h 7:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CSChar) => BG.CompatHasField.HasField "foo_8_c" Foo_8 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_8_c" x0)

instance ( ty ~ BG.CSChar
         ) => BG.HasField "foo_8_c" (BG.Ptr Foo_8) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_8_c")

instance HasCBitfield.HasCBitfield Foo_8 "foo_8_c" where

  type CBitfieldType Foo_8 "foo_8_c" = BG.CSChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 2

{-| __C declaration:__ @d@

    __defined at:__ @types\/unions\/bitfields.h 8:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CSChar) => BG.HasField "foo_8_d" Foo_8 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @d@

    __defined at:__ @types\/unions\/bitfields.h 8:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CSChar) => BG.CompatHasField.HasField "foo_8_d" Foo_8 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_8_d" x0)

instance ( ty ~ BG.CSChar
         ) => BG.HasField "foo_8_d" (BG.Ptr Foo_8) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_8_d")

instance HasCBitfield.HasCBitfield Foo_8 "foo_8_d" where

  type CBitfieldType Foo_8 "foo_8_d" = BG.CSChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 3

{-| __C declaration:__ @e@

    __defined at:__ @types\/unions\/bitfields.h 9:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CSChar) => BG.HasField "foo_8_e" Foo_8 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @e@

    __defined at:__ @types\/unions\/bitfields.h 9:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CSChar) => BG.CompatHasField.HasField "foo_8_e" Foo_8 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_8_e" x0)

instance ( ty ~ BG.CSChar
         ) => BG.HasField "foo_8_e" (BG.Ptr Foo_8) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_8_e")

instance HasCBitfield.HasCBitfield Foo_8 "foo_8_e" where

  type CBitfieldType Foo_8 "foo_8_e" = BG.CSChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 8

{-| __C declaration:__ @f@

    __defined at:__ @types\/unions\/bitfields.h 10:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CSChar) => BG.HasField "foo_8_f" Foo_8 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @f@

    __defined at:__ @types\/unions\/bitfields.h 10:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CSChar) => BG.CompatHasField.HasField "foo_8_f" Foo_8 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_8_f" x0)

instance ( ty ~ BG.CSChar
         ) => BG.HasField "foo_8_f" (BG.Ptr Foo_8) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_8_f")

instance HasCBitfield.HasCBitfield Foo_8 "foo_8_f" where

  type CBitfieldType Foo_8 "foo_8_f" = BG.CSChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 5

{-| __C declaration:__ @union foo_16@

    __defined at:__ @types\/unions\/bitfields.h 14:7@

    __exported by:__ @types\/unions\/bitfields.h@
-}
newtype Foo_16 = Foo_16
  { unwrapFoo_16 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize Foo_16

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw Foo_16

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw Foo_16

deriving via Marshal.EquivStorable Foo_16 instance BG.Storable Foo_16

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion Foo_16

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 15:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CSChar) => BG.HasField "foo_16_a" Foo_16 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 15:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CSChar
         ) => BG.CompatHasField.HasField "foo_16_a" Foo_16 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_16_a" x0)

instance ( ty ~ BG.CSChar
         ) => BG.HasField "foo_16_a" (BG.Ptr Foo_16) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_16_a")

instance HasCBitfield.HasCBitfield Foo_16 "foo_16_a" where

  type CBitfieldType Foo_16 "foo_16_a" = BG.CSChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 6

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 16:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "foo_16_b" Foo_16 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 16:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "foo_16_b" Foo_16 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_16_b" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "foo_16_b" (BG.Ptr Foo_16) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_16_b")

instance HasCBitfield.HasCBitfield Foo_16 "foo_16_b" where

  type CBitfieldType Foo_16 "foo_16_b" = BG.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 10

{-| __C declaration:__ @c@

    __defined at:__ @types\/unions\/bitfields.h 17:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "foo_16_c" Foo_16 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @c@

    __defined at:__ @types\/unions\/bitfields.h 17:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "foo_16_c" Foo_16 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_16_c" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "foo_16_c" (BG.Ptr Foo_16) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_16_c")

instance HasCBitfield.HasCBitfield Foo_16 "foo_16_c" where

  type CBitfieldType Foo_16 "foo_16_c" = BG.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 16

{-| __C declaration:__ @d@

    __defined at:__ @types\/unions\/bitfields.h 18:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "foo_16_d" Foo_16 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @d@

    __defined at:__ @types\/unions\/bitfields.h 18:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "foo_16_d" Foo_16 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_16_d" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "foo_16_d" (BG.Ptr Foo_16) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_16_d")

instance HasCBitfield.HasCBitfield Foo_16 "foo_16_d" where

  type CBitfieldType Foo_16 "foo_16_d" = BG.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 16

{-| __C declaration:__ @e@

    __defined at:__ @types\/unions\/bitfields.h 19:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "foo_16_e" Foo_16 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @e@

    __defined at:__ @types\/unions\/bitfields.h 19:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "foo_16_e" Foo_16 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_16_e" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "foo_16_e" (BG.Ptr Foo_16) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_16_e")

instance HasCBitfield.HasCBitfield Foo_16 "foo_16_e" where

  type CBitfieldType Foo_16 "foo_16_e" = BG.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 12

{-| __C declaration:__ @f@

    __defined at:__ @types\/unions\/bitfields.h 20:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "foo_16_f" Foo_16 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @f@

    __defined at:__ @types\/unions\/bitfields.h 20:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "foo_16_f" Foo_16 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_16_f" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "foo_16_f" (BG.Ptr Foo_16) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_16_f")

instance HasCBitfield.HasCBitfield Foo_16 "foo_16_f" where

  type CBitfieldType Foo_16 "foo_16_f" = BG.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 12

{-| __C declaration:__ @union foo_32@

    __defined at:__ @types\/unions\/bitfields.h 24:7@

    __exported by:__ @types\/unions\/bitfields.h@
-}
newtype Foo_32 = Foo_32
  { unwrapFoo_32 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 8 8 instance Marshal.StaticSize Foo_32

deriving via BG.SizedByteArray 8 8 instance Marshal.ReadRaw Foo_32

deriving via BG.SizedByteArray 8 8 instance Marshal.WriteRaw Foo_32

deriving via Marshal.EquivStorable Foo_32 instance BG.Storable Foo_32

deriving via BG.SizedByteArray 8 8 instance Union.IsUnion Foo_32

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 25:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CSChar) => BG.HasField "foo_32_a" Foo_32 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 25:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CSChar
         ) => BG.CompatHasField.HasField "foo_32_a" Foo_32 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_32_a" x0)

instance ( ty ~ BG.CSChar
         ) => BG.HasField "foo_32_a" (BG.Ptr Foo_32) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_32_a")

instance HasCBitfield.HasCBitfield Foo_32 "foo_32_a" where

  type CBitfieldType Foo_32 "foo_32_a" = BG.CSChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 6

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 26:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "foo_32_b" Foo_32 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 26:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "foo_32_b" Foo_32 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_32_b" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "foo_32_b" (BG.Ptr Foo_32) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_32_b")

instance HasCBitfield.HasCBitfield Foo_32 "foo_32_b" where

  type CBitfieldType Foo_32 "foo_32_b" = BG.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 12

{-| __C declaration:__ @c@

    __defined at:__ @types\/unions\/bitfields.h 27:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "foo_32_c" Foo_32 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @c@

    __defined at:__ @types\/unions\/bitfields.h 27:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "foo_32_c" Foo_32 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_32_c" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "foo_32_c" (BG.Ptr Foo_32) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_32_c")

instance HasCBitfield.HasCBitfield Foo_32 "foo_32_c" where

  type CBitfieldType Foo_32 "foo_32_c" = BG.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 14

{-| __C declaration:__ @d@

    __defined at:__ @types\/unions\/bitfields.h 28:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "foo_32_d" Foo_32 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @d@

    __defined at:__ @types\/unions\/bitfields.h 28:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "foo_32_d" Foo_32 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_32_d" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "foo_32_d" (BG.Ptr Foo_32) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_32_d")

instance HasCBitfield.HasCBitfield Foo_32 "foo_32_d" where

  type CBitfieldType Foo_32 "foo_32_d" = BG.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 10

{-| __C declaration:__ @e@

    __defined at:__ @types\/unions\/bitfields.h 29:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CLong) => BG.HasField "foo_32_e" Foo_32 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @e@

    __defined at:__ @types\/unions\/bitfields.h 29:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CLong
         ) => BG.CompatHasField.HasField "foo_32_e" Foo_32 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_32_e" x0)

instance ( ty ~ BG.CLong
         ) => BG.HasField "foo_32_e" (BG.Ptr Foo_32) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_32_e")

instance HasCBitfield.HasCBitfield Foo_32 "foo_32_e" where

  type CBitfieldType Foo_32 "foo_32_e" = BG.CLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 32

{-| __C declaration:__ @f@

    __defined at:__ @types\/unions\/bitfields.h 30:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "foo_32_f" Foo_32 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @f@

    __defined at:__ @types\/unions\/bitfields.h 30:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "foo_32_f" Foo_32 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_32_f" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "foo_32_f" (BG.Ptr Foo_32) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_32_f")

instance HasCBitfield.HasCBitfield Foo_32 "foo_32_f" where

  type CBitfieldType Foo_32 "foo_32_f" = BG.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 6

{-| __C declaration:__ @g@

    __defined at:__ @types\/unions\/bitfields.h 31:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CLong) => BG.HasField "foo_32_g" Foo_32 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @g@

    __defined at:__ @types\/unions\/bitfields.h 31:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CLong
         ) => BG.CompatHasField.HasField "foo_32_g" Foo_32 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_32_g" x0)

instance ( ty ~ BG.CLong
         ) => BG.HasField "foo_32_g" (BG.Ptr Foo_32) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_32_g")

instance HasCBitfield.HasCBitfield Foo_32 "foo_32_g" where

  type CBitfieldType Foo_32 "foo_32_g" = BG.CLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 24

{-| __C declaration:__ @union foo_64@

    __defined at:__ @types\/unions\/bitfields.h 35:7@

    __exported by:__ @types\/unions\/bitfields.h@
-}
newtype Foo_64 = Foo_64
  { unwrapFoo_64 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 8 8 instance Marshal.StaticSize Foo_64

deriving via BG.SizedByteArray 8 8 instance Marshal.ReadRaw Foo_64

deriving via BG.SizedByteArray 8 8 instance Marshal.WriteRaw Foo_64

deriving via Marshal.EquivStorable Foo_64 instance BG.Storable Foo_64

deriving via BG.SizedByteArray 8 8 instance Union.IsUnion Foo_64

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 36:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CLong) => BG.HasField "foo_64_a" Foo_64 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 36:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CLong
         ) => BG.CompatHasField.HasField "foo_64_a" Foo_64 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_64_a" x0)

instance ( ty ~ BG.CLong
         ) => BG.HasField "foo_64_a" (BG.Ptr Foo_64) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_64_a")

instance HasCBitfield.HasCBitfield Foo_64 "foo_64_a" where

  type CBitfieldType Foo_64 "foo_64_a" = BG.CLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 24

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 37:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CLLong) => BG.HasField "foo_64_b" Foo_64 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 37:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CLLong
         ) => BG.CompatHasField.HasField "foo_64_b" Foo_64 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_64_b" x0)

instance ( ty ~ BG.CLLong
         ) => BG.HasField "foo_64_b" (BG.Ptr Foo_64) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_64_b")

instance HasCBitfield.HasCBitfield Foo_64 "foo_64_b" where

  type CBitfieldType Foo_64 "foo_64_b" = BG.CLLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 40

{-| __C declaration:__ @c@

    __defined at:__ @types\/unions\/bitfields.h 38:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CLLong) => BG.HasField "foo_64_c" Foo_64 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @c@

    __defined at:__ @types\/unions\/bitfields.h 38:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CLLong
         ) => BG.CompatHasField.HasField "foo_64_c" Foo_64 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_64_c" x0)

instance ( ty ~ BG.CLLong
         ) => BG.HasField "foo_64_c" (BG.Ptr Foo_64) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_64_c")

instance HasCBitfield.HasCBitfield Foo_64 "foo_64_c" where

  type CBitfieldType Foo_64 "foo_64_c" = BG.CLLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 64

{-| __C declaration:__ @d@

    __defined at:__ @types\/unions\/bitfields.h 39:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CLLong) => BG.HasField "foo_64_d" Foo_64 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @d@

    __defined at:__ @types\/unions\/bitfields.h 39:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CLLong
         ) => BG.CompatHasField.HasField "foo_64_d" Foo_64 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"foo_64_d" x0)

instance ( ty ~ BG.CLLong
         ) => BG.HasField "foo_64_d" (BG.Ptr Foo_64) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_64_d")

instance HasCBitfield.HasCBitfield Foo_64 "foo_64_d" where

  type CBitfieldType Foo_64 "foo_64_d" = BG.CLLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 36

{-| __C declaration:__ @union bar_8_8@

    __defined at:__ @types\/unions\/bitfields.h 43:31@

    __exported by:__ @types\/unions\/bitfields.h@
-}
newtype Bar_8_8 = Bar_8_8
  { unwrapBar_8_8 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 1 1 instance Marshal.StaticSize Bar_8_8

deriving via BG.SizedByteArray 1 1 instance Marshal.ReadRaw Bar_8_8

deriving via BG.SizedByteArray 1 1 instance Marshal.WriteRaw Bar_8_8

deriving via Marshal.EquivStorable Bar_8_8 instance BG.Storable Bar_8_8

deriving via BG.SizedByteArray 1 1 instance Union.IsUnion Bar_8_8

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 44:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CSChar) => BG.HasField "bar_8_8_a" Bar_8_8 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 44:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CSChar
         ) => BG.CompatHasField.HasField "bar_8_8_a" Bar_8_8 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_8_8_a" x0)

instance ( ty ~ BG.CSChar
         ) => BG.HasField "bar_8_8_a" (BG.Ptr Bar_8_8) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"bar_8_8_a")

instance HasCBitfield.HasCBitfield Bar_8_8 "bar_8_8_a" where

  type CBitfieldType Bar_8_8 "bar_8_8_a" = BG.CSChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 6

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 45:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "bar_8_8_b" Bar_8_8 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 45:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "bar_8_8_b" Bar_8_8 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_8_8_b" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "bar_8_8_b" (BG.Ptr Bar_8_8) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"bar_8_8_b")

instance HasCBitfield.HasCBitfield Bar_8_8 "bar_8_8_b" where

  type CBitfieldType Bar_8_8 "bar_8_8_b" = BG.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 4

{-| __C declaration:__ @union bar_8_16@

    __defined at:__ @types\/unions\/bitfields.h 49:31@

    __exported by:__ @types\/unions\/bitfields.h@
-}
newtype Bar_8_16 = Bar_8_16
  { unwrapBar_8_16 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 2 1 instance Marshal.StaticSize Bar_8_16

deriving via BG.SizedByteArray 2 1 instance Marshal.ReadRaw Bar_8_16

deriving via BG.SizedByteArray 2 1 instance Marshal.WriteRaw Bar_8_16

deriving via Marshal.EquivStorable Bar_8_16 instance BG.Storable Bar_8_16

deriving via BG.SizedByteArray 2 1 instance Union.IsUnion Bar_8_16

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 50:14@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "bar_8_16_a" Bar_8_16 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 50:14@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "bar_8_16_a" Bar_8_16 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_8_16_a" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "bar_8_16_a" (BG.Ptr Bar_8_16) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"bar_8_16_a")

instance HasCBitfield.HasCBitfield Bar_8_16 "bar_8_16_a" where

  type CBitfieldType Bar_8_16 "bar_8_16_a" = BG.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 14

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 51:14@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "bar_8_16_b" Bar_8_16 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 51:14@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "bar_8_16_b" Bar_8_16 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_8_16_b" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "bar_8_16_b" (BG.Ptr Bar_8_16) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"bar_8_16_b")

instance HasCBitfield.HasCBitfield Bar_8_16 "bar_8_16_b" where

  type CBitfieldType Bar_8_16 "bar_8_16_b" = BG.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 4

{-| __C declaration:__ @union bar_8_32@

    __defined at:__ @types\/unions\/bitfields.h 55:31@

    __exported by:__ @types\/unions\/bitfields.h@
-}
newtype Bar_8_32 = Bar_8_32
  { unwrapBar_8_32 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 1 instance Marshal.StaticSize Bar_8_32

deriving via BG.SizedByteArray 4 1 instance Marshal.ReadRaw Bar_8_32

deriving via BG.SizedByteArray 4 1 instance Marshal.WriteRaw Bar_8_32

deriving via Marshal.EquivStorable Bar_8_32 instance BG.Storable Bar_8_32

deriving via BG.SizedByteArray 4 1 instance Union.IsUnion Bar_8_32

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 56:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CLong) => BG.HasField "bar_8_32_a" Bar_8_32 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 56:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CLong
         ) => BG.CompatHasField.HasField "bar_8_32_a" Bar_8_32 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_8_32_a" x0)

instance ( ty ~ BG.CLong
         ) => BG.HasField "bar_8_32_a" (BG.Ptr Bar_8_32) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"bar_8_32_a")

instance HasCBitfield.HasCBitfield Bar_8_32 "bar_8_32_a" where

  type CBitfieldType Bar_8_32 "bar_8_32_a" = BG.CLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 30

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 57:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "bar_8_32_b" Bar_8_32 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 57:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "bar_8_32_b" Bar_8_32 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_8_32_b" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "bar_8_32_b" (BG.Ptr Bar_8_32) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"bar_8_32_b")

instance HasCBitfield.HasCBitfield Bar_8_32 "bar_8_32_b" where

  type CBitfieldType Bar_8_32 "bar_8_32_b" = BG.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 4

{-| __C declaration:__ @union bar_8_64@

    __defined at:__ @types\/unions\/bitfields.h 61:31@

    __exported by:__ @types\/unions\/bitfields.h@
-}
newtype Bar_8_64 = Bar_8_64
  { unwrapBar_8_64 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 8 1 instance Marshal.StaticSize Bar_8_64

deriving via BG.SizedByteArray 8 1 instance Marshal.ReadRaw Bar_8_64

deriving via BG.SizedByteArray 8 1 instance Marshal.WriteRaw Bar_8_64

deriving via Marshal.EquivStorable Bar_8_64 instance BG.Storable Bar_8_64

deriving via BG.SizedByteArray 8 1 instance Union.IsUnion Bar_8_64

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 62:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CLLong) => BG.HasField "bar_8_64_a" Bar_8_64 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 62:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CLLong
         ) => BG.CompatHasField.HasField "bar_8_64_a" Bar_8_64 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_8_64_a" x0)

instance ( ty ~ BG.CLLong
         ) => BG.HasField "bar_8_64_a" (BG.Ptr Bar_8_64) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"bar_8_64_a")

instance HasCBitfield.HasCBitfield Bar_8_64 "bar_8_64_a" where

  type CBitfieldType Bar_8_64 "bar_8_64_a" = BG.CLLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 62

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 63:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "bar_8_64_b" Bar_8_64 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 63:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "bar_8_64_b" Bar_8_64 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_8_64_b" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "bar_8_64_b" (BG.Ptr Bar_8_64) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"bar_8_64_b")

instance HasCBitfield.HasCBitfield Bar_8_64 "bar_8_64_b" where

  type CBitfieldType Bar_8_64 "bar_8_64_b" = BG.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 4

{-| __C declaration:__ @union bar_16_16@

    __defined at:__ @types\/unions\/bitfields.h 67:31@

    __exported by:__ @types\/unions\/bitfields.h@
-}
newtype Bar_16_16 = Bar_16_16
  { unwrapBar_16_16 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 2 1 instance Marshal.StaticSize Bar_16_16

deriving via BG.SizedByteArray 2 1 instance Marshal.ReadRaw Bar_16_16

deriving via BG.SizedByteArray 2 1 instance Marshal.WriteRaw Bar_16_16

deriving via Marshal.EquivStorable Bar_16_16 instance BG.Storable Bar_16_16

deriving via BG.SizedByteArray 2 1 instance Union.IsUnion Bar_16_16

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 68:14@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "bar_16_16_a" Bar_16_16 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 68:14@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "bar_16_16_a" Bar_16_16 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_16_16_a" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "bar_16_16_a" (BG.Ptr Bar_16_16) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"bar_16_16_a")

instance HasCBitfield.HasCBitfield Bar_16_16 "bar_16_16_a" where

  type CBitfieldType Bar_16_16 "bar_16_16_a" = BG.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 14

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 69:14@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "bar_16_16_b" Bar_16_16 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 69:14@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "bar_16_16_b" Bar_16_16 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_16_16_b" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "bar_16_16_b" (BG.Ptr Bar_16_16) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"bar_16_16_b")

instance HasCBitfield.HasCBitfield Bar_16_16 "bar_16_16_b" where

  type CBitfieldType Bar_16_16 "bar_16_16_b" = BG.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 14

{-| __C declaration:__ @union bar_16_32@

    __defined at:__ @types\/unions\/bitfields.h 73:31@

    __exported by:__ @types\/unions\/bitfields.h@
-}
newtype Bar_16_32 = Bar_16_32
  { unwrapBar_16_32 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 3 1 instance Marshal.StaticSize Bar_16_32

deriving via BG.SizedByteArray 3 1 instance Marshal.ReadRaw Bar_16_32

deriving via BG.SizedByteArray 3 1 instance Marshal.WriteRaw Bar_16_32

deriving via Marshal.EquivStorable Bar_16_32 instance BG.Storable Bar_16_32

deriving via BG.SizedByteArray 3 1 instance Union.IsUnion Bar_16_32

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 74:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CLong) => BG.HasField "bar_16_32_a" Bar_16_32 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 74:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CLong
         ) => BG.CompatHasField.HasField "bar_16_32_a" Bar_16_32 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_16_32_a" x0)

instance ( ty ~ BG.CLong
         ) => BG.HasField "bar_16_32_a" (BG.Ptr Bar_16_32) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"bar_16_32_a")

instance HasCBitfield.HasCBitfield Bar_16_32 "bar_16_32_a" where

  type CBitfieldType Bar_16_32 "bar_16_32_a" = BG.CLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 24

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 75:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "bar_16_32_b" Bar_16_32 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 75:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "bar_16_32_b" Bar_16_32 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_16_32_b" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "bar_16_32_b" (BG.Ptr Bar_16_32) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"bar_16_32_b")

instance HasCBitfield.HasCBitfield Bar_16_32 "bar_16_32_b" where

  type CBitfieldType Bar_16_32 "bar_16_32_b" = BG.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 14

{-| __C declaration:__ @union bar_16_64@

    __defined at:__ @types\/unions\/bitfields.h 79:31@

    __exported by:__ @types\/unions\/bitfields.h@
-}
newtype Bar_16_64 = Bar_16_64
  { unwrapBar_16_64 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 7 1 instance Marshal.StaticSize Bar_16_64

deriving via BG.SizedByteArray 7 1 instance Marshal.ReadRaw Bar_16_64

deriving via BG.SizedByteArray 7 1 instance Marshal.WriteRaw Bar_16_64

deriving via Marshal.EquivStorable Bar_16_64 instance BG.Storable Bar_16_64

deriving via BG.SizedByteArray 7 1 instance Union.IsUnion Bar_16_64

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 80:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CLLong) => BG.HasField "bar_16_64_a" Bar_16_64 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 80:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CLLong
         ) => BG.CompatHasField.HasField "bar_16_64_a" Bar_16_64 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_16_64_a" x0)

instance ( ty ~ BG.CLLong
         ) => BG.HasField "bar_16_64_a" (BG.Ptr Bar_16_64) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"bar_16_64_a")

instance HasCBitfield.HasCBitfield Bar_16_64 "bar_16_64_a" where

  type CBitfieldType Bar_16_64 "bar_16_64_a" =
    BG.CLLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 56

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 81:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "bar_16_64_b" Bar_16_64 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 81:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "bar_16_64_b" Bar_16_64 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_16_64_b" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "bar_16_64_b" (BG.Ptr Bar_16_64) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"bar_16_64_b")

instance HasCBitfield.HasCBitfield Bar_16_64 "bar_16_64_b" where

  type CBitfieldType Bar_16_64 "bar_16_64_b" = BG.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 14

{-| __C declaration:__ @union bar_32_32@

    __defined at:__ @types\/unions\/bitfields.h 85:31@

    __exported by:__ @types\/unions\/bitfields.h@
-}
newtype Bar_32_32 = Bar_32_32
  { unwrapBar_32_32 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 1 instance Marshal.StaticSize Bar_32_32

deriving via BG.SizedByteArray 4 1 instance Marshal.ReadRaw Bar_32_32

deriving via BG.SizedByteArray 4 1 instance Marshal.WriteRaw Bar_32_32

deriving via Marshal.EquivStorable Bar_32_32 instance BG.Storable Bar_32_32

deriving via BG.SizedByteArray 4 1 instance Union.IsUnion Bar_32_32

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 86:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CLong) => BG.HasField "bar_32_32_a" Bar_32_32 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 86:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CLong
         ) => BG.CompatHasField.HasField "bar_32_32_a" Bar_32_32 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_32_32_a" x0)

instance ( ty ~ BG.CLong
         ) => BG.HasField "bar_32_32_a" (BG.Ptr Bar_32_32) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"bar_32_32_a")

instance HasCBitfield.HasCBitfield Bar_32_32 "bar_32_32_a" where

  type CBitfieldType Bar_32_32 "bar_32_32_a" = BG.CLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 30

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 87:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CLong) => BG.HasField "bar_32_32_b" Bar_32_32 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 87:15@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CLong
         ) => BG.CompatHasField.HasField "bar_32_32_b" Bar_32_32 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_32_32_b" x0)

instance ( ty ~ BG.CLong
         ) => BG.HasField "bar_32_32_b" (BG.Ptr Bar_32_32) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"bar_32_32_b")

instance HasCBitfield.HasCBitfield Bar_32_32 "bar_32_32_b" where

  type CBitfieldType Bar_32_32 "bar_32_32_b" = BG.CLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 30

{-| __C declaration:__ @union bar_32_64@

    __defined at:__ @types\/unions\/bitfields.h 91:31@

    __exported by:__ @types\/unions\/bitfields.h@
-}
newtype Bar_32_64 = Bar_32_64
  { unwrapBar_32_64 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 7 1 instance Marshal.StaticSize Bar_32_64

deriving via BG.SizedByteArray 7 1 instance Marshal.ReadRaw Bar_32_64

deriving via BG.SizedByteArray 7 1 instance Marshal.WriteRaw Bar_32_64

deriving via Marshal.EquivStorable Bar_32_64 instance BG.Storable Bar_32_64

deriving via BG.SizedByteArray 7 1 instance Union.IsUnion Bar_32_64

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 92:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CLLong) => BG.HasField "bar_32_64_a" Bar_32_64 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 92:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CLLong
         ) => BG.CompatHasField.HasField "bar_32_64_a" Bar_32_64 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_32_64_a" x0)

instance ( ty ~ BG.CLLong
         ) => BG.HasField "bar_32_64_a" (BG.Ptr Bar_32_64) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"bar_32_64_a")

instance HasCBitfield.HasCBitfield Bar_32_64 "bar_32_64_a" where

  type CBitfieldType Bar_32_64 "bar_32_64_a" =
    BG.CLLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 56

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 93:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CLong) => BG.HasField "bar_32_64_b" Bar_32_64 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 93:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CLong
         ) => BG.CompatHasField.HasField "bar_32_64_b" Bar_32_64 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_32_64_b" x0)

instance ( ty ~ BG.CLong
         ) => BG.HasField "bar_32_64_b" (BG.Ptr Bar_32_64) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"bar_32_64_b")

instance HasCBitfield.HasCBitfield Bar_32_64 "bar_32_64_b" where

  type CBitfieldType Bar_32_64 "bar_32_64_b" = BG.CLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 30

{-| __C declaration:__ @union bar_64_64@

    __defined at:__ @types\/unions\/bitfields.h 97:31@

    __exported by:__ @types\/unions\/bitfields.h@
-}
newtype Bar_64_64 = Bar_64_64
  { unwrapBar_64_64 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 7 1 instance Marshal.StaticSize Bar_64_64

deriving via BG.SizedByteArray 7 1 instance Marshal.ReadRaw Bar_64_64

deriving via BG.SizedByteArray 7 1 instance Marshal.WriteRaw Bar_64_64

deriving via Marshal.EquivStorable Bar_64_64 instance BG.Storable Bar_64_64

deriving via BG.SizedByteArray 7 1 instance Union.IsUnion Bar_64_64

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 98:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CLLong) => BG.HasField "bar_64_64_a" Bar_64_64 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/bitfields.h 98:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CLLong
         ) => BG.CompatHasField.HasField "bar_64_64_a" Bar_64_64 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_64_64_a" x0)

instance ( ty ~ BG.CLLong
         ) => BG.HasField "bar_64_64_a" (BG.Ptr Bar_64_64) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"bar_64_64_a")

instance HasCBitfield.HasCBitfield Bar_64_64 "bar_64_64_a" where

  type CBitfieldType Bar_64_64 "bar_64_64_a" =
    BG.CLLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 56

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 99:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance (ty ~ BG.CLLong) => BG.HasField "bar_64_64_b" Bar_64_64 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/bitfields.h 99:20@

    __exported by:__ @types\/unions\/bitfields.h@
-}
instance ( ty ~ BG.CLLong
         ) => BG.CompatHasField.HasField "bar_64_64_b" Bar_64_64 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"bar_64_64_b" x0)

instance ( ty ~ BG.CLLong
         ) => BG.HasField "bar_64_64_b" (BG.Ptr Bar_64_64) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"bar_64_64_b")

instance HasCBitfield.HasCBitfield Bar_64_64 "bar_64_64_b" where

  type CBitfieldType Bar_64_64 "bar_64_64_b" =
    BG.CLLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 40
