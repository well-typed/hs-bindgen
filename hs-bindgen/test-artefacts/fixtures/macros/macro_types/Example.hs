{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.PtrInt(..)
    , Example.ShortInt(..)
    , Example.SignedShortInt(..)
    , Example.UnsignedShortInt(..)
    , Example.PtrPtrChar(..)
    , Example.MTy(..)
    , Example.Tty(..)
    , Example.UINT8_T(..)
    , Example.BOOLEAN_T(..)
    , Example.Boolean_T(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro PtrInt@

    __defined at:__ @macros\/macro_types.h 2:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype PtrInt = PtrInt
  { unwrapPtrInt :: BG.Ptr BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.CompatHasField.HasField "unwrapPtrInt" PtrInt ty where

  hasField =
    \x0 ->
      (\y1 ->
         PtrInt {unwrapPtrInt = y1}, BG.getField @"unwrapPtrInt" x0)

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.HasField "unwrapPtrInt" (BG.Ptr PtrInt) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPtrInt")

instance HasCField.HasCField PtrInt "unwrapPtrInt" where

  type CFieldType PtrInt "unwrapPtrInt" =
    BG.Ptr BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro ShortInt@

    __defined at:__ @macros\/macro_types.h 3:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype ShortInt = ShortInt
  { unwrapShortInt :: BG.CShort
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

instance ( ty ~ BG.CShort
         ) => BG.CompatHasField.HasField "unwrapShortInt" ShortInt ty where

  hasField =
    \x0 ->
      (\y1 ->
         ShortInt {unwrapShortInt = y1}, BG.getField @"unwrapShortInt" x0)

instance ( ty ~ BG.CShort
         ) => BG.HasField "unwrapShortInt" (BG.Ptr ShortInt) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapShortInt")

instance HasCField.HasCField ShortInt "unwrapShortInt" where

  type CFieldType ShortInt "unwrapShortInt" = BG.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro SignedShortInt@

    __defined at:__ @macros\/macro_types.h 4:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype SignedShortInt = SignedShortInt
  { unwrapSignedShortInt :: BG.CShort
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

instance ( ty ~ BG.CShort
         ) => BG.CompatHasField.HasField "unwrapSignedShortInt" SignedShortInt ty where

  hasField =
    \x0 ->
      ( \y1 -> SignedShortInt {unwrapSignedShortInt = y1}
      , BG.getField @"unwrapSignedShortInt" x0
      )

instance ( ty ~ BG.CShort
         ) => BG.HasField "unwrapSignedShortInt" (BG.Ptr SignedShortInt) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapSignedShortInt")

instance HasCField.HasCField SignedShortInt "unwrapSignedShortInt" where

  type CFieldType SignedShortInt "unwrapSignedShortInt" =
    BG.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro UnsignedShortInt@

    __defined at:__ @macros\/macro_types.h 5:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype UnsignedShortInt = UnsignedShortInt
  { unwrapUnsignedShortInt :: BG.CUShort
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

instance ( ty ~ BG.CUShort
         ) => BG.CompatHasField.HasField "unwrapUnsignedShortInt" UnsignedShortInt ty where

  hasField =
    \x0 ->
      ( \y1 ->
          UnsignedShortInt {unwrapUnsignedShortInt = y1}
      , BG.getField @"unwrapUnsignedShortInt" x0
      )

instance ( ty ~ BG.CUShort
         ) => BG.HasField "unwrapUnsignedShortInt" (BG.Ptr UnsignedShortInt) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapUnsignedShortInt")

instance HasCField.HasCField UnsignedShortInt "unwrapUnsignedShortInt" where

  type CFieldType UnsignedShortInt "unwrapUnsignedShortInt" =
    BG.CUShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro PtrPtrChar@

    __defined at:__ @macros\/macro_types.h 8:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype PtrPtrChar = PtrPtrChar
  { unwrapPtrPtrChar :: BG.Ptr (BG.Ptr BG.CChar)
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr (BG.Ptr BG.CChar)
         ) => BG.CompatHasField.HasField "unwrapPtrPtrChar" PtrPtrChar ty where

  hasField =
    \x0 ->
      ( \y1 -> PtrPtrChar {unwrapPtrPtrChar = y1}
      , BG.getField @"unwrapPtrPtrChar" x0
      )

instance ( ty ~ BG.Ptr (BG.Ptr BG.CChar)
         ) => BG.HasField "unwrapPtrPtrChar" (BG.Ptr PtrPtrChar) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPtrPtrChar")

instance HasCField.HasCField PtrPtrChar "unwrapPtrPtrChar" where

  type CFieldType PtrPtrChar "unwrapPtrPtrChar" =
    BG.Ptr (BG.Ptr BG.CChar)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro MTy@

    __defined at:__ @macros\/macro_types.h 11:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype MTy = MTy
  { unwrapMTy :: BG.CFloat
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( Enum
    , Floating
    , Fractional
    , BG.HasFFIType
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , RealFloat
    , RealFrac
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ BG.CFloat) => BG.CompatHasField.HasField "unwrapMTy" MTy ty where

  hasField =
    \x0 ->
      (\y1 ->
         MTy {unwrapMTy = y1}, BG.getField @"unwrapMTy" x0)

instance ( ty ~ BG.CFloat
         ) => BG.HasField "unwrapMTy" (BG.Ptr MTy) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapMTy")

instance HasCField.HasCField MTy "unwrapMTy" where

  type CFieldType MTy "unwrapMTy" = BG.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @tty@

    __defined at:__ @macros\/macro_types.h 12:13@

    __exported by:__ @macros\/macro_types.h@
-}
newtype Tty = Tty
  { unwrapTty :: MTy
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( Enum
    , Floating
    , Fractional
    , BG.HasFFIType
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , RealFloat
    , RealFrac
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ MTy) => BG.CompatHasField.HasField "unwrapTty" Tty ty where

  hasField =
    \x0 ->
      (\y1 ->
         Tty {unwrapTty = y1}, BG.getField @"unwrapTty" x0)

instance (ty ~ MTy) => BG.HasField "unwrapTty" (BG.Ptr Tty) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapTty")

instance HasCField.HasCField Tty "unwrapTty" where

  type CFieldType Tty "unwrapTty" = MTy

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro UINT8_T@

    __defined at:__ @macros\/macro_types.h 14:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype UINT8_T = UINT8_T
  { unwrapUINT8_T :: BG.CUChar
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

instance ( ty ~ BG.CUChar
         ) => BG.CompatHasField.HasField "unwrapUINT8_T" UINT8_T ty where

  hasField =
    \x0 ->
      (\y1 ->
         UINT8_T {unwrapUINT8_T = y1}, BG.getField @"unwrapUINT8_T" x0)

instance ( ty ~ BG.CUChar
         ) => BG.HasField "unwrapUINT8_T" (BG.Ptr UINT8_T) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapUINT8_T")

instance HasCField.HasCField UINT8_T "unwrapUINT8_T" where

  type CFieldType UINT8_T "unwrapUINT8_T" = BG.CUChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro BOOLEAN_T@

    __defined at:__ @macros\/macro_types.h 15:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype BOOLEAN_T = BOOLEAN_T
  { unwrapBOOLEAN_T :: UINT8_T
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

instance ( ty ~ UINT8_T
         ) => BG.CompatHasField.HasField "unwrapBOOLEAN_T" BOOLEAN_T ty where

  hasField =
    \x0 ->
      (\y1 ->
         BOOLEAN_T {unwrapBOOLEAN_T = y1}, BG.getField @"unwrapBOOLEAN_T" x0)

instance ( ty ~ UINT8_T
         ) => BG.HasField "unwrapBOOLEAN_T" (BG.Ptr BOOLEAN_T) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapBOOLEAN_T")

instance HasCField.HasCField BOOLEAN_T "unwrapBOOLEAN_T" where

  type CFieldType BOOLEAN_T "unwrapBOOLEAN_T" = UINT8_T

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @boolean_T@

    __defined at:__ @macros\/macro_types.h 16:19@

    __exported by:__ @macros\/macro_types.h@
-}
newtype Boolean_T = Boolean_T
  { unwrapBoolean_T :: BOOLEAN_T
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

instance ( ty ~ BOOLEAN_T
         ) => BG.CompatHasField.HasField "unwrapBoolean_T" Boolean_T ty where

  hasField =
    \x0 ->
      (\y1 ->
         Boolean_T {unwrapBoolean_T = y1}, BG.getField @"unwrapBoolean_T" x0)

instance ( ty ~ BOOLEAN_T
         ) => BG.HasField "unwrapBoolean_T" (BG.Ptr Boolean_T) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapBoolean_T")

instance HasCField.HasCField Boolean_T "unwrapBoolean_T" where

  type CFieldType Boolean_T "unwrapBoolean_T" =
    BOOLEAN_T

  offset# = \_ -> \_ -> 0
