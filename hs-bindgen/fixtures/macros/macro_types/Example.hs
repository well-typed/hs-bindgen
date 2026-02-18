{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @PtrInt@

    __defined at:__ @macros\/macro_types.h 2:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype PtrInt = PtrInt
  { unwrapPtrInt :: RIP.Ptr RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr RIP.CInt)
         ) => RIP.HasField "unwrapPtrInt" (RIP.Ptr PtrInt) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPtrInt")

instance HasCField.HasCField PtrInt "unwrapPtrInt" where

  type CFieldType PtrInt "unwrapPtrInt" =
    RIP.Ptr RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @ShortInt@

    __defined at:__ @macros\/macro_types.h 3:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype ShortInt = ShortInt
  { unwrapShortInt :: RIP.CShort
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

instance ( ((~) ty) RIP.CShort
         ) => RIP.HasField "unwrapShortInt" (RIP.Ptr ShortInt) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapShortInt")

instance HasCField.HasCField ShortInt "unwrapShortInt" where

  type CFieldType ShortInt "unwrapShortInt" =
    RIP.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @SignedShortInt@

    __defined at:__ @macros\/macro_types.h 4:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype SignedShortInt = SignedShortInt
  { unwrapSignedShortInt :: RIP.CShort
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

instance ( ((~) ty) RIP.CShort
         ) => RIP.HasField "unwrapSignedShortInt" (RIP.Ptr SignedShortInt) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapSignedShortInt")

instance HasCField.HasCField SignedShortInt "unwrapSignedShortInt" where

  type CFieldType SignedShortInt "unwrapSignedShortInt" =
    RIP.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @UnsignedShortInt@

    __defined at:__ @macros\/macro_types.h 5:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype UnsignedShortInt = UnsignedShortInt
  { unwrapUnsignedShortInt :: RIP.CUShort
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

instance ( ((~) ty) RIP.CUShort
         ) => RIP.HasField "unwrapUnsignedShortInt" (RIP.Ptr UnsignedShortInt) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapUnsignedShortInt")

instance HasCField.HasCField UnsignedShortInt "unwrapUnsignedShortInt" where

  type CFieldType UnsignedShortInt "unwrapUnsignedShortInt" =
    RIP.CUShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @PtrPtrChar@

    __defined at:__ @macros\/macro_types.h 8:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype PtrPtrChar = PtrPtrChar
  { unwrapPtrPtrChar :: RIP.Ptr (RIP.Ptr RIP.CChar)
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr (RIP.Ptr RIP.CChar))
         ) => RIP.HasField "unwrapPtrPtrChar" (RIP.Ptr PtrPtrChar) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPtrPtrChar")

instance HasCField.HasCField PtrPtrChar "unwrapPtrPtrChar" where

  type CFieldType PtrPtrChar "unwrapPtrPtrChar" =
    RIP.Ptr (RIP.Ptr RIP.CChar)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MTy@

    __defined at:__ @macros\/macro_types.h 11:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype MTy = MTy
  { unwrapMTy :: RIP.CFloat
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( Enum
    , Floating
    , Fractional
    , RIP.HasFFIType
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , RealFloat
    , RealFrac
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) RIP.CFloat
         ) => RIP.HasField "unwrapMTy" (RIP.Ptr MTy) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapMTy")

instance HasCField.HasCField MTy "unwrapMTy" where

  type CFieldType MTy "unwrapMTy" = RIP.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @tty@

    __defined at:__ @macros\/macro_types.h 12:13@

    __exported by:__ @macros\/macro_types.h@
-}
newtype Tty = Tty
  { unwrapTty :: MTy
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( Enum
    , Floating
    , Fractional
    , RIP.HasFFIType
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , RealFloat
    , RealFrac
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) MTy
         ) => RIP.HasField "unwrapTty" (RIP.Ptr Tty) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapTty")

instance HasCField.HasCField Tty "unwrapTty" where

  type CFieldType Tty "unwrapTty" = MTy

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @UINT8_T@

    __defined at:__ @macros\/macro_types.h 14:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype UINT8_T = UINT8_T
  { unwrapUINT8_T :: RIP.CUChar
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

instance ( ((~) ty) RIP.CUChar
         ) => RIP.HasField "unwrapUINT8_T" (RIP.Ptr UINT8_T) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapUINT8_T")

instance HasCField.HasCField UINT8_T "unwrapUINT8_T" where

  type CFieldType UINT8_T "unwrapUINT8_T" = RIP.CUChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @BOOLEAN_T@

    __defined at:__ @macros\/macro_types.h 15:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype BOOLEAN_T = BOOLEAN_T
  { unwrapBOOLEAN_T :: UINT8_T
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

instance ( ((~) ty) UINT8_T
         ) => RIP.HasField "unwrapBOOLEAN_T" (RIP.Ptr BOOLEAN_T) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapBOOLEAN_T")

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

instance ( ((~) ty) BOOLEAN_T
         ) => RIP.HasField "unwrapBoolean_T" (RIP.Ptr Boolean_T) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapBoolean_T")

instance HasCField.HasCField Boolean_T "unwrapBoolean_T" where

  type CFieldType Boolean_T "unwrapBoolean_T" =
    BOOLEAN_T

  offset# = \_ -> \_ -> 0
