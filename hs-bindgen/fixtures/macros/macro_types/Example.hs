{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.Bitfield
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import Data.Bits (FiniteBits)
import Prelude (Bounded, Enum, Eq, Floating, Fractional, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

{-| __C declaration:__ @PtrInt@

    __defined at:__ @macros\/macro_types.h 2:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype PtrInt = PtrInt
  { unwrapPtrInt :: Ptr.Ptr FC.CInt
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance GHC.Records.HasField "unwrapPtrInt" (Ptr.Ptr PtrInt) (Ptr.Ptr (Ptr.Ptr FC.CInt)) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPtrInt")

instance HsBindgen.Runtime.HasCField.HasCField PtrInt "unwrapPtrInt" where

  type CFieldType PtrInt "unwrapPtrInt" =
    Ptr.Ptr FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @ShortInt@

    __defined at:__ @macros\/macro_types.h 3:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype ShortInt = ShortInt
  { unwrapShortInt :: FC.CShort
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance GHC.Records.HasField "unwrapShortInt" (Ptr.Ptr ShortInt) (Ptr.Ptr FC.CShort) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapShortInt")

instance HsBindgen.Runtime.HasCField.HasCField ShortInt "unwrapShortInt" where

  type CFieldType ShortInt "unwrapShortInt" = FC.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @SignedShortInt@

    __defined at:__ @macros\/macro_types.h 4:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype SignedShortInt = SignedShortInt
  { unwrapSignedShortInt :: FC.CShort
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance GHC.Records.HasField "unwrapSignedShortInt" (Ptr.Ptr SignedShortInt) (Ptr.Ptr FC.CShort) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapSignedShortInt")

instance HsBindgen.Runtime.HasCField.HasCField SignedShortInt "unwrapSignedShortInt" where

  type CFieldType SignedShortInt "unwrapSignedShortInt" =
    FC.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @UnsignedShortInt@

    __defined at:__ @macros\/macro_types.h 5:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype UnsignedShortInt = UnsignedShortInt
  { unwrapUnsignedShortInt :: FC.CUShort
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance GHC.Records.HasField "unwrapUnsignedShortInt" (Ptr.Ptr UnsignedShortInt) (Ptr.Ptr FC.CUShort) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapUnsignedShortInt")

instance HsBindgen.Runtime.HasCField.HasCField UnsignedShortInt "unwrapUnsignedShortInt" where

  type CFieldType UnsignedShortInt "unwrapUnsignedShortInt" =
    FC.CUShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @PtrPtrChar@

    __defined at:__ @macros\/macro_types.h 8:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype PtrPtrChar = PtrPtrChar
  { unwrapPtrPtrChar :: Ptr.Ptr (Ptr.Ptr FC.CChar)
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance GHC.Records.HasField "unwrapPtrPtrChar" (Ptr.Ptr PtrPtrChar) (Ptr.Ptr (Ptr.Ptr (Ptr.Ptr FC.CChar))) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPtrPtrChar")

instance HsBindgen.Runtime.HasCField.HasCField PtrPtrChar "unwrapPtrPtrChar" where

  type CFieldType PtrPtrChar "unwrapPtrPtrChar" =
    Ptr.Ptr (Ptr.Ptr FC.CChar)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MTy@

    __defined at:__ @macros\/macro_types.h 11:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype MTy = MTy
  { unwrapMTy :: FC.CFloat
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , Enum
    , Floating
    , Fractional
    , Num
    , Real
    , RealFloat
    , RealFrac
    )

instance GHC.Records.HasField "unwrapMTy" (Ptr.Ptr MTy) (Ptr.Ptr FC.CFloat) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMTy")

instance HsBindgen.Runtime.HasCField.HasCField MTy "unwrapMTy" where

  type CFieldType MTy "unwrapMTy" = FC.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @tty@

    __defined at:__ @macros\/macro_types.h 12:13@

    __exported by:__ @macros\/macro_types.h@
-}
newtype Tty = Tty
  { unwrapTty :: MTy
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , Enum
    , Floating
    , Fractional
    , Num
    , Real
    , RealFloat
    , RealFrac
    )

instance GHC.Records.HasField "unwrapTty" (Ptr.Ptr Tty) (Ptr.Ptr MTy) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTty")

instance HsBindgen.Runtime.HasCField.HasCField Tty "unwrapTty" where

  type CFieldType Tty "unwrapTty" = MTy

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @UINT8_T@

    __defined at:__ @macros\/macro_types.h 14:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype UINT8_T = UINT8_T
  { unwrapUINT8_T :: FC.CUChar
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance GHC.Records.HasField "unwrapUINT8_T" (Ptr.Ptr UINT8_T) (Ptr.Ptr FC.CUChar) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapUINT8_T")

instance HsBindgen.Runtime.HasCField.HasCField UINT8_T "unwrapUINT8_T" where

  type CFieldType UINT8_T "unwrapUINT8_T" = FC.CUChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @BOOLEAN_T@

    __defined at:__ @macros\/macro_types.h 15:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype BOOLEAN_T = BOOLEAN_T
  { unwrapBOOLEAN_T :: UINT8_T
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance GHC.Records.HasField "unwrapBOOLEAN_T" (Ptr.Ptr BOOLEAN_T) (Ptr.Ptr UINT8_T) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapBOOLEAN_T")

instance HsBindgen.Runtime.HasCField.HasCField BOOLEAN_T "unwrapBOOLEAN_T" where

  type CFieldType BOOLEAN_T "unwrapBOOLEAN_T" = UINT8_T

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @boolean_T@

    __defined at:__ @macros\/macro_types.h 16:19@

    __exported by:__ @macros\/macro_types.h@
-}
newtype Boolean_T = Boolean_T
  { unwrapBoolean_T :: BOOLEAN_T
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance GHC.Records.HasField "unwrapBoolean_T" (Ptr.Ptr Boolean_T) (Ptr.Ptr BOOLEAN_T) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapBoolean_T")

instance HsBindgen.Runtime.HasCField.HasCField Boolean_T "unwrapBoolean_T" where

  type CFieldType Boolean_T "unwrapBoolean_T" =
    BOOLEAN_T

  offset# = \_ -> \_ -> 0
