{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude (Bounded, Enum, Eq, Floating, Fractional, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

{-| __C declaration:__ @prim_HsPrimCPtrdiff@

    __defined at:__ @types\/primitives\/primitive_insts.h 13:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCPtrdiff = Prim_HsPrimCPtrdiff
  { unwrapPrim_HsPrimCPtrdiff :: HsBindgen.Runtime.LibC.CPtrdiff
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

instance ( TyEq ty HsBindgen.Runtime.LibC.CPtrdiff
         ) => GHC.Records.HasField "unwrapPrim_HsPrimCPtrdiff" (Ptr.Ptr Prim_HsPrimCPtrdiff) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimCPtrdiff")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimCPtrdiff "unwrapPrim_HsPrimCPtrdiff" where

  type CFieldType Prim_HsPrimCPtrdiff "unwrapPrim_HsPrimCPtrdiff" =
    HsBindgen.Runtime.LibC.CPtrdiff

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimInt8@

    __defined at:__ @types\/primitives\/primitive_insts.h 19:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimInt8 = Prim_HsPrimInt8
  { unwrapPrim_HsPrimInt8 :: HsBindgen.Runtime.LibC.Int8
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

instance ( TyEq ty HsBindgen.Runtime.LibC.Int8
         ) => GHC.Records.HasField "unwrapPrim_HsPrimInt8" (Ptr.Ptr Prim_HsPrimInt8) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimInt8")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimInt8 "unwrapPrim_HsPrimInt8" where

  type CFieldType Prim_HsPrimInt8 "unwrapPrim_HsPrimInt8" =
    HsBindgen.Runtime.LibC.Int8

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimInt16@

    __defined at:__ @types\/primitives\/primitive_insts.h 20:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimInt16 = Prim_HsPrimInt16
  { unwrapPrim_HsPrimInt16 :: HsBindgen.Runtime.LibC.Int16
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

instance ( TyEq ty HsBindgen.Runtime.LibC.Int16
         ) => GHC.Records.HasField "unwrapPrim_HsPrimInt16" (Ptr.Ptr Prim_HsPrimInt16) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimInt16")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimInt16 "unwrapPrim_HsPrimInt16" where

  type CFieldType Prim_HsPrimInt16 "unwrapPrim_HsPrimInt16" =
    HsBindgen.Runtime.LibC.Int16

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimInt32@

    __defined at:__ @types\/primitives\/primitive_insts.h 21:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimInt32 = Prim_HsPrimInt32
  { unwrapPrim_HsPrimInt32 :: HsBindgen.Runtime.LibC.Int32
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

instance ( TyEq ty HsBindgen.Runtime.LibC.Int32
         ) => GHC.Records.HasField "unwrapPrim_HsPrimInt32" (Ptr.Ptr Prim_HsPrimInt32) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimInt32")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimInt32 "unwrapPrim_HsPrimInt32" where

  type CFieldType Prim_HsPrimInt32 "unwrapPrim_HsPrimInt32" =
    HsBindgen.Runtime.LibC.Int32

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimInt64@

    __defined at:__ @types\/primitives\/primitive_insts.h 22:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimInt64 = Prim_HsPrimInt64
  { unwrapPrim_HsPrimInt64 :: HsBindgen.Runtime.LibC.Int64
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

instance ( TyEq ty HsBindgen.Runtime.LibC.Int64
         ) => GHC.Records.HasField "unwrapPrim_HsPrimInt64" (Ptr.Ptr Prim_HsPrimInt64) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimInt64")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimInt64 "unwrapPrim_HsPrimInt64" where

  type CFieldType Prim_HsPrimInt64 "unwrapPrim_HsPrimInt64" =
    HsBindgen.Runtime.LibC.Int64

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimWord8@

    __defined at:__ @types\/primitives\/primitive_insts.h 24:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimWord8 = Prim_HsPrimWord8
  { unwrapPrim_HsPrimWord8 :: HsBindgen.Runtime.LibC.Word8
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

instance ( TyEq ty HsBindgen.Runtime.LibC.Word8
         ) => GHC.Records.HasField "unwrapPrim_HsPrimWord8" (Ptr.Ptr Prim_HsPrimWord8) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimWord8")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimWord8 "unwrapPrim_HsPrimWord8" where

  type CFieldType Prim_HsPrimWord8 "unwrapPrim_HsPrimWord8" =
    HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimWord16@

    __defined at:__ @types\/primitives\/primitive_insts.h 25:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimWord16 = Prim_HsPrimWord16
  { unwrapPrim_HsPrimWord16 :: HsBindgen.Runtime.LibC.Word16
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

instance ( TyEq ty HsBindgen.Runtime.LibC.Word16
         ) => GHC.Records.HasField "unwrapPrim_HsPrimWord16" (Ptr.Ptr Prim_HsPrimWord16) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimWord16")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimWord16 "unwrapPrim_HsPrimWord16" where

  type CFieldType Prim_HsPrimWord16 "unwrapPrim_HsPrimWord16" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimWord32@

    __defined at:__ @types\/primitives\/primitive_insts.h 26:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimWord32 = Prim_HsPrimWord32
  { unwrapPrim_HsPrimWord32 :: HsBindgen.Runtime.LibC.Word32
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

instance ( TyEq ty HsBindgen.Runtime.LibC.Word32
         ) => GHC.Records.HasField "unwrapPrim_HsPrimWord32" (Ptr.Ptr Prim_HsPrimWord32) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimWord32")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimWord32 "unwrapPrim_HsPrimWord32" where

  type CFieldType Prim_HsPrimWord32 "unwrapPrim_HsPrimWord32" =
    HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimWord64@

    __defined at:__ @types\/primitives\/primitive_insts.h 27:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimWord64 = Prim_HsPrimWord64
  { unwrapPrim_HsPrimWord64 :: HsBindgen.Runtime.LibC.Word64
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

instance ( TyEq ty HsBindgen.Runtime.LibC.Word64
         ) => GHC.Records.HasField "unwrapPrim_HsPrimWord64" (Ptr.Ptr Prim_HsPrimWord64) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimWord64")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimWord64 "unwrapPrim_HsPrimWord64" where

  type CFieldType Prim_HsPrimWord64 "unwrapPrim_HsPrimWord64" =
    HsBindgen.Runtime.LibC.Word64

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCChar@

    __defined at:__ @types\/primitives\/primitive_insts.h 28:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCChar = Prim_HsPrimCChar
  { unwrapPrim_HsPrimCChar :: FC.CChar
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

instance ( TyEq ty FC.CChar
         ) => GHC.Records.HasField "unwrapPrim_HsPrimCChar" (Ptr.Ptr Prim_HsPrimCChar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimCChar")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimCChar "unwrapPrim_HsPrimCChar" where

  type CFieldType Prim_HsPrimCChar "unwrapPrim_HsPrimCChar" =
    FC.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCSChar@

    __defined at:__ @types\/primitives\/primitive_insts.h 29:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCSChar = Prim_HsPrimCSChar
  { unwrapPrim_HsPrimCSChar :: FC.CSChar
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

instance ( TyEq ty FC.CSChar
         ) => GHC.Records.HasField "unwrapPrim_HsPrimCSChar" (Ptr.Ptr Prim_HsPrimCSChar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimCSChar")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimCSChar "unwrapPrim_HsPrimCSChar" where

  type CFieldType Prim_HsPrimCSChar "unwrapPrim_HsPrimCSChar" =
    FC.CSChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCUChar@

    __defined at:__ @types\/primitives\/primitive_insts.h 30:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCUChar = Prim_HsPrimCUChar
  { unwrapPrim_HsPrimCUChar :: FC.CUChar
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

instance ( TyEq ty FC.CUChar
         ) => GHC.Records.HasField "unwrapPrim_HsPrimCUChar" (Ptr.Ptr Prim_HsPrimCUChar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimCUChar")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimCUChar "unwrapPrim_HsPrimCUChar" where

  type CFieldType Prim_HsPrimCUChar "unwrapPrim_HsPrimCUChar" =
    FC.CUChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCShort@

    __defined at:__ @types\/primitives\/primitive_insts.h 31:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCShort = Prim_HsPrimCShort
  { unwrapPrim_HsPrimCShort :: FC.CShort
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

instance ( TyEq ty FC.CShort
         ) => GHC.Records.HasField "unwrapPrim_HsPrimCShort" (Ptr.Ptr Prim_HsPrimCShort) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimCShort")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimCShort "unwrapPrim_HsPrimCShort" where

  type CFieldType Prim_HsPrimCShort "unwrapPrim_HsPrimCShort" =
    FC.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCUShort@

    __defined at:__ @types\/primitives\/primitive_insts.h 32:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCUShort = Prim_HsPrimCUShort
  { unwrapPrim_HsPrimCUShort :: FC.CUShort
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

instance ( TyEq ty FC.CUShort
         ) => GHC.Records.HasField "unwrapPrim_HsPrimCUShort" (Ptr.Ptr Prim_HsPrimCUShort) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimCUShort")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimCUShort "unwrapPrim_HsPrimCUShort" where

  type CFieldType Prim_HsPrimCUShort "unwrapPrim_HsPrimCUShort" =
    FC.CUShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCInt@

    __defined at:__ @types\/primitives\/primitive_insts.h 33:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCInt = Prim_HsPrimCInt
  { unwrapPrim_HsPrimCInt :: FC.CInt
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

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "unwrapPrim_HsPrimCInt" (Ptr.Ptr Prim_HsPrimCInt) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimCInt")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimCInt "unwrapPrim_HsPrimCInt" where

  type CFieldType Prim_HsPrimCInt "unwrapPrim_HsPrimCInt" =
    FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCUInt@

    __defined at:__ @types\/primitives\/primitive_insts.h 34:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCUInt = Prim_HsPrimCUInt
  { unwrapPrim_HsPrimCUInt :: FC.CUInt
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

instance ( TyEq ty FC.CUInt
         ) => GHC.Records.HasField "unwrapPrim_HsPrimCUInt" (Ptr.Ptr Prim_HsPrimCUInt) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimCUInt")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimCUInt "unwrapPrim_HsPrimCUInt" where

  type CFieldType Prim_HsPrimCUInt "unwrapPrim_HsPrimCUInt" =
    FC.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCLong@

    __defined at:__ @types\/primitives\/primitive_insts.h 35:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCLong = Prim_HsPrimCLong
  { unwrapPrim_HsPrimCLong :: FC.CLong
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

instance ( TyEq ty FC.CLong
         ) => GHC.Records.HasField "unwrapPrim_HsPrimCLong" (Ptr.Ptr Prim_HsPrimCLong) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimCLong")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimCLong "unwrapPrim_HsPrimCLong" where

  type CFieldType Prim_HsPrimCLong "unwrapPrim_HsPrimCLong" =
    FC.CLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCULong@

    __defined at:__ @types\/primitives\/primitive_insts.h 36:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCULong = Prim_HsPrimCULong
  { unwrapPrim_HsPrimCULong :: FC.CULong
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

instance ( TyEq ty FC.CULong
         ) => GHC.Records.HasField "unwrapPrim_HsPrimCULong" (Ptr.Ptr Prim_HsPrimCULong) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimCULong")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimCULong "unwrapPrim_HsPrimCULong" where

  type CFieldType Prim_HsPrimCULong "unwrapPrim_HsPrimCULong" =
    FC.CULong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCLLong@

    __defined at:__ @types\/primitives\/primitive_insts.h 37:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCLLong = Prim_HsPrimCLLong
  { unwrapPrim_HsPrimCLLong :: FC.CLLong
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

instance ( TyEq ty FC.CLLong
         ) => GHC.Records.HasField "unwrapPrim_HsPrimCLLong" (Ptr.Ptr Prim_HsPrimCLLong) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimCLLong")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimCLLong "unwrapPrim_HsPrimCLLong" where

  type CFieldType Prim_HsPrimCLLong "unwrapPrim_HsPrimCLLong" =
    FC.CLLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCULLong@

    __defined at:__ @types\/primitives\/primitive_insts.h 38:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCULLong = Prim_HsPrimCULLong
  { unwrapPrim_HsPrimCULLong :: FC.CULLong
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

instance ( TyEq ty FC.CULLong
         ) => GHC.Records.HasField "unwrapPrim_HsPrimCULLong" (Ptr.Ptr Prim_HsPrimCULLong) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimCULLong")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimCULLong "unwrapPrim_HsPrimCULLong" where

  type CFieldType Prim_HsPrimCULLong "unwrapPrim_HsPrimCULLong" =
    FC.CULLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCBool@

    __defined at:__ @types\/primitives\/primitive_insts.h 39:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCBool = Prim_HsPrimCBool
  { unwrapPrim_HsPrimCBool :: FC.CBool
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

instance ( TyEq ty FC.CBool
         ) => GHC.Records.HasField "unwrapPrim_HsPrimCBool" (Ptr.Ptr Prim_HsPrimCBool) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimCBool")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimCBool "unwrapPrim_HsPrimCBool" where

  type CFieldType Prim_HsPrimCBool "unwrapPrim_HsPrimCBool" =
    FC.CBool

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCFloat@

    __defined at:__ @types\/primitives\/primitive_insts.h 40:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCFloat = Prim_HsPrimCFloat
  { unwrapPrim_HsPrimCFloat :: FC.CFloat
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

instance ( TyEq ty FC.CFloat
         ) => GHC.Records.HasField "unwrapPrim_HsPrimCFloat" (Ptr.Ptr Prim_HsPrimCFloat) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimCFloat")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimCFloat "unwrapPrim_HsPrimCFloat" where

  type CFieldType Prim_HsPrimCFloat "unwrapPrim_HsPrimCFloat" =
    FC.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCDouble@

    __defined at:__ @types\/primitives\/primitive_insts.h 41:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCDouble = Prim_HsPrimCDouble
  { unwrapPrim_HsPrimCDouble :: FC.CDouble
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

instance ( TyEq ty FC.CDouble
         ) => GHC.Records.HasField "unwrapPrim_HsPrimCDouble" (Ptr.Ptr Prim_HsPrimCDouble) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPrim_HsPrimCDouble")

instance HsBindgen.Runtime.HasCField.HasCField Prim_HsPrimCDouble "unwrapPrim_HsPrimCDouble" where

  type CFieldType Prim_HsPrimCDouble "unwrapPrim_HsPrimCDouble" =
    FC.CDouble

  offset# = \_ -> \_ -> 0
