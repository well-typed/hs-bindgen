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
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @prim_HsPrimCPtrdiff@

    __defined at:__ @types\/primitives\/primitive_insts.h 13:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCPtrdiff = Prim_HsPrimCPtrdiff
  { unwrapPrim_HsPrimCPtrdiff :: HsBindgen.Runtime.LibC.CPtrdiff
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

instance ( ((~) ty) HsBindgen.Runtime.LibC.CPtrdiff
         ) => RIP.HasField "unwrapPrim_HsPrimCPtrdiff" (RIP.Ptr Prim_HsPrimCPtrdiff) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimCPtrdiff")

instance HasCField.HasCField Prim_HsPrimCPtrdiff "unwrapPrim_HsPrimCPtrdiff" where

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

instance ( ((~) ty) HsBindgen.Runtime.LibC.Int8
         ) => RIP.HasField "unwrapPrim_HsPrimInt8" (RIP.Ptr Prim_HsPrimInt8) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimInt8")

instance HasCField.HasCField Prim_HsPrimInt8 "unwrapPrim_HsPrimInt8" where

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

instance ( ((~) ty) HsBindgen.Runtime.LibC.Int16
         ) => RIP.HasField "unwrapPrim_HsPrimInt16" (RIP.Ptr Prim_HsPrimInt16) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimInt16")

instance HasCField.HasCField Prim_HsPrimInt16 "unwrapPrim_HsPrimInt16" where

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

instance ( ((~) ty) HsBindgen.Runtime.LibC.Int32
         ) => RIP.HasField "unwrapPrim_HsPrimInt32" (RIP.Ptr Prim_HsPrimInt32) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimInt32")

instance HasCField.HasCField Prim_HsPrimInt32 "unwrapPrim_HsPrimInt32" where

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

instance ( ((~) ty) HsBindgen.Runtime.LibC.Int64
         ) => RIP.HasField "unwrapPrim_HsPrimInt64" (RIP.Ptr Prim_HsPrimInt64) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimInt64")

instance HasCField.HasCField Prim_HsPrimInt64 "unwrapPrim_HsPrimInt64" where

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

instance ( ((~) ty) HsBindgen.Runtime.LibC.Word8
         ) => RIP.HasField "unwrapPrim_HsPrimWord8" (RIP.Ptr Prim_HsPrimWord8) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimWord8")

instance HasCField.HasCField Prim_HsPrimWord8 "unwrapPrim_HsPrimWord8" where

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

instance ( ((~) ty) HsBindgen.Runtime.LibC.Word16
         ) => RIP.HasField "unwrapPrim_HsPrimWord16" (RIP.Ptr Prim_HsPrimWord16) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimWord16")

instance HasCField.HasCField Prim_HsPrimWord16 "unwrapPrim_HsPrimWord16" where

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

instance ( ((~) ty) HsBindgen.Runtime.LibC.Word32
         ) => RIP.HasField "unwrapPrim_HsPrimWord32" (RIP.Ptr Prim_HsPrimWord32) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimWord32")

instance HasCField.HasCField Prim_HsPrimWord32 "unwrapPrim_HsPrimWord32" where

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

instance ( ((~) ty) HsBindgen.Runtime.LibC.Word64
         ) => RIP.HasField "unwrapPrim_HsPrimWord64" (RIP.Ptr Prim_HsPrimWord64) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimWord64")

instance HasCField.HasCField Prim_HsPrimWord64 "unwrapPrim_HsPrimWord64" where

  type CFieldType Prim_HsPrimWord64 "unwrapPrim_HsPrimWord64" =
    HsBindgen.Runtime.LibC.Word64

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCChar@

    __defined at:__ @types\/primitives\/primitive_insts.h 28:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCChar = Prim_HsPrimCChar
  { unwrapPrim_HsPrimCChar :: RIP.CChar
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

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "unwrapPrim_HsPrimCChar" (RIP.Ptr Prim_HsPrimCChar) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimCChar")

instance HasCField.HasCField Prim_HsPrimCChar "unwrapPrim_HsPrimCChar" where

  type CFieldType Prim_HsPrimCChar "unwrapPrim_HsPrimCChar" =
    RIP.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCSChar@

    __defined at:__ @types\/primitives\/primitive_insts.h 29:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCSChar = Prim_HsPrimCSChar
  { unwrapPrim_HsPrimCSChar :: RIP.CSChar
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

instance ( ((~) ty) RIP.CSChar
         ) => RIP.HasField "unwrapPrim_HsPrimCSChar" (RIP.Ptr Prim_HsPrimCSChar) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimCSChar")

instance HasCField.HasCField Prim_HsPrimCSChar "unwrapPrim_HsPrimCSChar" where

  type CFieldType Prim_HsPrimCSChar "unwrapPrim_HsPrimCSChar" =
    RIP.CSChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCUChar@

    __defined at:__ @types\/primitives\/primitive_insts.h 30:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCUChar = Prim_HsPrimCUChar
  { unwrapPrim_HsPrimCUChar :: RIP.CUChar
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
         ) => RIP.HasField "unwrapPrim_HsPrimCUChar" (RIP.Ptr Prim_HsPrimCUChar) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimCUChar")

instance HasCField.HasCField Prim_HsPrimCUChar "unwrapPrim_HsPrimCUChar" where

  type CFieldType Prim_HsPrimCUChar "unwrapPrim_HsPrimCUChar" =
    RIP.CUChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCShort@

    __defined at:__ @types\/primitives\/primitive_insts.h 31:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCShort = Prim_HsPrimCShort
  { unwrapPrim_HsPrimCShort :: RIP.CShort
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
         ) => RIP.HasField "unwrapPrim_HsPrimCShort" (RIP.Ptr Prim_HsPrimCShort) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimCShort")

instance HasCField.HasCField Prim_HsPrimCShort "unwrapPrim_HsPrimCShort" where

  type CFieldType Prim_HsPrimCShort "unwrapPrim_HsPrimCShort" =
    RIP.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCUShort@

    __defined at:__ @types\/primitives\/primitive_insts.h 32:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCUShort = Prim_HsPrimCUShort
  { unwrapPrim_HsPrimCUShort :: RIP.CUShort
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
         ) => RIP.HasField "unwrapPrim_HsPrimCUShort" (RIP.Ptr Prim_HsPrimCUShort) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimCUShort")

instance HasCField.HasCField Prim_HsPrimCUShort "unwrapPrim_HsPrimCUShort" where

  type CFieldType Prim_HsPrimCUShort "unwrapPrim_HsPrimCUShort" =
    RIP.CUShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCInt@

    __defined at:__ @types\/primitives\/primitive_insts.h 33:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCInt = Prim_HsPrimCInt
  { unwrapPrim_HsPrimCInt :: RIP.CInt
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

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapPrim_HsPrimCInt" (RIP.Ptr Prim_HsPrimCInt) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimCInt")

instance HasCField.HasCField Prim_HsPrimCInt "unwrapPrim_HsPrimCInt" where

  type CFieldType Prim_HsPrimCInt "unwrapPrim_HsPrimCInt" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCUInt@

    __defined at:__ @types\/primitives\/primitive_insts.h 34:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCUInt = Prim_HsPrimCUInt
  { unwrapPrim_HsPrimCUInt :: RIP.CUInt
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

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapPrim_HsPrimCUInt" (RIP.Ptr Prim_HsPrimCUInt) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimCUInt")

instance HasCField.HasCField Prim_HsPrimCUInt "unwrapPrim_HsPrimCUInt" where

  type CFieldType Prim_HsPrimCUInt "unwrapPrim_HsPrimCUInt" =
    RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCLong@

    __defined at:__ @types\/primitives\/primitive_insts.h 35:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCLong = Prim_HsPrimCLong
  { unwrapPrim_HsPrimCLong :: RIP.CLong
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

instance ( ((~) ty) RIP.CLong
         ) => RIP.HasField "unwrapPrim_HsPrimCLong" (RIP.Ptr Prim_HsPrimCLong) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimCLong")

instance HasCField.HasCField Prim_HsPrimCLong "unwrapPrim_HsPrimCLong" where

  type CFieldType Prim_HsPrimCLong "unwrapPrim_HsPrimCLong" =
    RIP.CLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCULong@

    __defined at:__ @types\/primitives\/primitive_insts.h 36:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCULong = Prim_HsPrimCULong
  { unwrapPrim_HsPrimCULong :: RIP.CULong
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

instance ( ((~) ty) RIP.CULong
         ) => RIP.HasField "unwrapPrim_HsPrimCULong" (RIP.Ptr Prim_HsPrimCULong) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimCULong")

instance HasCField.HasCField Prim_HsPrimCULong "unwrapPrim_HsPrimCULong" where

  type CFieldType Prim_HsPrimCULong "unwrapPrim_HsPrimCULong" =
    RIP.CULong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCLLong@

    __defined at:__ @types\/primitives\/primitive_insts.h 37:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCLLong = Prim_HsPrimCLLong
  { unwrapPrim_HsPrimCLLong :: RIP.CLLong
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

instance ( ((~) ty) RIP.CLLong
         ) => RIP.HasField "unwrapPrim_HsPrimCLLong" (RIP.Ptr Prim_HsPrimCLLong) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimCLLong")

instance HasCField.HasCField Prim_HsPrimCLLong "unwrapPrim_HsPrimCLLong" where

  type CFieldType Prim_HsPrimCLLong "unwrapPrim_HsPrimCLLong" =
    RIP.CLLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCULLong@

    __defined at:__ @types\/primitives\/primitive_insts.h 38:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCULLong = Prim_HsPrimCULLong
  { unwrapPrim_HsPrimCULLong :: RIP.CULLong
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

instance ( ((~) ty) RIP.CULLong
         ) => RIP.HasField "unwrapPrim_HsPrimCULLong" (RIP.Ptr Prim_HsPrimCULLong) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimCULLong")

instance HasCField.HasCField Prim_HsPrimCULLong "unwrapPrim_HsPrimCULLong" where

  type CFieldType Prim_HsPrimCULLong "unwrapPrim_HsPrimCULLong" =
    RIP.CULLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCBool@

    __defined at:__ @types\/primitives\/primitive_insts.h 39:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCBool = Prim_HsPrimCBool
  { unwrapPrim_HsPrimCBool :: RIP.CBool
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

instance ( ((~) ty) RIP.CBool
         ) => RIP.HasField "unwrapPrim_HsPrimCBool" (RIP.Ptr Prim_HsPrimCBool) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimCBool")

instance HasCField.HasCField Prim_HsPrimCBool "unwrapPrim_HsPrimCBool" where

  type CFieldType Prim_HsPrimCBool "unwrapPrim_HsPrimCBool" =
    RIP.CBool

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCFloat@

    __defined at:__ @types\/primitives\/primitive_insts.h 40:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCFloat = Prim_HsPrimCFloat
  { unwrapPrim_HsPrimCFloat :: RIP.CFloat
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
         ) => RIP.HasField "unwrapPrim_HsPrimCFloat" (RIP.Ptr Prim_HsPrimCFloat) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimCFloat")

instance HasCField.HasCField Prim_HsPrimCFloat "unwrapPrim_HsPrimCFloat" where

  type CFieldType Prim_HsPrimCFloat "unwrapPrim_HsPrimCFloat" =
    RIP.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCDouble@

    __defined at:__ @types\/primitives\/primitive_insts.h 41:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCDouble = Prim_HsPrimCDouble
  { unwrapPrim_HsPrimCDouble :: RIP.CDouble
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

instance ( ((~) ty) RIP.CDouble
         ) => RIP.HasField "unwrapPrim_HsPrimCDouble" (RIP.Ptr Prim_HsPrimCDouble) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPrim_HsPrimCDouble")

instance HasCField.HasCField Prim_HsPrimCDouble "unwrapPrim_HsPrimCDouble" where

  type CFieldType Prim_HsPrimCDouble "unwrapPrim_HsPrimCDouble" =
    RIP.CDouble

  offset# = \_ -> \_ -> 0
