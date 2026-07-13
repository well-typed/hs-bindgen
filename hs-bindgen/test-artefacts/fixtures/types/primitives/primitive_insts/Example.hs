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
    ( Example.Prim_HsPrimCPtrdiff(..)
    , Example.Prim_HsPrimInt8(..)
    , Example.Prim_HsPrimInt16(..)
    , Example.Prim_HsPrimInt32(..)
    , Example.Prim_HsPrimInt64(..)
    , Example.Prim_HsPrimWord8(..)
    , Example.Prim_HsPrimWord16(..)
    , Example.Prim_HsPrimWord32(..)
    , Example.Prim_HsPrimWord64(..)
    , Example.Prim_HsPrimCChar(..)
    , Example.Prim_HsPrimCSChar(..)
    , Example.Prim_HsPrimCUChar(..)
    , Example.Prim_HsPrimCShort(..)
    , Example.Prim_HsPrimCUShort(..)
    , Example.Prim_HsPrimCInt(..)
    , Example.Prim_HsPrimCUInt(..)
    , Example.Prim_HsPrimCLong(..)
    , Example.Prim_HsPrimCULong(..)
    , Example.Prim_HsPrimCLLong(..)
    , Example.Prim_HsPrimCULLong(..)
    , Example.Prim_HsPrimCBool(..)
    , Example.Prim_HsPrimCFloat(..)
    , Example.Prim_HsPrimCDouble(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @prim_HsPrimCPtrdiff@

    __defined at:__ @types\/primitives\/primitive_insts.h 13:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCPtrdiff = Prim_HsPrimCPtrdiff
  { unwrapPrim_HsPrimCPtrdiff :: HsBindgen.Runtime.LibC.CPtrdiff
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

instance ( ty ~ HsBindgen.Runtime.LibC.CPtrdiff
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimCPtrdiff" Prim_HsPrimCPtrdiff ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimCPtrdiff {unwrapPrim_HsPrimCPtrdiff = y1}
      , BG.getField @"unwrapPrim_HsPrimCPtrdiff" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.CPtrdiff
         ) => BG.HasField "unwrapPrim_HsPrimCPtrdiff" (BG.Ptr Prim_HsPrimCPtrdiff) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimCPtrdiff")

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

instance ( ty ~ HsBindgen.Runtime.LibC.Int8
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimInt8" Prim_HsPrimInt8 ty where

  hasField =
    \x0 ->
      ( \y1 -> Prim_HsPrimInt8 {unwrapPrim_HsPrimInt8 = y1}
      , BG.getField @"unwrapPrim_HsPrimInt8" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Int8
         ) => BG.HasField "unwrapPrim_HsPrimInt8" (BG.Ptr Prim_HsPrimInt8) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimInt8")

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

instance ( ty ~ HsBindgen.Runtime.LibC.Int16
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimInt16" Prim_HsPrimInt16 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimInt16 {unwrapPrim_HsPrimInt16 = y1}
      , BG.getField @"unwrapPrim_HsPrimInt16" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Int16
         ) => BG.HasField "unwrapPrim_HsPrimInt16" (BG.Ptr Prim_HsPrimInt16) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimInt16")

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

instance ( ty ~ HsBindgen.Runtime.LibC.Int32
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimInt32" Prim_HsPrimInt32 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimInt32 {unwrapPrim_HsPrimInt32 = y1}
      , BG.getField @"unwrapPrim_HsPrimInt32" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Int32
         ) => BG.HasField "unwrapPrim_HsPrimInt32" (BG.Ptr Prim_HsPrimInt32) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimInt32")

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

instance ( ty ~ HsBindgen.Runtime.LibC.Int64
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimInt64" Prim_HsPrimInt64 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimInt64 {unwrapPrim_HsPrimInt64 = y1}
      , BG.getField @"unwrapPrim_HsPrimInt64" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Int64
         ) => BG.HasField "unwrapPrim_HsPrimInt64" (BG.Ptr Prim_HsPrimInt64) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimInt64")

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

instance ( ty ~ HsBindgen.Runtime.LibC.Word8
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimWord8" Prim_HsPrimWord8 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimWord8 {unwrapPrim_HsPrimWord8 = y1}
      , BG.getField @"unwrapPrim_HsPrimWord8" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word8
         ) => BG.HasField "unwrapPrim_HsPrimWord8" (BG.Ptr Prim_HsPrimWord8) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimWord8")

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

instance ( ty ~ HsBindgen.Runtime.LibC.Word16
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimWord16" Prim_HsPrimWord16 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimWord16 {unwrapPrim_HsPrimWord16 = y1}
      , BG.getField @"unwrapPrim_HsPrimWord16" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word16
         ) => BG.HasField "unwrapPrim_HsPrimWord16" (BG.Ptr Prim_HsPrimWord16) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimWord16")

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

instance ( ty ~ HsBindgen.Runtime.LibC.Word32
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimWord32" Prim_HsPrimWord32 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimWord32 {unwrapPrim_HsPrimWord32 = y1}
      , BG.getField @"unwrapPrim_HsPrimWord32" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word32
         ) => BG.HasField "unwrapPrim_HsPrimWord32" (BG.Ptr Prim_HsPrimWord32) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimWord32")

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

instance ( ty ~ HsBindgen.Runtime.LibC.Word64
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimWord64" Prim_HsPrimWord64 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimWord64 {unwrapPrim_HsPrimWord64 = y1}
      , BG.getField @"unwrapPrim_HsPrimWord64" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word64
         ) => BG.HasField "unwrapPrim_HsPrimWord64" (BG.Ptr Prim_HsPrimWord64) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimWord64")

instance HasCField.HasCField Prim_HsPrimWord64 "unwrapPrim_HsPrimWord64" where

  type CFieldType Prim_HsPrimWord64 "unwrapPrim_HsPrimWord64" =
    HsBindgen.Runtime.LibC.Word64

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCChar@

    __defined at:__ @types\/primitives\/primitive_insts.h 28:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCChar = Prim_HsPrimCChar
  { unwrapPrim_HsPrimCChar :: BG.CChar
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

instance ( ty ~ BG.CChar
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimCChar" Prim_HsPrimCChar ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimCChar {unwrapPrim_HsPrimCChar = y1}
      , BG.getField @"unwrapPrim_HsPrimCChar" x0
      )

instance ( ty ~ BG.CChar
         ) => BG.HasField "unwrapPrim_HsPrimCChar" (BG.Ptr Prim_HsPrimCChar) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimCChar")

instance HasCField.HasCField Prim_HsPrimCChar "unwrapPrim_HsPrimCChar" where

  type CFieldType Prim_HsPrimCChar "unwrapPrim_HsPrimCChar" =
    BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCSChar@

    __defined at:__ @types\/primitives\/primitive_insts.h 29:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCSChar = Prim_HsPrimCSChar
  { unwrapPrim_HsPrimCSChar :: BG.CSChar
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

instance ( ty ~ BG.CSChar
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimCSChar" Prim_HsPrimCSChar ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimCSChar {unwrapPrim_HsPrimCSChar = y1}
      , BG.getField @"unwrapPrim_HsPrimCSChar" x0
      )

instance ( ty ~ BG.CSChar
         ) => BG.HasField "unwrapPrim_HsPrimCSChar" (BG.Ptr Prim_HsPrimCSChar) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimCSChar")

instance HasCField.HasCField Prim_HsPrimCSChar "unwrapPrim_HsPrimCSChar" where

  type CFieldType Prim_HsPrimCSChar "unwrapPrim_HsPrimCSChar" =
    BG.CSChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCUChar@

    __defined at:__ @types\/primitives\/primitive_insts.h 30:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCUChar = Prim_HsPrimCUChar
  { unwrapPrim_HsPrimCUChar :: BG.CUChar
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
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimCUChar" Prim_HsPrimCUChar ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimCUChar {unwrapPrim_HsPrimCUChar = y1}
      , BG.getField @"unwrapPrim_HsPrimCUChar" x0
      )

instance ( ty ~ BG.CUChar
         ) => BG.HasField "unwrapPrim_HsPrimCUChar" (BG.Ptr Prim_HsPrimCUChar) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimCUChar")

instance HasCField.HasCField Prim_HsPrimCUChar "unwrapPrim_HsPrimCUChar" where

  type CFieldType Prim_HsPrimCUChar "unwrapPrim_HsPrimCUChar" =
    BG.CUChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCShort@

    __defined at:__ @types\/primitives\/primitive_insts.h 31:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCShort = Prim_HsPrimCShort
  { unwrapPrim_HsPrimCShort :: BG.CShort
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
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimCShort" Prim_HsPrimCShort ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimCShort {unwrapPrim_HsPrimCShort = y1}
      , BG.getField @"unwrapPrim_HsPrimCShort" x0
      )

instance ( ty ~ BG.CShort
         ) => BG.HasField "unwrapPrim_HsPrimCShort" (BG.Ptr Prim_HsPrimCShort) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimCShort")

instance HasCField.HasCField Prim_HsPrimCShort "unwrapPrim_HsPrimCShort" where

  type CFieldType Prim_HsPrimCShort "unwrapPrim_HsPrimCShort" =
    BG.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCUShort@

    __defined at:__ @types\/primitives\/primitive_insts.h 32:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCUShort = Prim_HsPrimCUShort
  { unwrapPrim_HsPrimCUShort :: BG.CUShort
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
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimCUShort" Prim_HsPrimCUShort ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimCUShort {unwrapPrim_HsPrimCUShort = y1}
      , BG.getField @"unwrapPrim_HsPrimCUShort" x0
      )

instance ( ty ~ BG.CUShort
         ) => BG.HasField "unwrapPrim_HsPrimCUShort" (BG.Ptr Prim_HsPrimCUShort) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimCUShort")

instance HasCField.HasCField Prim_HsPrimCUShort "unwrapPrim_HsPrimCUShort" where

  type CFieldType Prim_HsPrimCUShort "unwrapPrim_HsPrimCUShort" =
    BG.CUShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCInt@

    __defined at:__ @types\/primitives\/primitive_insts.h 33:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCInt = Prim_HsPrimCInt
  { unwrapPrim_HsPrimCInt :: BG.CInt
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

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimCInt" Prim_HsPrimCInt ty where

  hasField =
    \x0 ->
      ( \y1 -> Prim_HsPrimCInt {unwrapPrim_HsPrimCInt = y1}
      , BG.getField @"unwrapPrim_HsPrimCInt" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapPrim_HsPrimCInt" (BG.Ptr Prim_HsPrimCInt) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimCInt")

instance HasCField.HasCField Prim_HsPrimCInt "unwrapPrim_HsPrimCInt" where

  type CFieldType Prim_HsPrimCInt "unwrapPrim_HsPrimCInt" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCUInt@

    __defined at:__ @types\/primitives\/primitive_insts.h 34:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCUInt = Prim_HsPrimCUInt
  { unwrapPrim_HsPrimCUInt :: BG.CUInt
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

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimCUInt" Prim_HsPrimCUInt ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimCUInt {unwrapPrim_HsPrimCUInt = y1}
      , BG.getField @"unwrapPrim_HsPrimCUInt" x0
      )

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapPrim_HsPrimCUInt" (BG.Ptr Prim_HsPrimCUInt) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimCUInt")

instance HasCField.HasCField Prim_HsPrimCUInt "unwrapPrim_HsPrimCUInt" where

  type CFieldType Prim_HsPrimCUInt "unwrapPrim_HsPrimCUInt" =
    BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCLong@

    __defined at:__ @types\/primitives\/primitive_insts.h 35:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCLong = Prim_HsPrimCLong
  { unwrapPrim_HsPrimCLong :: BG.CLong
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

instance ( ty ~ BG.CLong
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimCLong" Prim_HsPrimCLong ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimCLong {unwrapPrim_HsPrimCLong = y1}
      , BG.getField @"unwrapPrim_HsPrimCLong" x0
      )

instance ( ty ~ BG.CLong
         ) => BG.HasField "unwrapPrim_HsPrimCLong" (BG.Ptr Prim_HsPrimCLong) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimCLong")

instance HasCField.HasCField Prim_HsPrimCLong "unwrapPrim_HsPrimCLong" where

  type CFieldType Prim_HsPrimCLong "unwrapPrim_HsPrimCLong" =
    BG.CLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCULong@

    __defined at:__ @types\/primitives\/primitive_insts.h 36:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCULong = Prim_HsPrimCULong
  { unwrapPrim_HsPrimCULong :: BG.CULong
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

instance ( ty ~ BG.CULong
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimCULong" Prim_HsPrimCULong ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimCULong {unwrapPrim_HsPrimCULong = y1}
      , BG.getField @"unwrapPrim_HsPrimCULong" x0
      )

instance ( ty ~ BG.CULong
         ) => BG.HasField "unwrapPrim_HsPrimCULong" (BG.Ptr Prim_HsPrimCULong) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimCULong")

instance HasCField.HasCField Prim_HsPrimCULong "unwrapPrim_HsPrimCULong" where

  type CFieldType Prim_HsPrimCULong "unwrapPrim_HsPrimCULong" =
    BG.CULong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCLLong@

    __defined at:__ @types\/primitives\/primitive_insts.h 37:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCLLong = Prim_HsPrimCLLong
  { unwrapPrim_HsPrimCLLong :: BG.CLLong
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

instance ( ty ~ BG.CLLong
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimCLLong" Prim_HsPrimCLLong ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimCLLong {unwrapPrim_HsPrimCLLong = y1}
      , BG.getField @"unwrapPrim_HsPrimCLLong" x0
      )

instance ( ty ~ BG.CLLong
         ) => BG.HasField "unwrapPrim_HsPrimCLLong" (BG.Ptr Prim_HsPrimCLLong) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimCLLong")

instance HasCField.HasCField Prim_HsPrimCLLong "unwrapPrim_HsPrimCLLong" where

  type CFieldType Prim_HsPrimCLLong "unwrapPrim_HsPrimCLLong" =
    BG.CLLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCULLong@

    __defined at:__ @types\/primitives\/primitive_insts.h 38:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCULLong = Prim_HsPrimCULLong
  { unwrapPrim_HsPrimCULLong :: BG.CULLong
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

instance ( ty ~ BG.CULLong
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimCULLong" Prim_HsPrimCULLong ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimCULLong {unwrapPrim_HsPrimCULLong = y1}
      , BG.getField @"unwrapPrim_HsPrimCULLong" x0
      )

instance ( ty ~ BG.CULLong
         ) => BG.HasField "unwrapPrim_HsPrimCULLong" (BG.Ptr Prim_HsPrimCULLong) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimCULLong")

instance HasCField.HasCField Prim_HsPrimCULLong "unwrapPrim_HsPrimCULLong" where

  type CFieldType Prim_HsPrimCULLong "unwrapPrim_HsPrimCULLong" =
    BG.CULLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCBool@

    __defined at:__ @types\/primitives\/primitive_insts.h 39:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCBool = Prim_HsPrimCBool
  { unwrapPrim_HsPrimCBool :: BG.CBool
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

instance ( ty ~ BG.CBool
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimCBool" Prim_HsPrimCBool ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimCBool {unwrapPrim_HsPrimCBool = y1}
      , BG.getField @"unwrapPrim_HsPrimCBool" x0
      )

instance ( ty ~ BG.CBool
         ) => BG.HasField "unwrapPrim_HsPrimCBool" (BG.Ptr Prim_HsPrimCBool) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimCBool")

instance HasCField.HasCField Prim_HsPrimCBool "unwrapPrim_HsPrimCBool" where

  type CFieldType Prim_HsPrimCBool "unwrapPrim_HsPrimCBool" =
    BG.CBool

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCFloat@

    __defined at:__ @types\/primitives\/primitive_insts.h 40:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCFloat = Prim_HsPrimCFloat
  { unwrapPrim_HsPrimCFloat :: BG.CFloat
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

instance ( ty ~ BG.CFloat
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimCFloat" Prim_HsPrimCFloat ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimCFloat {unwrapPrim_HsPrimCFloat = y1}
      , BG.getField @"unwrapPrim_HsPrimCFloat" x0
      )

instance ( ty ~ BG.CFloat
         ) => BG.HasField "unwrapPrim_HsPrimCFloat" (BG.Ptr Prim_HsPrimCFloat) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimCFloat")

instance HasCField.HasCField Prim_HsPrimCFloat "unwrapPrim_HsPrimCFloat" where

  type CFieldType Prim_HsPrimCFloat "unwrapPrim_HsPrimCFloat" =
    BG.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @prim_HsPrimCDouble@

    __defined at:__ @types\/primitives\/primitive_insts.h 41:28@

    __exported by:__ @types\/primitives\/primitive_insts.h@
-}
newtype Prim_HsPrimCDouble = Prim_HsPrimCDouble
  { unwrapPrim_HsPrimCDouble :: BG.CDouble
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

instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "unwrapPrim_HsPrimCDouble" Prim_HsPrimCDouble ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Prim_HsPrimCDouble {unwrapPrim_HsPrimCDouble = y1}
      , BG.getField @"unwrapPrim_HsPrimCDouble" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "unwrapPrim_HsPrimCDouble" (BG.Ptr Prim_HsPrimCDouble) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPrim_HsPrimCDouble")

instance HasCField.HasCField Prim_HsPrimCDouble "unwrapPrim_HsPrimCDouble" where

  type CFieldType Prim_HsPrimCDouble "unwrapPrim_HsPrimCDouble" =
    BG.CDouble

  offset# = \_ -> \_ -> 0
