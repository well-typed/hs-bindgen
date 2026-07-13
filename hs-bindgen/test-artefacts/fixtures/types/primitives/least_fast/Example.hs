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
    ( Example.Int_fast16_t(..)
    , Example.Int_fast32_t(..)
    , Example.Uint_fast16_t(..)
    , Example.Uint_fast32_t(..)
    , Example.Int_fast8_t(..)
    , Example.Int_fast64_t(..)
    , Example.Int_least8_t(..)
    , Example.Int_least16_t(..)
    , Example.Int_least32_t(..)
    , Example.Int_least64_t(..)
    , Example.Uint_fast8_t(..)
    , Example.Uint_fast64_t(..)
    , Example.Uint_least8_t(..)
    , Example.Uint_least16_t(..)
    , Example.Uint_least32_t(..)
    , Example.Uint_least64_t(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @int_fast16_t@

    __defined at:__ @bits\/stdint.h 1:17@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
newtype Int_fast16_t = Int_fast16_t
  { unwrapInt_fast16_t :: HsBindgen.Runtime.LibC.Int32
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
         ) => BG.CompatHasField.HasField "unwrapInt_fast16_t" Int_fast16_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Int_fast16_t {unwrapInt_fast16_t = y1}
      , BG.getField @"unwrapInt_fast16_t" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Int32
         ) => BG.HasField "unwrapInt_fast16_t" (BG.Ptr Int_fast16_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapInt_fast16_t")

instance HasCField.HasCField Int_fast16_t "unwrapInt_fast16_t" where

  type CFieldType Int_fast16_t "unwrapInt_fast16_t" =
    HsBindgen.Runtime.LibC.Int32

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int_fast32_t@

    __defined at:__ @bits\/stdint.h 2:17@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
newtype Int_fast32_t = Int_fast32_t
  { unwrapInt_fast32_t :: HsBindgen.Runtime.LibC.Int32
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
         ) => BG.CompatHasField.HasField "unwrapInt_fast32_t" Int_fast32_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Int_fast32_t {unwrapInt_fast32_t = y1}
      , BG.getField @"unwrapInt_fast32_t" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Int32
         ) => BG.HasField "unwrapInt_fast32_t" (BG.Ptr Int_fast32_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapInt_fast32_t")

instance HasCField.HasCField Int_fast32_t "unwrapInt_fast32_t" where

  type CFieldType Int_fast32_t "unwrapInt_fast32_t" =
    HsBindgen.Runtime.LibC.Int32

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @uint_fast16_t@

    __defined at:__ @bits\/stdint.h 3:18@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
newtype Uint_fast16_t = Uint_fast16_t
  { unwrapUint_fast16_t :: HsBindgen.Runtime.LibC.Word32
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
         ) => BG.CompatHasField.HasField "unwrapUint_fast16_t" Uint_fast16_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Uint_fast16_t {unwrapUint_fast16_t = y1}
      , BG.getField @"unwrapUint_fast16_t" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word32
         ) => BG.HasField "unwrapUint_fast16_t" (BG.Ptr Uint_fast16_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapUint_fast16_t")

instance HasCField.HasCField Uint_fast16_t "unwrapUint_fast16_t" where

  type CFieldType Uint_fast16_t "unwrapUint_fast16_t" =
    HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @uint_fast32_t@

    __defined at:__ @bits\/stdint.h 4:18@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
newtype Uint_fast32_t = Uint_fast32_t
  { unwrapUint_fast32_t :: HsBindgen.Runtime.LibC.Word32
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
         ) => BG.CompatHasField.HasField "unwrapUint_fast32_t" Uint_fast32_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Uint_fast32_t {unwrapUint_fast32_t = y1}
      , BG.getField @"unwrapUint_fast32_t" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word32
         ) => BG.HasField "unwrapUint_fast32_t" (BG.Ptr Uint_fast32_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapUint_fast32_t")

instance HasCField.HasCField Uint_fast32_t "unwrapUint_fast32_t" where

  type CFieldType Uint_fast32_t "unwrapUint_fast32_t" =
    HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int_fast8_t@

    __defined at:__ @stdint.h 22:16@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
newtype Int_fast8_t = Int_fast8_t
  { unwrapInt_fast8_t :: HsBindgen.Runtime.LibC.Int8
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
         ) => BG.CompatHasField.HasField "unwrapInt_fast8_t" Int_fast8_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Int_fast8_t {unwrapInt_fast8_t = y1}
      , BG.getField @"unwrapInt_fast8_t" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Int8
         ) => BG.HasField "unwrapInt_fast8_t" (BG.Ptr Int_fast8_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapInt_fast8_t")

instance HasCField.HasCField Int_fast8_t "unwrapInt_fast8_t" where

  type CFieldType Int_fast8_t "unwrapInt_fast8_t" =
    HsBindgen.Runtime.LibC.Int8

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int_fast64_t@

    __defined at:__ @stdint.h 23:17@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
newtype Int_fast64_t = Int_fast64_t
  { unwrapInt_fast64_t :: HsBindgen.Runtime.LibC.Int64
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
         ) => BG.CompatHasField.HasField "unwrapInt_fast64_t" Int_fast64_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Int_fast64_t {unwrapInt_fast64_t = y1}
      , BG.getField @"unwrapInt_fast64_t" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Int64
         ) => BG.HasField "unwrapInt_fast64_t" (BG.Ptr Int_fast64_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapInt_fast64_t")

instance HasCField.HasCField Int_fast64_t "unwrapInt_fast64_t" where

  type CFieldType Int_fast64_t "unwrapInt_fast64_t" =
    HsBindgen.Runtime.LibC.Int64

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int_least8_t@

    __defined at:__ @stdint.h 25:17@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
newtype Int_least8_t = Int_least8_t
  { unwrapInt_least8_t :: HsBindgen.Runtime.LibC.Int8
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
         ) => BG.CompatHasField.HasField "unwrapInt_least8_t" Int_least8_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Int_least8_t {unwrapInt_least8_t = y1}
      , BG.getField @"unwrapInt_least8_t" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Int8
         ) => BG.HasField "unwrapInt_least8_t" (BG.Ptr Int_least8_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapInt_least8_t")

instance HasCField.HasCField Int_least8_t "unwrapInt_least8_t" where

  type CFieldType Int_least8_t "unwrapInt_least8_t" =
    HsBindgen.Runtime.LibC.Int8

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int_least16_t@

    __defined at:__ @stdint.h 26:17@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
newtype Int_least16_t = Int_least16_t
  { unwrapInt_least16_t :: HsBindgen.Runtime.LibC.Int16
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
         ) => BG.CompatHasField.HasField "unwrapInt_least16_t" Int_least16_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Int_least16_t {unwrapInt_least16_t = y1}
      , BG.getField @"unwrapInt_least16_t" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Int16
         ) => BG.HasField "unwrapInt_least16_t" (BG.Ptr Int_least16_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapInt_least16_t")

instance HasCField.HasCField Int_least16_t "unwrapInt_least16_t" where

  type CFieldType Int_least16_t "unwrapInt_least16_t" =
    HsBindgen.Runtime.LibC.Int16

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int_least32_t@

    __defined at:__ @stdint.h 27:17@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
newtype Int_least32_t = Int_least32_t
  { unwrapInt_least32_t :: HsBindgen.Runtime.LibC.Int32
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
         ) => BG.CompatHasField.HasField "unwrapInt_least32_t" Int_least32_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Int_least32_t {unwrapInt_least32_t = y1}
      , BG.getField @"unwrapInt_least32_t" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Int32
         ) => BG.HasField "unwrapInt_least32_t" (BG.Ptr Int_least32_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapInt_least32_t")

instance HasCField.HasCField Int_least32_t "unwrapInt_least32_t" where

  type CFieldType Int_least32_t "unwrapInt_least32_t" =
    HsBindgen.Runtime.LibC.Int32

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int_least64_t@

    __defined at:__ @stdint.h 28:17@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
newtype Int_least64_t = Int_least64_t
  { unwrapInt_least64_t :: HsBindgen.Runtime.LibC.Int64
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
         ) => BG.CompatHasField.HasField "unwrapInt_least64_t" Int_least64_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Int_least64_t {unwrapInt_least64_t = y1}
      , BG.getField @"unwrapInt_least64_t" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Int64
         ) => BG.HasField "unwrapInt_least64_t" (BG.Ptr Int_least64_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapInt_least64_t")

instance HasCField.HasCField Int_least64_t "unwrapInt_least64_t" where

  type CFieldType Int_least64_t "unwrapInt_least64_t" =
    HsBindgen.Runtime.LibC.Int64

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @uint_fast8_t@

    __defined at:__ @stdint.h 30:17@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
newtype Uint_fast8_t = Uint_fast8_t
  { unwrapUint_fast8_t :: HsBindgen.Runtime.LibC.Word8
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
         ) => BG.CompatHasField.HasField "unwrapUint_fast8_t" Uint_fast8_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Uint_fast8_t {unwrapUint_fast8_t = y1}
      , BG.getField @"unwrapUint_fast8_t" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word8
         ) => BG.HasField "unwrapUint_fast8_t" (BG.Ptr Uint_fast8_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapUint_fast8_t")

instance HasCField.HasCField Uint_fast8_t "unwrapUint_fast8_t" where

  type CFieldType Uint_fast8_t "unwrapUint_fast8_t" =
    HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @uint_fast64_t@

    __defined at:__ @stdint.h 31:18@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
newtype Uint_fast64_t = Uint_fast64_t
  { unwrapUint_fast64_t :: HsBindgen.Runtime.LibC.Word64
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
         ) => BG.CompatHasField.HasField "unwrapUint_fast64_t" Uint_fast64_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Uint_fast64_t {unwrapUint_fast64_t = y1}
      , BG.getField @"unwrapUint_fast64_t" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word64
         ) => BG.HasField "unwrapUint_fast64_t" (BG.Ptr Uint_fast64_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapUint_fast64_t")

instance HasCField.HasCField Uint_fast64_t "unwrapUint_fast64_t" where

  type CFieldType Uint_fast64_t "unwrapUint_fast64_t" =
    HsBindgen.Runtime.LibC.Word64

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @uint_least8_t@

    __defined at:__ @stdint.h 33:18@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
newtype Uint_least8_t = Uint_least8_t
  { unwrapUint_least8_t :: HsBindgen.Runtime.LibC.Word8
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
         ) => BG.CompatHasField.HasField "unwrapUint_least8_t" Uint_least8_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Uint_least8_t {unwrapUint_least8_t = y1}
      , BG.getField @"unwrapUint_least8_t" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word8
         ) => BG.HasField "unwrapUint_least8_t" (BG.Ptr Uint_least8_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapUint_least8_t")

instance HasCField.HasCField Uint_least8_t "unwrapUint_least8_t" where

  type CFieldType Uint_least8_t "unwrapUint_least8_t" =
    HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @uint_least16_t@

    __defined at:__ @stdint.h 34:18@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
newtype Uint_least16_t = Uint_least16_t
  { unwrapUint_least16_t :: HsBindgen.Runtime.LibC.Word16
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
         ) => BG.CompatHasField.HasField "unwrapUint_least16_t" Uint_least16_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Uint_least16_t {unwrapUint_least16_t = y1}
      , BG.getField @"unwrapUint_least16_t" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word16
         ) => BG.HasField "unwrapUint_least16_t" (BG.Ptr Uint_least16_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapUint_least16_t")

instance HasCField.HasCField Uint_least16_t "unwrapUint_least16_t" where

  type CFieldType Uint_least16_t "unwrapUint_least16_t" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @uint_least32_t@

    __defined at:__ @stdint.h 35:18@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
newtype Uint_least32_t = Uint_least32_t
  { unwrapUint_least32_t :: HsBindgen.Runtime.LibC.Word32
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
         ) => BG.CompatHasField.HasField "unwrapUint_least32_t" Uint_least32_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Uint_least32_t {unwrapUint_least32_t = y1}
      , BG.getField @"unwrapUint_least32_t" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word32
         ) => BG.HasField "unwrapUint_least32_t" (BG.Ptr Uint_least32_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapUint_least32_t")

instance HasCField.HasCField Uint_least32_t "unwrapUint_least32_t" where

  type CFieldType Uint_least32_t "unwrapUint_least32_t" =
    HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @uint_least64_t@

    __defined at:__ @stdint.h 36:18@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
newtype Uint_least64_t = Uint_least64_t
  { unwrapUint_least64_t :: HsBindgen.Runtime.LibC.Word64
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
         ) => BG.CompatHasField.HasField "unwrapUint_least64_t" Uint_least64_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Uint_least64_t {unwrapUint_least64_t = y1}
      , BG.getField @"unwrapUint_least64_t" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word64
         ) => BG.HasField "unwrapUint_least64_t" (BG.Ptr Uint_least64_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapUint_least64_t")

instance HasCField.HasCField Uint_least64_t "unwrapUint_least64_t" where

  type CFieldType Uint_least64_t "unwrapUint_least64_t" =
    HsBindgen.Runtime.LibC.Word64

  offset# = \_ -> \_ -> 0
