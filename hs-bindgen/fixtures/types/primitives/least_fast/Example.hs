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
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

{-| __C declaration:__ @int_fast16_t@

    __defined at:__ @bits\/stdint.h 1:17@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
newtype Int_fast16_t = Int_fast16_t
  { unwrapInt_fast16_t :: HsBindgen.Runtime.LibC.Int32
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
         ) => GHC.Records.HasField "unwrapInt_fast16_t" (Ptr.Ptr Int_fast16_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapInt_fast16_t")

instance HsBindgen.Runtime.HasCField.HasCField Int_fast16_t "unwrapInt_fast16_t" where

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
         ) => GHC.Records.HasField "unwrapInt_fast32_t" (Ptr.Ptr Int_fast32_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapInt_fast32_t")

instance HsBindgen.Runtime.HasCField.HasCField Int_fast32_t "unwrapInt_fast32_t" where

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
         ) => GHC.Records.HasField "unwrapUint_fast16_t" (Ptr.Ptr Uint_fast16_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapUint_fast16_t")

instance HsBindgen.Runtime.HasCField.HasCField Uint_fast16_t "unwrapUint_fast16_t" where

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
         ) => GHC.Records.HasField "unwrapUint_fast32_t" (Ptr.Ptr Uint_fast32_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapUint_fast32_t")

instance HsBindgen.Runtime.HasCField.HasCField Uint_fast32_t "unwrapUint_fast32_t" where

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
         ) => GHC.Records.HasField "unwrapInt_fast8_t" (Ptr.Ptr Int_fast8_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapInt_fast8_t")

instance HsBindgen.Runtime.HasCField.HasCField Int_fast8_t "unwrapInt_fast8_t" where

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
         ) => GHC.Records.HasField "unwrapInt_fast64_t" (Ptr.Ptr Int_fast64_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapInt_fast64_t")

instance HsBindgen.Runtime.HasCField.HasCField Int_fast64_t "unwrapInt_fast64_t" where

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
         ) => GHC.Records.HasField "unwrapInt_least8_t" (Ptr.Ptr Int_least8_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapInt_least8_t")

instance HsBindgen.Runtime.HasCField.HasCField Int_least8_t "unwrapInt_least8_t" where

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
         ) => GHC.Records.HasField "unwrapInt_least16_t" (Ptr.Ptr Int_least16_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapInt_least16_t")

instance HsBindgen.Runtime.HasCField.HasCField Int_least16_t "unwrapInt_least16_t" where

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
         ) => GHC.Records.HasField "unwrapInt_least32_t" (Ptr.Ptr Int_least32_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapInt_least32_t")

instance HsBindgen.Runtime.HasCField.HasCField Int_least32_t "unwrapInt_least32_t" where

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
         ) => GHC.Records.HasField "unwrapInt_least64_t" (Ptr.Ptr Int_least64_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapInt_least64_t")

instance HsBindgen.Runtime.HasCField.HasCField Int_least64_t "unwrapInt_least64_t" where

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
         ) => GHC.Records.HasField "unwrapUint_fast8_t" (Ptr.Ptr Uint_fast8_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapUint_fast8_t")

instance HsBindgen.Runtime.HasCField.HasCField Uint_fast8_t "unwrapUint_fast8_t" where

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
         ) => GHC.Records.HasField "unwrapUint_fast64_t" (Ptr.Ptr Uint_fast64_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapUint_fast64_t")

instance HsBindgen.Runtime.HasCField.HasCField Uint_fast64_t "unwrapUint_fast64_t" where

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
         ) => GHC.Records.HasField "unwrapUint_least8_t" (Ptr.Ptr Uint_least8_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapUint_least8_t")

instance HsBindgen.Runtime.HasCField.HasCField Uint_least8_t "unwrapUint_least8_t" where

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
         ) => GHC.Records.HasField "unwrapUint_least16_t" (Ptr.Ptr Uint_least16_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapUint_least16_t")

instance HsBindgen.Runtime.HasCField.HasCField Uint_least16_t "unwrapUint_least16_t" where

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
         ) => GHC.Records.HasField "unwrapUint_least32_t" (Ptr.Ptr Uint_least32_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapUint_least32_t")

instance HsBindgen.Runtime.HasCField.HasCField Uint_least32_t "unwrapUint_least32_t" where

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
         ) => GHC.Records.HasField "unwrapUint_least64_t" (Ptr.Ptr Uint_least64_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapUint_least64_t")

instance HsBindgen.Runtime.HasCField.HasCField Uint_least64_t "unwrapUint_least64_t" where

  type CFieldType Uint_least64_t "unwrapUint_least64_t" =
    HsBindgen.Runtime.LibC.Word64

  offset# = \_ -> \_ -> 0
