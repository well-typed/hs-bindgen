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
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.Bitfield
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal
import Data.Bits (FiniteBits)
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

{-| __C declaration:__ @stdlib_CBool@

    __defined at:__ @binding-specs\/stdlib\/instances.h 18:14@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CBool = Stdlib_CBool
  { unwrapStdlib_CBool :: HsBindgen.Runtime.LibC.CBool
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

instance GHC.Records.HasField "unwrapStdlib_CBool" (Ptr.Ptr Stdlib_CBool) (Ptr.Ptr HsBindgen.Runtime.LibC.CBool) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CBool")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CBool "unwrapStdlib_CBool" where

  type CFieldType Stdlib_CBool "unwrapStdlib_CBool" =
    HsBindgen.Runtime.LibC.CBool

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_Int8@

    __defined at:__ @binding-specs\/stdlib\/instances.h 21:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_Int8 = Stdlib_Int8
  { unwrapStdlib_Int8 :: HsBindgen.Runtime.LibC.Int8
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

instance GHC.Records.HasField "unwrapStdlib_Int8" (Ptr.Ptr Stdlib_Int8) (Ptr.Ptr HsBindgen.Runtime.LibC.Int8) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_Int8")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_Int8 "unwrapStdlib_Int8" where

  type CFieldType Stdlib_Int8 "unwrapStdlib_Int8" =
    HsBindgen.Runtime.LibC.Int8

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_Int16@

    __defined at:__ @binding-specs\/stdlib\/instances.h 22:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_Int16 = Stdlib_Int16
  { unwrapStdlib_Int16 :: HsBindgen.Runtime.LibC.Int16
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

instance GHC.Records.HasField "unwrapStdlib_Int16" (Ptr.Ptr Stdlib_Int16) (Ptr.Ptr HsBindgen.Runtime.LibC.Int16) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_Int16")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_Int16 "unwrapStdlib_Int16" where

  type CFieldType Stdlib_Int16 "unwrapStdlib_Int16" =
    HsBindgen.Runtime.LibC.Int16

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_Int32@

    __defined at:__ @binding-specs\/stdlib\/instances.h 23:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_Int32 = Stdlib_Int32
  { unwrapStdlib_Int32 :: HsBindgen.Runtime.LibC.Int32
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

instance GHC.Records.HasField "unwrapStdlib_Int32" (Ptr.Ptr Stdlib_Int32) (Ptr.Ptr HsBindgen.Runtime.LibC.Int32) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_Int32")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_Int32 "unwrapStdlib_Int32" where

  type CFieldType Stdlib_Int32 "unwrapStdlib_Int32" =
    HsBindgen.Runtime.LibC.Int32

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_Int64@

    __defined at:__ @binding-specs\/stdlib\/instances.h 24:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_Int64 = Stdlib_Int64
  { unwrapStdlib_Int64 :: HsBindgen.Runtime.LibC.Int64
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

instance GHC.Records.HasField "unwrapStdlib_Int64" (Ptr.Ptr Stdlib_Int64) (Ptr.Ptr HsBindgen.Runtime.LibC.Int64) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_Int64")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_Int64 "unwrapStdlib_Int64" where

  type CFieldType Stdlib_Int64 "unwrapStdlib_Int64" =
    HsBindgen.Runtime.LibC.Int64

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_Word8@

    __defined at:__ @binding-specs\/stdlib\/instances.h 25:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_Word8 = Stdlib_Word8
  { unwrapStdlib_Word8 :: HsBindgen.Runtime.LibC.Word8
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

instance GHC.Records.HasField "unwrapStdlib_Word8" (Ptr.Ptr Stdlib_Word8) (Ptr.Ptr HsBindgen.Runtime.LibC.Word8) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_Word8")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_Word8 "unwrapStdlib_Word8" where

  type CFieldType Stdlib_Word8 "unwrapStdlib_Word8" =
    HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_Word16@

    __defined at:__ @binding-specs\/stdlib\/instances.h 26:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_Word16 = Stdlib_Word16
  { unwrapStdlib_Word16 :: HsBindgen.Runtime.LibC.Word16
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

instance GHC.Records.HasField "unwrapStdlib_Word16" (Ptr.Ptr Stdlib_Word16) (Ptr.Ptr HsBindgen.Runtime.LibC.Word16) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_Word16")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_Word16 "unwrapStdlib_Word16" where

  type CFieldType Stdlib_Word16 "unwrapStdlib_Word16" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_Word32@

    __defined at:__ @binding-specs\/stdlib\/instances.h 27:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_Word32 = Stdlib_Word32
  { unwrapStdlib_Word32 :: HsBindgen.Runtime.LibC.Word32
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

instance GHC.Records.HasField "unwrapStdlib_Word32" (Ptr.Ptr Stdlib_Word32) (Ptr.Ptr HsBindgen.Runtime.LibC.Word32) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_Word32")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_Word32 "unwrapStdlib_Word32" where

  type CFieldType Stdlib_Word32 "unwrapStdlib_Word32" =
    HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_Word64@

    __defined at:__ @binding-specs\/stdlib\/instances.h 28:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_Word64 = Stdlib_Word64
  { unwrapStdlib_Word64 :: HsBindgen.Runtime.LibC.Word64
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

instance GHC.Records.HasField "unwrapStdlib_Word64" (Ptr.Ptr Stdlib_Word64) (Ptr.Ptr HsBindgen.Runtime.LibC.Word64) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_Word64")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_Word64 "unwrapStdlib_Word64" where

  type CFieldType Stdlib_Word64 "unwrapStdlib_Word64" =
    HsBindgen.Runtime.LibC.Word64

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CIntMax@

    __defined at:__ @binding-specs\/stdlib\/instances.h 29:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CIntMax = Stdlib_CIntMax
  { unwrapStdlib_CIntMax :: HsBindgen.Runtime.LibC.CIntMax
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

instance GHC.Records.HasField "unwrapStdlib_CIntMax" (Ptr.Ptr Stdlib_CIntMax) (Ptr.Ptr HsBindgen.Runtime.LibC.CIntMax) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CIntMax")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CIntMax "unwrapStdlib_CIntMax" where

  type CFieldType Stdlib_CIntMax "unwrapStdlib_CIntMax" =
    HsBindgen.Runtime.LibC.CIntMax

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CUIntMax@

    __defined at:__ @binding-specs\/stdlib\/instances.h 30:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CUIntMax = Stdlib_CUIntMax
  { unwrapStdlib_CUIntMax :: HsBindgen.Runtime.LibC.CUIntMax
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

instance GHC.Records.HasField "unwrapStdlib_CUIntMax" (Ptr.Ptr Stdlib_CUIntMax) (Ptr.Ptr HsBindgen.Runtime.LibC.CUIntMax) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CUIntMax")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CUIntMax "unwrapStdlib_CUIntMax" where

  type CFieldType Stdlib_CUIntMax "unwrapStdlib_CUIntMax" =
    HsBindgen.Runtime.LibC.CUIntMax

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CIntPtr@

    __defined at:__ @binding-specs\/stdlib\/instances.h 31:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CIntPtr = Stdlib_CIntPtr
  { unwrapStdlib_CIntPtr :: HsBindgen.Runtime.LibC.CIntPtr
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

instance GHC.Records.HasField "unwrapStdlib_CIntPtr" (Ptr.Ptr Stdlib_CIntPtr) (Ptr.Ptr HsBindgen.Runtime.LibC.CIntPtr) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CIntPtr")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CIntPtr "unwrapStdlib_CIntPtr" where

  type CFieldType Stdlib_CIntPtr "unwrapStdlib_CIntPtr" =
    HsBindgen.Runtime.LibC.CIntPtr

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CUIntPtr@

    __defined at:__ @binding-specs\/stdlib\/instances.h 32:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CUIntPtr = Stdlib_CUIntPtr
  { unwrapStdlib_CUIntPtr :: HsBindgen.Runtime.LibC.CUIntPtr
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

instance GHC.Records.HasField "unwrapStdlib_CUIntPtr" (Ptr.Ptr Stdlib_CUIntPtr) (Ptr.Ptr HsBindgen.Runtime.LibC.CUIntPtr) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CUIntPtr")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CUIntPtr "unwrapStdlib_CUIntPtr" where

  type CFieldType Stdlib_CUIntPtr "unwrapStdlib_CUIntPtr" =
    HsBindgen.Runtime.LibC.CUIntPtr

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CFenvT@

    __defined at:__ @binding-specs\/stdlib\/instances.h 35:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CFenvT = Stdlib_CFenvT
  { unwrapStdlib_CFenvT :: HsBindgen.Runtime.LibC.CFenvT
  }
  deriving stock (GHC.Generics.Generic)

instance GHC.Records.HasField "unwrapStdlib_CFenvT" (Ptr.Ptr Stdlib_CFenvT) (Ptr.Ptr HsBindgen.Runtime.LibC.CFenvT) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CFenvT")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CFenvT "unwrapStdlib_CFenvT" where

  type CFieldType Stdlib_CFenvT "unwrapStdlib_CFenvT" =
    HsBindgen.Runtime.LibC.CFenvT

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CFexceptT@

    __defined at:__ @binding-specs\/stdlib\/instances.h 36:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CFexceptT = Stdlib_CFexceptT
  { unwrapStdlib_CFexceptT :: HsBindgen.Runtime.LibC.CFexceptT
  }
  deriving stock (GHC.Generics.Generic)

instance GHC.Records.HasField "unwrapStdlib_CFexceptT" (Ptr.Ptr Stdlib_CFexceptT) (Ptr.Ptr HsBindgen.Runtime.LibC.CFexceptT) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CFexceptT")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CFexceptT "unwrapStdlib_CFexceptT" where

  type CFieldType Stdlib_CFexceptT "unwrapStdlib_CFexceptT" =
    HsBindgen.Runtime.LibC.CFexceptT

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CSize@

    __defined at:__ @binding-specs\/stdlib\/instances.h 39:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CSize = Stdlib_CSize
  { unwrapStdlib_CSize :: HsBindgen.Runtime.LibC.CSize
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

instance GHC.Records.HasField "unwrapStdlib_CSize" (Ptr.Ptr Stdlib_CSize) (Ptr.Ptr HsBindgen.Runtime.LibC.CSize) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CSize")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CSize "unwrapStdlib_CSize" where

  type CFieldType Stdlib_CSize "unwrapStdlib_CSize" =
    HsBindgen.Runtime.LibC.CSize

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CPtrdiff@

    __defined at:__ @binding-specs\/stdlib\/instances.h 40:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CPtrdiff = Stdlib_CPtrdiff
  { unwrapStdlib_CPtrdiff :: HsBindgen.Runtime.LibC.CPtrdiff
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

instance GHC.Records.HasField "unwrapStdlib_CPtrdiff" (Ptr.Ptr Stdlib_CPtrdiff) (Ptr.Ptr HsBindgen.Runtime.LibC.CPtrdiff) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CPtrdiff")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CPtrdiff "unwrapStdlib_CPtrdiff" where

  type CFieldType Stdlib_CPtrdiff "unwrapStdlib_CPtrdiff" =
    HsBindgen.Runtime.LibC.CPtrdiff

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CJmpBuf@

    __defined at:__ @binding-specs\/stdlib\/instances.h 43:17@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CJmpBuf = Stdlib_CJmpBuf
  { unwrapStdlib_CJmpBuf :: HsBindgen.Runtime.LibC.CJmpBuf
  }
  deriving stock (GHC.Generics.Generic)

instance GHC.Records.HasField "unwrapStdlib_CJmpBuf" (Ptr.Ptr Stdlib_CJmpBuf) (Ptr.Ptr HsBindgen.Runtime.LibC.CJmpBuf) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CJmpBuf")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CJmpBuf "unwrapStdlib_CJmpBuf" where

  type CFieldType Stdlib_CJmpBuf "unwrapStdlib_CJmpBuf" =
    HsBindgen.Runtime.LibC.CJmpBuf

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CWchar@

    __defined at:__ @binding-specs\/stdlib\/instances.h 46:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CWchar = Stdlib_CWchar
  { unwrapStdlib_CWchar :: HsBindgen.Runtime.LibC.CWchar
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

instance GHC.Records.HasField "unwrapStdlib_CWchar" (Ptr.Ptr Stdlib_CWchar) (Ptr.Ptr HsBindgen.Runtime.LibC.CWchar) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CWchar")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CWchar "unwrapStdlib_CWchar" where

  type CFieldType Stdlib_CWchar "unwrapStdlib_CWchar" =
    HsBindgen.Runtime.LibC.CWchar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CWintT@

    __defined at:__ @binding-specs\/stdlib\/instances.h 47:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CWintT = Stdlib_CWintT
  { unwrapStdlib_CWintT :: HsBindgen.Runtime.LibC.CWintT
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

instance GHC.Records.HasField "unwrapStdlib_CWintT" (Ptr.Ptr Stdlib_CWintT) (Ptr.Ptr HsBindgen.Runtime.LibC.CWintT) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CWintT")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CWintT "unwrapStdlib_CWintT" where

  type CFieldType Stdlib_CWintT "unwrapStdlib_CWintT" =
    HsBindgen.Runtime.LibC.CWintT

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CMbstateT@

    __defined at:__ @binding-specs\/stdlib\/instances.h 48:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CMbstateT = Stdlib_CMbstateT
  { unwrapStdlib_CMbstateT :: HsBindgen.Runtime.LibC.CMbstateT
  }
  deriving stock (GHC.Generics.Generic)

instance GHC.Records.HasField "unwrapStdlib_CMbstateT" (Ptr.Ptr Stdlib_CMbstateT) (Ptr.Ptr HsBindgen.Runtime.LibC.CMbstateT) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CMbstateT")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CMbstateT "unwrapStdlib_CMbstateT" where

  type CFieldType Stdlib_CMbstateT "unwrapStdlib_CMbstateT" =
    HsBindgen.Runtime.LibC.CMbstateT

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CWctransT@

    __defined at:__ @binding-specs\/stdlib\/instances.h 49:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CWctransT = Stdlib_CWctransT
  { unwrapStdlib_CWctransT :: HsBindgen.Runtime.LibC.CWctransT
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    )

instance GHC.Records.HasField "unwrapStdlib_CWctransT" (Ptr.Ptr Stdlib_CWctransT) (Ptr.Ptr HsBindgen.Runtime.LibC.CWctransT) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CWctransT")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CWctransT "unwrapStdlib_CWctransT" where

  type CFieldType Stdlib_CWctransT "unwrapStdlib_CWctransT" =
    HsBindgen.Runtime.LibC.CWctransT

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CWctypeT@

    __defined at:__ @binding-specs\/stdlib\/instances.h 50:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CWctypeT = Stdlib_CWctypeT
  { unwrapStdlib_CWctypeT :: HsBindgen.Runtime.LibC.CWctypeT
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    )

instance GHC.Records.HasField "unwrapStdlib_CWctypeT" (Ptr.Ptr Stdlib_CWctypeT) (Ptr.Ptr HsBindgen.Runtime.LibC.CWctypeT) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CWctypeT")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CWctypeT "unwrapStdlib_CWctypeT" where

  type CFieldType Stdlib_CWctypeT "unwrapStdlib_CWctypeT" =
    HsBindgen.Runtime.LibC.CWctypeT

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CChar16T@

    __defined at:__ @binding-specs\/stdlib\/instances.h 51:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CChar16T = Stdlib_CChar16T
  { unwrapStdlib_CChar16T :: HsBindgen.Runtime.LibC.CChar16T
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

instance GHC.Records.HasField "unwrapStdlib_CChar16T" (Ptr.Ptr Stdlib_CChar16T) (Ptr.Ptr HsBindgen.Runtime.LibC.CChar16T) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CChar16T")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CChar16T "unwrapStdlib_CChar16T" where

  type CFieldType Stdlib_CChar16T "unwrapStdlib_CChar16T" =
    HsBindgen.Runtime.LibC.CChar16T

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CChar32T@

    __defined at:__ @binding-specs\/stdlib\/instances.h 52:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CChar32T = Stdlib_CChar32T
  { unwrapStdlib_CChar32T :: HsBindgen.Runtime.LibC.CChar32T
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

instance GHC.Records.HasField "unwrapStdlib_CChar32T" (Ptr.Ptr Stdlib_CChar32T) (Ptr.Ptr HsBindgen.Runtime.LibC.CChar32T) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CChar32T")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CChar32T "unwrapStdlib_CChar32T" where

  type CFieldType Stdlib_CChar32T "unwrapStdlib_CChar32T" =
    HsBindgen.Runtime.LibC.CChar32T

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CTime@

    __defined at:__ @binding-specs\/stdlib\/instances.h 55:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CTime = Stdlib_CTime
  { unwrapStdlib_CTime :: HsBindgen.Runtime.LibC.CTime
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Enum
    , Num
    , Real
    )

instance GHC.Records.HasField "unwrapStdlib_CTime" (Ptr.Ptr Stdlib_CTime) (Ptr.Ptr HsBindgen.Runtime.LibC.CTime) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CTime")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CTime "unwrapStdlib_CTime" where

  type CFieldType Stdlib_CTime "unwrapStdlib_CTime" =
    HsBindgen.Runtime.LibC.CTime

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CClock@

    __defined at:__ @binding-specs\/stdlib\/instances.h 56:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CClock = Stdlib_CClock
  { unwrapStdlib_CClock :: HsBindgen.Runtime.LibC.CClock
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Enum
    , Num
    , Real
    )

instance GHC.Records.HasField "unwrapStdlib_CClock" (Ptr.Ptr Stdlib_CClock) (Ptr.Ptr HsBindgen.Runtime.LibC.CClock) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CClock")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CClock "unwrapStdlib_CClock" where

  type CFieldType Stdlib_CClock "unwrapStdlib_CClock" =
    HsBindgen.Runtime.LibC.CClock

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CTm@

    __defined at:__ @binding-specs\/stdlib\/instances.h 57:19@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CTm = Stdlib_CTm
  { unwrapStdlib_CTm :: HsBindgen.Runtime.LibC.CTm
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    )

instance GHC.Records.HasField "unwrapStdlib_CTm" (Ptr.Ptr Stdlib_CTm) (Ptr.Ptr HsBindgen.Runtime.LibC.CTm) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CTm")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CTm "unwrapStdlib_CTm" where

  type CFieldType Stdlib_CTm "unwrapStdlib_CTm" =
    HsBindgen.Runtime.LibC.CTm

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CFile@

    __defined at:__ @binding-specs\/stdlib\/instances.h 60:16@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CFile = Stdlib_CFile
  { unwrapStdlib_CFile :: HsBindgen.Runtime.LibC.CFile
  }
  deriving stock (GHC.Generics.Generic)

instance GHC.Records.HasField "unwrapStdlib_CFile" (Ptr.Ptr Stdlib_CFile) (Ptr.Ptr HsBindgen.Runtime.LibC.CFile) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CFile")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CFile "unwrapStdlib_CFile" where

  type CFieldType Stdlib_CFile "unwrapStdlib_CFile" =
    HsBindgen.Runtime.LibC.CFile

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CFpos@

    __defined at:__ @binding-specs\/stdlib\/instances.h 61:16@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CFpos = Stdlib_CFpos
  { unwrapStdlib_CFpos :: HsBindgen.Runtime.LibC.CFpos
  }
  deriving stock (GHC.Generics.Generic)

instance GHC.Records.HasField "unwrapStdlib_CFpos" (Ptr.Ptr Stdlib_CFpos) (Ptr.Ptr HsBindgen.Runtime.LibC.CFpos) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CFpos")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CFpos "unwrapStdlib_CFpos" where

  type CFieldType Stdlib_CFpos "unwrapStdlib_CFpos" =
    HsBindgen.Runtime.LibC.CFpos

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CSigAtomic@

    __defined at:__ @binding-specs\/stdlib\/instances.h 64:22@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CSigAtomic = Stdlib_CSigAtomic
  { unwrapStdlib_CSigAtomic :: HsBindgen.Runtime.LibC.CSigAtomic
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

instance GHC.Records.HasField "unwrapStdlib_CSigAtomic" (Ptr.Ptr Stdlib_CSigAtomic) (Ptr.Ptr HsBindgen.Runtime.LibC.CSigAtomic) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CSigAtomic")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CSigAtomic "unwrapStdlib_CSigAtomic" where

  type CFieldType Stdlib_CSigAtomic "unwrapStdlib_CSigAtomic" =
    HsBindgen.Runtime.LibC.CSigAtomic

  offset# = \_ -> \_ -> 0
