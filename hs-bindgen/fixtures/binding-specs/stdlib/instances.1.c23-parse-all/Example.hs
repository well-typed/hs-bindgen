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

{-| __C declaration:__ @stdlib_CBool@

    __defined at:__ @binding-specs\/stdlib\/instances.h 18:14@

    __exported by:__ @binding-specs\/stdlib\/instances.h@
-}
newtype Stdlib_CBool = Stdlib_CBool
  { unwrapStdlib_CBool :: HsBindgen.Runtime.LibC.CBool
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

instance ( ((~) ty) HsBindgen.Runtime.LibC.CBool
         ) => RIP.HasField "unwrapStdlib_CBool" (RIP.Ptr Stdlib_CBool) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CBool")

instance HasCField.HasCField Stdlib_CBool "unwrapStdlib_CBool" where

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
         ) => RIP.HasField "unwrapStdlib_Int8" (RIP.Ptr Stdlib_Int8) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_Int8")

instance HasCField.HasCField Stdlib_Int8 "unwrapStdlib_Int8" where

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
         ) => RIP.HasField "unwrapStdlib_Int16" (RIP.Ptr Stdlib_Int16) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_Int16")

instance HasCField.HasCField Stdlib_Int16 "unwrapStdlib_Int16" where

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
         ) => RIP.HasField "unwrapStdlib_Int32" (RIP.Ptr Stdlib_Int32) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_Int32")

instance HasCField.HasCField Stdlib_Int32 "unwrapStdlib_Int32" where

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
         ) => RIP.HasField "unwrapStdlib_Int64" (RIP.Ptr Stdlib_Int64) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_Int64")

instance HasCField.HasCField Stdlib_Int64 "unwrapStdlib_Int64" where

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
         ) => RIP.HasField "unwrapStdlib_Word8" (RIP.Ptr Stdlib_Word8) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_Word8")

instance HasCField.HasCField Stdlib_Word8 "unwrapStdlib_Word8" where

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
         ) => RIP.HasField "unwrapStdlib_Word16" (RIP.Ptr Stdlib_Word16) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_Word16")

instance HasCField.HasCField Stdlib_Word16 "unwrapStdlib_Word16" where

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
         ) => RIP.HasField "unwrapStdlib_Word32" (RIP.Ptr Stdlib_Word32) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_Word32")

instance HasCField.HasCField Stdlib_Word32 "unwrapStdlib_Word32" where

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
         ) => RIP.HasField "unwrapStdlib_Word64" (RIP.Ptr Stdlib_Word64) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_Word64")

instance HasCField.HasCField Stdlib_Word64 "unwrapStdlib_Word64" where

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

instance ( ((~) ty) HsBindgen.Runtime.LibC.CIntMax
         ) => RIP.HasField "unwrapStdlib_CIntMax" (RIP.Ptr Stdlib_CIntMax) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CIntMax")

instance HasCField.HasCField Stdlib_CIntMax "unwrapStdlib_CIntMax" where

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

instance ( ((~) ty) HsBindgen.Runtime.LibC.CUIntMax
         ) => RIP.HasField "unwrapStdlib_CUIntMax" (RIP.Ptr Stdlib_CUIntMax) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CUIntMax")

instance HasCField.HasCField Stdlib_CUIntMax "unwrapStdlib_CUIntMax" where

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

instance ( ((~) ty) HsBindgen.Runtime.LibC.CIntPtr
         ) => RIP.HasField "unwrapStdlib_CIntPtr" (RIP.Ptr Stdlib_CIntPtr) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CIntPtr")

instance HasCField.HasCField Stdlib_CIntPtr "unwrapStdlib_CIntPtr" where

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

instance ( ((~) ty) HsBindgen.Runtime.LibC.CUIntPtr
         ) => RIP.HasField "unwrapStdlib_CUIntPtr" (RIP.Ptr Stdlib_CUIntPtr) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CUIntPtr")

instance HasCField.HasCField Stdlib_CUIntPtr "unwrapStdlib_CUIntPtr" where

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
  deriving stock (RIP.Generic)

instance ( ((~) ty) HsBindgen.Runtime.LibC.CFenvT
         ) => RIP.HasField "unwrapStdlib_CFenvT" (RIP.Ptr Stdlib_CFenvT) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CFenvT")

instance HasCField.HasCField Stdlib_CFenvT "unwrapStdlib_CFenvT" where

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
  deriving stock (RIP.Generic)

instance ( ((~) ty) HsBindgen.Runtime.LibC.CFexceptT
         ) => RIP.HasField "unwrapStdlib_CFexceptT" (RIP.Ptr Stdlib_CFexceptT) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CFexceptT")

instance HasCField.HasCField Stdlib_CFexceptT "unwrapStdlib_CFexceptT" where

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

instance ( ((~) ty) HsBindgen.Runtime.LibC.CSize
         ) => RIP.HasField "unwrapStdlib_CSize" (RIP.Ptr Stdlib_CSize) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CSize")

instance HasCField.HasCField Stdlib_CSize "unwrapStdlib_CSize" where

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
         ) => RIP.HasField "unwrapStdlib_CPtrdiff" (RIP.Ptr Stdlib_CPtrdiff) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CPtrdiff")

instance HasCField.HasCField Stdlib_CPtrdiff "unwrapStdlib_CPtrdiff" where

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
  deriving stock (RIP.Generic)

instance ( ((~) ty) HsBindgen.Runtime.LibC.CJmpBuf
         ) => RIP.HasField "unwrapStdlib_CJmpBuf" (RIP.Ptr Stdlib_CJmpBuf) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CJmpBuf")

instance HasCField.HasCField Stdlib_CJmpBuf "unwrapStdlib_CJmpBuf" where

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

instance ( ((~) ty) HsBindgen.Runtime.LibC.CWchar
         ) => RIP.HasField "unwrapStdlib_CWchar" (RIP.Ptr Stdlib_CWchar) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CWchar")

instance HasCField.HasCField Stdlib_CWchar "unwrapStdlib_CWchar" where

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

instance ( ((~) ty) HsBindgen.Runtime.LibC.CWintT
         ) => RIP.HasField "unwrapStdlib_CWintT" (RIP.Ptr Stdlib_CWintT) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CWintT")

instance HasCField.HasCField Stdlib_CWintT "unwrapStdlib_CWintT" where

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
  deriving stock (RIP.Generic)

instance ( ((~) ty) HsBindgen.Runtime.LibC.CMbstateT
         ) => RIP.HasField "unwrapStdlib_CMbstateT" (RIP.Ptr Stdlib_CMbstateT) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CMbstateT")

instance HasCField.HasCField Stdlib_CMbstateT "unwrapStdlib_CMbstateT" where

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
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( RIP.HasFFIType
    , RIP.Prim
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) HsBindgen.Runtime.LibC.CWctransT
         ) => RIP.HasField "unwrapStdlib_CWctransT" (RIP.Ptr Stdlib_CWctransT) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CWctransT")

instance HasCField.HasCField Stdlib_CWctransT "unwrapStdlib_CWctransT" where

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
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( RIP.HasFFIType
    , RIP.Prim
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) HsBindgen.Runtime.LibC.CWctypeT
         ) => RIP.HasField "unwrapStdlib_CWctypeT" (RIP.Ptr Stdlib_CWctypeT) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CWctypeT")

instance HasCField.HasCField Stdlib_CWctypeT "unwrapStdlib_CWctypeT" where

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

instance ( ((~) ty) HsBindgen.Runtime.LibC.CChar16T
         ) => RIP.HasField "unwrapStdlib_CChar16T" (RIP.Ptr Stdlib_CChar16T) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CChar16T")

instance HasCField.HasCField Stdlib_CChar16T "unwrapStdlib_CChar16T" where

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

instance ( ((~) ty) HsBindgen.Runtime.LibC.CChar32T
         ) => RIP.HasField "unwrapStdlib_CChar32T" (RIP.Ptr Stdlib_CChar32T) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CChar32T")

instance HasCField.HasCField Stdlib_CChar32T "unwrapStdlib_CChar32T" where

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
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( Enum
    , RIP.HasFFIType
    , Num
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) HsBindgen.Runtime.LibC.CTime
         ) => RIP.HasField "unwrapStdlib_CTime" (RIP.Ptr Stdlib_CTime) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CTime")

instance HasCField.HasCField Stdlib_CTime "unwrapStdlib_CTime" where

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
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( Enum
    , RIP.HasFFIType
    , Num
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) HsBindgen.Runtime.LibC.CClock
         ) => RIP.HasField "unwrapStdlib_CClock" (RIP.Ptr Stdlib_CClock) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CClock")

instance HasCField.HasCField Stdlib_CClock "unwrapStdlib_CClock" where

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
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype (Marshal.ReadRaw, Marshal.StaticSize)

instance ( ((~) ty) HsBindgen.Runtime.LibC.CTm
         ) => RIP.HasField "unwrapStdlib_CTm" (RIP.Ptr Stdlib_CTm) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CTm")

instance HasCField.HasCField Stdlib_CTm "unwrapStdlib_CTm" where

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
  deriving stock (RIP.Generic)

instance ( ((~) ty) HsBindgen.Runtime.LibC.CFile
         ) => RIP.HasField "unwrapStdlib_CFile" (RIP.Ptr Stdlib_CFile) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CFile")

instance HasCField.HasCField Stdlib_CFile "unwrapStdlib_CFile" where

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
  deriving stock (RIP.Generic)

instance ( ((~) ty) HsBindgen.Runtime.LibC.CFpos
         ) => RIP.HasField "unwrapStdlib_CFpos" (RIP.Ptr Stdlib_CFpos) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CFpos")

instance HasCField.HasCField Stdlib_CFpos "unwrapStdlib_CFpos" where

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

instance ( ((~) ty) HsBindgen.Runtime.LibC.CSigAtomic
         ) => RIP.HasField "unwrapStdlib_CSigAtomic" (RIP.Ptr Stdlib_CSigAtomic) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CSigAtomic")

instance HasCField.HasCField Stdlib_CSigAtomic "unwrapStdlib_CSigAtomic" where

  type CFieldType Stdlib_CSigAtomic "unwrapStdlib_CSigAtomic" =
    HsBindgen.Runtime.LibC.CSigAtomic

  offset# = \_ -> \_ -> 0
