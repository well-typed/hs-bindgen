{-# LANGUAGE DataKinds #-}
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
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Internal.Bitfield
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

{-| __C declaration:__ @stdlib_CBool@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 20:14@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CBool = Stdlib_CBool
  { unwrapStdlib_CBool :: FC.CBool
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CBool) "unwrapStdlib_CBool")
         ) => GHC.Records.HasField "unwrapStdlib_CBool" (Ptr.Ptr Stdlib_CBool) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CBool")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CBool "unwrapStdlib_CBool" where

  type CFieldType Stdlib_CBool "unwrapStdlib_CBool" =
    FC.CBool

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_Int8@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 23:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_Int8 = Stdlib_Int8
  { unwrapStdlib_Int8 :: HsBindgen.Runtime.LibC.Int8
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_Int8) "unwrapStdlib_Int8")
         ) => GHC.Records.HasField "unwrapStdlib_Int8" (Ptr.Ptr Stdlib_Int8) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_Int8")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_Int8 "unwrapStdlib_Int8" where

  type CFieldType Stdlib_Int8 "unwrapStdlib_Int8" =
    HsBindgen.Runtime.LibC.Int8

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_Int16@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 24:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_Int16 = Stdlib_Int16
  { unwrapStdlib_Int16 :: HsBindgen.Runtime.LibC.Int16
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_Int16) "unwrapStdlib_Int16")
         ) => GHC.Records.HasField "unwrapStdlib_Int16" (Ptr.Ptr Stdlib_Int16) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_Int16")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_Int16 "unwrapStdlib_Int16" where

  type CFieldType Stdlib_Int16 "unwrapStdlib_Int16" =
    HsBindgen.Runtime.LibC.Int16

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_Int32@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 25:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_Int32 = Stdlib_Int32
  { unwrapStdlib_Int32 :: HsBindgen.Runtime.LibC.Int32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_Int32) "unwrapStdlib_Int32")
         ) => GHC.Records.HasField "unwrapStdlib_Int32" (Ptr.Ptr Stdlib_Int32) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_Int32")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_Int32 "unwrapStdlib_Int32" where

  type CFieldType Stdlib_Int32 "unwrapStdlib_Int32" =
    HsBindgen.Runtime.LibC.Int32

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_Int64@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 26:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_Int64 = Stdlib_Int64
  { unwrapStdlib_Int64 :: HsBindgen.Runtime.LibC.Int64
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_Int64) "unwrapStdlib_Int64")
         ) => GHC.Records.HasField "unwrapStdlib_Int64" (Ptr.Ptr Stdlib_Int64) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_Int64")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_Int64 "unwrapStdlib_Int64" where

  type CFieldType Stdlib_Int64 "unwrapStdlib_Int64" =
    HsBindgen.Runtime.LibC.Int64

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_Word8@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 27:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_Word8 = Stdlib_Word8
  { unwrapStdlib_Word8 :: HsBindgen.Runtime.LibC.Word8
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_Word8) "unwrapStdlib_Word8")
         ) => GHC.Records.HasField "unwrapStdlib_Word8" (Ptr.Ptr Stdlib_Word8) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_Word8")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_Word8 "unwrapStdlib_Word8" where

  type CFieldType Stdlib_Word8 "unwrapStdlib_Word8" =
    HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_Word16@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 28:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_Word16 = Stdlib_Word16
  { unwrapStdlib_Word16 :: HsBindgen.Runtime.LibC.Word16
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_Word16) "unwrapStdlib_Word16")
         ) => GHC.Records.HasField "unwrapStdlib_Word16" (Ptr.Ptr Stdlib_Word16) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_Word16")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_Word16 "unwrapStdlib_Word16" where

  type CFieldType Stdlib_Word16 "unwrapStdlib_Word16" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_Word32@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 29:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_Word32 = Stdlib_Word32
  { unwrapStdlib_Word32 :: HsBindgen.Runtime.LibC.Word32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_Word32) "unwrapStdlib_Word32")
         ) => GHC.Records.HasField "unwrapStdlib_Word32" (Ptr.Ptr Stdlib_Word32) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_Word32")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_Word32 "unwrapStdlib_Word32" where

  type CFieldType Stdlib_Word32 "unwrapStdlib_Word32" =
    HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_Word64@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 30:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_Word64 = Stdlib_Word64
  { unwrapStdlib_Word64 :: HsBindgen.Runtime.LibC.Word64
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_Word64) "unwrapStdlib_Word64")
         ) => GHC.Records.HasField "unwrapStdlib_Word64" (Ptr.Ptr Stdlib_Word64) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_Word64")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_Word64 "unwrapStdlib_Word64" where

  type CFieldType Stdlib_Word64 "unwrapStdlib_Word64" =
    HsBindgen.Runtime.LibC.Word64

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CIntMax@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 31:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CIntMax = Stdlib_CIntMax
  { unwrapStdlib_CIntMax :: HsBindgen.Runtime.LibC.CIntMax
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CIntMax) "unwrapStdlib_CIntMax")
         ) => GHC.Records.HasField "unwrapStdlib_CIntMax" (Ptr.Ptr Stdlib_CIntMax) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CIntMax")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CIntMax "unwrapStdlib_CIntMax" where

  type CFieldType Stdlib_CIntMax "unwrapStdlib_CIntMax" =
    HsBindgen.Runtime.LibC.CIntMax

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CUIntMax@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 32:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CUIntMax = Stdlib_CUIntMax
  { unwrapStdlib_CUIntMax :: HsBindgen.Runtime.LibC.CUIntMax
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CUIntMax) "unwrapStdlib_CUIntMax")
         ) => GHC.Records.HasField "unwrapStdlib_CUIntMax" (Ptr.Ptr Stdlib_CUIntMax) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CUIntMax")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CUIntMax "unwrapStdlib_CUIntMax" where

  type CFieldType Stdlib_CUIntMax "unwrapStdlib_CUIntMax" =
    HsBindgen.Runtime.LibC.CUIntMax

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CIntPtr@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 33:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CIntPtr = Stdlib_CIntPtr
  { unwrapStdlib_CIntPtr :: HsBindgen.Runtime.LibC.CIntPtr
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CIntPtr) "unwrapStdlib_CIntPtr")
         ) => GHC.Records.HasField "unwrapStdlib_CIntPtr" (Ptr.Ptr Stdlib_CIntPtr) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CIntPtr")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CIntPtr "unwrapStdlib_CIntPtr" where

  type CFieldType Stdlib_CIntPtr "unwrapStdlib_CIntPtr" =
    HsBindgen.Runtime.LibC.CIntPtr

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CUIntPtr@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 34:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CUIntPtr = Stdlib_CUIntPtr
  { unwrapStdlib_CUIntPtr :: HsBindgen.Runtime.LibC.CUIntPtr
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CUIntPtr) "unwrapStdlib_CUIntPtr")
         ) => GHC.Records.HasField "unwrapStdlib_CUIntPtr" (Ptr.Ptr Stdlib_CUIntPtr) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CUIntPtr")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CUIntPtr "unwrapStdlib_CUIntPtr" where

  type CFieldType Stdlib_CUIntPtr "unwrapStdlib_CUIntPtr" =
    HsBindgen.Runtime.LibC.CUIntPtr

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CFenvT@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 37:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CFenvT = Stdlib_CFenvT
  { unwrapStdlib_CFenvT :: HsBindgen.Runtime.LibC.CFenvT
  }

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CFenvT) "unwrapStdlib_CFenvT")
         ) => GHC.Records.HasField "unwrapStdlib_CFenvT" (Ptr.Ptr Stdlib_CFenvT) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CFenvT")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CFenvT "unwrapStdlib_CFenvT" where

  type CFieldType Stdlib_CFenvT "unwrapStdlib_CFenvT" =
    HsBindgen.Runtime.LibC.CFenvT

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CFexceptT@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 38:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CFexceptT = Stdlib_CFexceptT
  { unwrapStdlib_CFexceptT :: HsBindgen.Runtime.LibC.CFexceptT
  }

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CFexceptT) "unwrapStdlib_CFexceptT")
         ) => GHC.Records.HasField "unwrapStdlib_CFexceptT" (Ptr.Ptr Stdlib_CFexceptT) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CFexceptT")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CFexceptT "unwrapStdlib_CFexceptT" where

  type CFieldType Stdlib_CFexceptT "unwrapStdlib_CFexceptT" =
    HsBindgen.Runtime.LibC.CFexceptT

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CDivT@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 41:17@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CDivT = Stdlib_CDivT
  { unwrapStdlib_CDivT :: HsBindgen.Runtime.LibC.CDivT
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CDivT) "unwrapStdlib_CDivT")
         ) => GHC.Records.HasField "unwrapStdlib_CDivT" (Ptr.Ptr Stdlib_CDivT) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CDivT")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CDivT "unwrapStdlib_CDivT" where

  type CFieldType Stdlib_CDivT "unwrapStdlib_CDivT" =
    HsBindgen.Runtime.LibC.CDivT

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CLdivT@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 42:17@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CLdivT = Stdlib_CLdivT
  { unwrapStdlib_CLdivT :: HsBindgen.Runtime.LibC.CLdivT
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CLdivT) "unwrapStdlib_CLdivT")
         ) => GHC.Records.HasField "unwrapStdlib_CLdivT" (Ptr.Ptr Stdlib_CLdivT) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CLdivT")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CLdivT "unwrapStdlib_CLdivT" where

  type CFieldType Stdlib_CLdivT "unwrapStdlib_CLdivT" =
    HsBindgen.Runtime.LibC.CLdivT

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CLldivT@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 43:17@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CLldivT = Stdlib_CLldivT
  { unwrapStdlib_CLldivT :: HsBindgen.Runtime.LibC.CLldivT
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CLldivT) "unwrapStdlib_CLldivT")
         ) => GHC.Records.HasField "unwrapStdlib_CLldivT" (Ptr.Ptr Stdlib_CLldivT) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CLldivT")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CLldivT "unwrapStdlib_CLldivT" where

  type CFieldType Stdlib_CLldivT "unwrapStdlib_CLldivT" =
    HsBindgen.Runtime.LibC.CLldivT

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CImaxdivT@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 44:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CImaxdivT = Stdlib_CImaxdivT
  { unwrapStdlib_CImaxdivT :: HsBindgen.Runtime.LibC.CImaxdivT
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CImaxdivT) "unwrapStdlib_CImaxdivT")
         ) => GHC.Records.HasField "unwrapStdlib_CImaxdivT" (Ptr.Ptr Stdlib_CImaxdivT) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CImaxdivT")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CImaxdivT "unwrapStdlib_CImaxdivT" where

  type CFieldType Stdlib_CImaxdivT "unwrapStdlib_CImaxdivT" =
    HsBindgen.Runtime.LibC.CImaxdivT

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CSize@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 47:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CSize = Stdlib_CSize
  { unwrapStdlib_CSize :: HsBindgen.Runtime.LibC.CSize
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CSize) "unwrapStdlib_CSize")
         ) => GHC.Records.HasField "unwrapStdlib_CSize" (Ptr.Ptr Stdlib_CSize) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CSize")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CSize "unwrapStdlib_CSize" where

  type CFieldType Stdlib_CSize "unwrapStdlib_CSize" =
    HsBindgen.Runtime.LibC.CSize

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CPtrdiff@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 48:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CPtrdiff = Stdlib_CPtrdiff
  { unwrapStdlib_CPtrdiff :: HsBindgen.Runtime.LibC.CPtrdiff
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CPtrdiff) "unwrapStdlib_CPtrdiff")
         ) => GHC.Records.HasField "unwrapStdlib_CPtrdiff" (Ptr.Ptr Stdlib_CPtrdiff) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CPtrdiff")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CPtrdiff "unwrapStdlib_CPtrdiff" where

  type CFieldType Stdlib_CPtrdiff "unwrapStdlib_CPtrdiff" =
    HsBindgen.Runtime.LibC.CPtrdiff

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CJmpBuf@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 51:17@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CJmpBuf = Stdlib_CJmpBuf
  { unwrapStdlib_CJmpBuf :: HsBindgen.Runtime.LibC.CJmpBuf
  }

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CJmpBuf) "unwrapStdlib_CJmpBuf")
         ) => GHC.Records.HasField "unwrapStdlib_CJmpBuf" (Ptr.Ptr Stdlib_CJmpBuf) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CJmpBuf")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CJmpBuf "unwrapStdlib_CJmpBuf" where

  type CFieldType Stdlib_CJmpBuf "unwrapStdlib_CJmpBuf" =
    HsBindgen.Runtime.LibC.CJmpBuf

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CWchar@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 54:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CWchar = Stdlib_CWchar
  { unwrapStdlib_CWchar :: HsBindgen.Runtime.LibC.CWchar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CWchar) "unwrapStdlib_CWchar")
         ) => GHC.Records.HasField "unwrapStdlib_CWchar" (Ptr.Ptr Stdlib_CWchar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CWchar")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CWchar "unwrapStdlib_CWchar" where

  type CFieldType Stdlib_CWchar "unwrapStdlib_CWchar" =
    HsBindgen.Runtime.LibC.CWchar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CWintT@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 55:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CWintT = Stdlib_CWintT
  { unwrapStdlib_CWintT :: HsBindgen.Runtime.LibC.CWintT
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CWintT) "unwrapStdlib_CWintT")
         ) => GHC.Records.HasField "unwrapStdlib_CWintT" (Ptr.Ptr Stdlib_CWintT) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CWintT")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CWintT "unwrapStdlib_CWintT" where

  type CFieldType Stdlib_CWintT "unwrapStdlib_CWintT" =
    HsBindgen.Runtime.LibC.CWintT

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CMbstateT@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 56:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CMbstateT = Stdlib_CMbstateT
  { unwrapStdlib_CMbstateT :: HsBindgen.Runtime.LibC.CMbstateT
  }

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CMbstateT) "unwrapStdlib_CMbstateT")
         ) => GHC.Records.HasField "unwrapStdlib_CMbstateT" (Ptr.Ptr Stdlib_CMbstateT) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CMbstateT")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CMbstateT "unwrapStdlib_CMbstateT" where

  type CFieldType Stdlib_CMbstateT "unwrapStdlib_CMbstateT" =
    HsBindgen.Runtime.LibC.CMbstateT

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CWctransT@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 57:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CWctransT = Stdlib_CWctransT
  { unwrapStdlib_CWctransT :: HsBindgen.Runtime.LibC.CWctransT
  }
  deriving stock (Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CWctransT) "unwrapStdlib_CWctransT")
         ) => GHC.Records.HasField "unwrapStdlib_CWctransT" (Ptr.Ptr Stdlib_CWctransT) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CWctransT")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CWctransT "unwrapStdlib_CWctransT" where

  type CFieldType Stdlib_CWctransT "unwrapStdlib_CWctransT" =
    HsBindgen.Runtime.LibC.CWctransT

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CWctypeT@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 58:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CWctypeT = Stdlib_CWctypeT
  { unwrapStdlib_CWctypeT :: HsBindgen.Runtime.LibC.CWctypeT
  }
  deriving stock (Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CWctypeT) "unwrapStdlib_CWctypeT")
         ) => GHC.Records.HasField "unwrapStdlib_CWctypeT" (Ptr.Ptr Stdlib_CWctypeT) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CWctypeT")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CWctypeT "unwrapStdlib_CWctypeT" where

  type CFieldType Stdlib_CWctypeT "unwrapStdlib_CWctypeT" =
    HsBindgen.Runtime.LibC.CWctypeT

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CChar16T@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 59:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CChar16T = Stdlib_CChar16T
  { unwrapStdlib_CChar16T :: HsBindgen.Runtime.LibC.CChar16T
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CChar16T) "unwrapStdlib_CChar16T")
         ) => GHC.Records.HasField "unwrapStdlib_CChar16T" (Ptr.Ptr Stdlib_CChar16T) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CChar16T")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CChar16T "unwrapStdlib_CChar16T" where

  type CFieldType Stdlib_CChar16T "unwrapStdlib_CChar16T" =
    HsBindgen.Runtime.LibC.CChar16T

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CChar32T@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 60:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CChar32T = Stdlib_CChar32T
  { unwrapStdlib_CChar32T :: HsBindgen.Runtime.LibC.CChar32T
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CChar32T) "unwrapStdlib_CChar32T")
         ) => GHC.Records.HasField "unwrapStdlib_CChar32T" (Ptr.Ptr Stdlib_CChar32T) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CChar32T")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CChar32T "unwrapStdlib_CChar32T" where

  type CFieldType Stdlib_CChar32T "unwrapStdlib_CChar32T" =
    HsBindgen.Runtime.LibC.CChar32T

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CTime@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 63:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CTime = Stdlib_CTime
  { unwrapStdlib_CTime :: HsBindgen.Runtime.LibC.CTime
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
    , Enum
    , Num
    , Real
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CTime) "unwrapStdlib_CTime")
         ) => GHC.Records.HasField "unwrapStdlib_CTime" (Ptr.Ptr Stdlib_CTime) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CTime")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CTime "unwrapStdlib_CTime" where

  type CFieldType Stdlib_CTime "unwrapStdlib_CTime" =
    HsBindgen.Runtime.LibC.CTime

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CClock@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 64:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CClock = Stdlib_CClock
  { unwrapStdlib_CClock :: HsBindgen.Runtime.LibC.CClock
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
    , Enum
    , Num
    , Real
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CClock) "unwrapStdlib_CClock")
         ) => GHC.Records.HasField "unwrapStdlib_CClock" (Ptr.Ptr Stdlib_CClock) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CClock")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CClock "unwrapStdlib_CClock" where

  type CFieldType Stdlib_CClock "unwrapStdlib_CClock" =
    HsBindgen.Runtime.LibC.CClock

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CTm@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 65:19@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CTm = Stdlib_CTm
  { unwrapStdlib_CTm :: HsBindgen.Runtime.LibC.CTm
  }
  deriving stock (Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CTm) "unwrapStdlib_CTm")
         ) => GHC.Records.HasField "unwrapStdlib_CTm" (Ptr.Ptr Stdlib_CTm) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CTm")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CTm "unwrapStdlib_CTm" where

  type CFieldType Stdlib_CTm "unwrapStdlib_CTm" =
    HsBindgen.Runtime.LibC.CTm

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CFile@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 68:16@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CFile = Stdlib_CFile
  { unwrapStdlib_CFile :: HsBindgen.Runtime.LibC.CFile
  }

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CFile) "unwrapStdlib_CFile")
         ) => GHC.Records.HasField "unwrapStdlib_CFile" (Ptr.Ptr Stdlib_CFile) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CFile")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CFile "unwrapStdlib_CFile" where

  type CFieldType Stdlib_CFile "unwrapStdlib_CFile" =
    HsBindgen.Runtime.LibC.CFile

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CFpos@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 69:16@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CFpos = Stdlib_CFpos
  { unwrapStdlib_CFpos :: HsBindgen.Runtime.LibC.CFpos
  }

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CFpos) "unwrapStdlib_CFpos")
         ) => GHC.Records.HasField "unwrapStdlib_CFpos" (Ptr.Ptr Stdlib_CFpos) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CFpos")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CFpos "unwrapStdlib_CFpos" where

  type CFieldType Stdlib_CFpos "unwrapStdlib_CFpos" =
    HsBindgen.Runtime.LibC.CFpos

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @stdlib_CSigAtomic@

    __defined at:__ @types\/stdlib\/stdlib_insts.h 72:22@

    __exported by:__ @types\/stdlib\/stdlib_insts.h@
-}
newtype Stdlib_CSigAtomic = Stdlib_CSigAtomic
  { unwrapStdlib_CSigAtomic :: HsBindgen.Runtime.LibC.CSigAtomic
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Stdlib_CSigAtomic) "unwrapStdlib_CSigAtomic")
         ) => GHC.Records.HasField "unwrapStdlib_CSigAtomic" (Ptr.Ptr Stdlib_CSigAtomic) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStdlib_CSigAtomic")

instance HsBindgen.Runtime.HasCField.HasCField Stdlib_CSigAtomic "unwrapStdlib_CSigAtomic" where

  type CFieldType Stdlib_CSigAtomic "unwrapStdlib_CSigAtomic" =
    HsBindgen.Runtime.LibC.CSigAtomic

  offset# = \_ -> \_ -> 0
