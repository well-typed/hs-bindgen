{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import qualified HsBindgen.Runtime.BitfieldPtr
import qualified HsBindgen.Runtime.HasCBitfield
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.Bitfield
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @myInt@

    __defined at:__ @edge-cases\/typedef_bitfield.h 2:13@

    __exported by:__ @edge-cases\/typedef_bitfield.h@
-}
newtype MyInt = MyInt
  { unwrapMyInt :: FC.CInt
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
         ) => GHC.Records.HasField "unwrapMyInt" (Ptr.Ptr MyInt) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMyInt")

instance HsBindgen.Runtime.HasCField.HasCField MyInt "unwrapMyInt" where

  type CFieldType MyInt "unwrapMyInt" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @myUInt@

    __defined at:__ @edge-cases\/typedef_bitfield.h 3:22@

    __exported by:__ @edge-cases\/typedef_bitfield.h@
-}
newtype MyUInt = MyUInt
  { unwrapMyUInt :: FC.CUInt
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
         ) => GHC.Records.HasField "unwrapMyUInt" (Ptr.Ptr MyUInt) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMyUInt")

instance HsBindgen.Runtime.HasCField.HasCField MyUInt "unwrapMyUInt" where

  type CFieldType MyUInt "unwrapMyUInt" = FC.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @myLong@

    __defined at:__ @edge-cases\/typedef_bitfield.h 4:14@

    __exported by:__ @edge-cases\/typedef_bitfield.h@
-}
newtype MyLong = MyLong
  { unwrapMyLong :: FC.CLong
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
         ) => GHC.Records.HasField "unwrapMyLong" (Ptr.Ptr MyLong) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMyLong")

instance HsBindgen.Runtime.HasCField.HasCField MyLong "unwrapMyLong" where

  type CFieldType MyLong "unwrapMyLong" = FC.CLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct myStruct@

    __defined at:__ @edge-cases\/typedef_bitfield.h 6:8@

    __exported by:__ @edge-cases\/typedef_bitfield.h@
-}
data MyStruct = MyStruct
  { myStruct_x :: MyInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/typedef_bitfield.h 7:9@

         __exported by:__ @edge-cases\/typedef_bitfield.h@
    -}
  , myStruct_y :: MyUInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/typedef_bitfield.h 8:10@

         __exported by:__ @edge-cases\/typedef_bitfield.h@
    -}
  , myStruct_z :: MyLong
    {- ^ __C declaration:__ @z@

         __defined at:__ @edge-cases\/typedef_bitfield.h 9:10@

         __exported by:__ @edge-cases\/typedef_bitfield.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize MyStruct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw MyStruct where

  readRaw =
    \ptr0 ->
          pure MyStruct
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"myStruct_x") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"myStruct_y") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"myStruct_z") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw MyStruct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyStruct myStruct_x2 myStruct_y3 myStruct_z4 ->
               HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"myStruct_x") ptr0 myStruct_x2
            >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"myStruct_y") ptr0 myStruct_y3
            >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"myStruct_z") ptr0 myStruct_z4

deriving via HsBindgen.Runtime.Marshal.EquivStorable MyStruct instance F.Storable MyStruct

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield MyStruct "myStruct_x" where

  type CBitfieldType MyStruct "myStruct_x" = MyInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 2

instance ( TyEq ty MyInt
         ) => GHC.Records.HasField "myStruct_x" (Ptr.Ptr MyStruct) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"myStruct_x")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield MyStruct "myStruct_y" where

  type CBitfieldType MyStruct "myStruct_y" = MyUInt

  bitfieldOffset# = \_ -> \_ -> 2

  bitfieldWidth# = \_ -> \_ -> 4

instance ( TyEq ty MyUInt
         ) => GHC.Records.HasField "myStruct_y" (Ptr.Ptr MyStruct) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"myStruct_y")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield MyStruct "myStruct_z" where

  type CBitfieldType MyStruct "myStruct_z" = MyLong

  bitfieldOffset# = \_ -> \_ -> 6

  bitfieldWidth# = \_ -> \_ -> 3

instance ( TyEq ty MyLong
         ) => GHC.Records.HasField "myStruct_z" (Ptr.Ptr MyStruct) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"myStruct_z")
