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
import qualified HsBindgen.Runtime.Bitfield
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import Data.Bits (FiniteBits)
import GHC.Exts ((*#), (+#))
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @myInt@

    __defined at:__ @edge-cases\/typedef_bitfield.h 2:13@

    __exported by:__ @edge-cases\/typedef_bitfield.h@
-}
newtype MyInt = MyInt
  { un_MyInt :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType, Data.Primitive.Types.Prim, HsBindgen.Runtime.Bitfield.Bitfield, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MyInt) "un_MyInt")
         ) => GHC.Records.HasField "un_MyInt" (Ptr.Ptr MyInt) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_MyInt")

instance HsBindgen.Runtime.HasCField.HasCField MyInt "un_MyInt" where

  type CFieldType MyInt "un_MyInt" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @myUInt@

    __defined at:__ @edge-cases\/typedef_bitfield.h 3:22@

    __exported by:__ @edge-cases\/typedef_bitfield.h@
-}
newtype MyUInt = MyUInt
  { un_MyUInt :: FC.CUInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType, Data.Primitive.Types.Prim, HsBindgen.Runtime.Bitfield.Bitfield, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MyUInt) "un_MyUInt")
         ) => GHC.Records.HasField "un_MyUInt" (Ptr.Ptr MyUInt) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_MyUInt")

instance HsBindgen.Runtime.HasCField.HasCField MyUInt "un_MyUInt" where

  type CFieldType MyUInt "un_MyUInt" = FC.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @myLong@

    __defined at:__ @edge-cases\/typedef_bitfield.h 4:14@

    __exported by:__ @edge-cases\/typedef_bitfield.h@
-}
newtype MyLong = MyLong
  { un_MyLong :: FC.CLong
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType, Data.Primitive.Types.Prim, HsBindgen.Runtime.Bitfield.Bitfield, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MyLong) "un_MyLong")
         ) => GHC.Records.HasField "un_MyLong" (Ptr.Ptr MyLong) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_MyLong")

instance HsBindgen.Runtime.HasCField.HasCField MyLong "un_MyLong" where

  type CFieldType MyLong "un_MyLong" = FC.CLong

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
  deriving stock (Eq, Show)

instance F.Storable MyStruct where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure MyStruct
      <*> HsBindgen.Runtime.HasCField.peekCBitfield (Data.Proxy.Proxy @"myStruct_x") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCBitfield (Data.Proxy.Proxy @"myStruct_y") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCBitfield (Data.Proxy.Proxy @"myStruct_z") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyStruct myStruct_x2 myStruct_y3 myStruct_z4 ->
               HsBindgen.Runtime.HasCField.pokeCBitfield (Data.Proxy.Proxy @"myStruct_x") ptr0 myStruct_x2
            >> HsBindgen.Runtime.HasCField.pokeCBitfield (Data.Proxy.Proxy @"myStruct_y") ptr0 myStruct_y3
            >> HsBindgen.Runtime.HasCField.pokeCBitfield (Data.Proxy.Proxy @"myStruct_z") ptr0 myStruct_z4

instance Data.Primitive.Types.Prim MyStruct where

  sizeOf# = \_ -> (8#)

  alignment# = \_ -> (8#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        MyStruct (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (3#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (3#) i1) (1#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (3#) i1) (2#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (3#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (3#) i1) (1#)) s3 of
                (# s5, v6 #) ->
                  case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (3#) i1) (2#)) s5 of
                    (# s7, v8 #) -> (# s7, MyStruct v4 v6 v8 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              MyStruct myStruct_x4 myStruct_y5 myStruct_z6 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (3#) i1) (0#)) myStruct_x4 s3 of
                  s7 ->
                    case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (3#) i1) (1#)) myStruct_y5 s7 of
                      s8 ->
                        Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (3#) i1) (2#)) myStruct_z6 s8

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        MyStruct (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (3#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (3#) i1) (1#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (3#) i1) (2#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (3#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (3#) i1) (1#)) s3 of
                (# s5, v6 #) ->
                  case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (3#) i1) (2#)) s5 of
                    (# s7, v8 #) -> (# s7, MyStruct v4 v6 v8 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              MyStruct myStruct_x4 myStruct_y5 myStruct_z6 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (3#) i1) (0#)) myStruct_x4 s3 of
                  s7 ->
                    case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (3#) i1) (1#)) myStruct_y5 s7 of
                      s8 ->
                        Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (3#) i1) (2#)) myStruct_z6 s8

instance HsBindgen.Runtime.HasCField.HasCBitfield MyStruct "myStruct_x" where

  type CBitfieldType MyStruct "myStruct_x" = MyInt

  bitOffset# = \_ -> \_ -> 0

  bitWidth# = \_ -> \_ -> 2

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CBitfieldType MyStruct) "myStruct_x")
         ) => GHC.Records.HasField "myStruct_x" (Ptr.Ptr MyStruct) ((HsBindgen.Runtime.HasCField.BitfieldPtr MyStruct) "myStruct_x") where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCBitfield (Data.Proxy.Proxy @"myStruct_x")

instance HsBindgen.Runtime.HasCField.HasCBitfield MyStruct "myStruct_y" where

  type CBitfieldType MyStruct "myStruct_y" = MyUInt

  bitOffset# = \_ -> \_ -> 2

  bitWidth# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CBitfieldType MyStruct) "myStruct_y")
         ) => GHC.Records.HasField "myStruct_y" (Ptr.Ptr MyStruct) ((HsBindgen.Runtime.HasCField.BitfieldPtr MyStruct) "myStruct_y") where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCBitfield (Data.Proxy.Proxy @"myStruct_y")

instance HsBindgen.Runtime.HasCField.HasCBitfield MyStruct "myStruct_z" where

  type CBitfieldType MyStruct "myStruct_z" = MyLong

  bitOffset# = \_ -> \_ -> 6

  bitWidth# = \_ -> \_ -> 3

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CBitfieldType MyStruct) "myStruct_z")
         ) => GHC.Records.HasField "myStruct_z" (Ptr.Ptr MyStruct) ((HsBindgen.Runtime.HasCField.BitfieldPtr MyStruct) "myStruct_z") where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCBitfield (Data.Proxy.Proxy @"myStruct_z")
