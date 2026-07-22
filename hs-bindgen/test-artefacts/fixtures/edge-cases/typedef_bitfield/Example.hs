{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.MyInt(..)
    , Example.MyUInt(..)
    , Example.MyLong(..)
    , Example.MyStruct(..)
    )
  where

import qualified HsBindgen.Runtime.BitfieldPtr as BitfieldPtr
import qualified HsBindgen.Runtime.HasCBitfield as HasCBitfield
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @myInt@

    __defined at:__ @edge-cases\/typedef_bitfield.h 2:13@

    __exported by:__ @edge-cases\/typedef_bitfield.h@
-}
newtype MyInt = MyInt
  { unwrapMyInt :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapMyInt" MyInt ty where

  hasField =
    \x0 ->
      (\y1 ->
         MyInt {unwrapMyInt = y1}, BG.getField @"unwrapMyInt" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapMyInt" (BG.Ptr MyInt) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMyInt")

instance HasCField.HasCField MyInt "unwrapMyInt" where

  type CFieldType MyInt "unwrapMyInt" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @myUInt@

    __defined at:__ @edge-cases\/typedef_bitfield.h 3:22@

    __exported by:__ @edge-cases\/typedef_bitfield.h@
-}
newtype MyUInt = MyUInt
  { unwrapMyUInt :: BG.CUInt
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
         ) => BG.CompatHasField.HasField "unwrapMyUInt" MyUInt ty where

  hasField =
    \x0 ->
      (\y1 ->
         MyUInt {unwrapMyUInt = y1}, BG.getField @"unwrapMyUInt" x0)

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapMyUInt" (BG.Ptr MyUInt) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMyUInt")

instance HasCField.HasCField MyUInt "unwrapMyUInt" where

  type CFieldType MyUInt "unwrapMyUInt" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @myLong@

    __defined at:__ @edge-cases\/typedef_bitfield.h 4:14@

    __exported by:__ @edge-cases\/typedef_bitfield.h@
-}
newtype MyLong = MyLong
  { unwrapMyLong :: BG.CLong
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
         ) => BG.CompatHasField.HasField "unwrapMyLong" MyLong ty where

  hasField =
    \x0 ->
      (\y1 ->
         MyLong {unwrapMyLong = y1}, BG.getField @"unwrapMyLong" x0)

instance ( ty ~ BG.CLong
         ) => BG.HasField "unwrapMyLong" (BG.Ptr MyLong) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMyLong")

instance HasCField.HasCField MyLong "unwrapMyLong" where

  type CFieldType MyLong "unwrapMyLong" = BG.CLong

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
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize MyStruct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw MyStruct where

  readRaw =
    \ptr0 ->
          pure MyStruct
      <*> HasCBitfield.peek (BG.Proxy @"myStruct_x") ptr0
      <*> HasCBitfield.peek (BG.Proxy @"myStruct_y") ptr0
      <*> HasCBitfield.peek (BG.Proxy @"myStruct_z") ptr0

instance Marshal.WriteRaw MyStruct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyStruct myStruct_x2 myStruct_y3 myStruct_z4 ->
               HasCBitfield.poke (BG.Proxy @"myStruct_x") ptr0 myStruct_x2
            >> HasCBitfield.poke (BG.Proxy @"myStruct_y") ptr0 myStruct_y3
            >> HasCBitfield.poke (BG.Proxy @"myStruct_z") ptr0 myStruct_z4

deriving via Marshal.EquivStorable MyStruct instance BG.Storable MyStruct

{-| __C declaration:__ @x@

    __defined at:__ @edge-cases\/typedef_bitfield.h 7:9@

    __exported by:__ @edge-cases\/typedef_bitfield.h@
-}
instance ( ty ~ MyInt
         ) => BG.CompatHasField.HasField "myStruct_x" MyStruct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          MyStruct { myStruct_x = y1
                   , myStruct_y = BG.getField @"myStruct_y" x0
                   , myStruct_z = BG.getField @"myStruct_z" x0
                   }
      , BG.getField @"myStruct_x" x0
      )

instance ( ty ~ MyInt
         ) => BG.HasField "myStruct_x" (BG.Ptr MyStruct) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"myStruct_x")

instance HasCBitfield.HasCBitfield MyStruct "myStruct_x" where

  type CBitfieldType MyStruct "myStruct_x" = MyInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 2

{-| __C declaration:__ @y@

    __defined at:__ @edge-cases\/typedef_bitfield.h 8:10@

    __exported by:__ @edge-cases\/typedef_bitfield.h@
-}
instance ( ty ~ MyUInt
         ) => BG.CompatHasField.HasField "myStruct_y" MyStruct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          MyStruct { myStruct_y = y1
                   , myStruct_x = BG.getField @"myStruct_x" x0
                   , myStruct_z = BG.getField @"myStruct_z" x0
                   }
      , BG.getField @"myStruct_y" x0
      )

instance ( ty ~ MyUInt
         ) => BG.HasField "myStruct_y" (BG.Ptr MyStruct) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"myStruct_y")

instance HasCBitfield.HasCBitfield MyStruct "myStruct_y" where

  type CBitfieldType MyStruct "myStruct_y" = MyUInt

  bitfieldOffset# = \_ -> \_ -> 2

  bitfieldWidth# = \_ -> \_ -> 4

{-| __C declaration:__ @z@

    __defined at:__ @edge-cases\/typedef_bitfield.h 9:10@

    __exported by:__ @edge-cases\/typedef_bitfield.h@
-}
instance ( ty ~ MyLong
         ) => BG.CompatHasField.HasField "myStruct_z" MyStruct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          MyStruct { myStruct_z = y1
                   , myStruct_x = BG.getField @"myStruct_x" x0
                   , myStruct_y = BG.getField @"myStruct_y" x0
                   }
      , BG.getField @"myStruct_z" x0
      )

instance ( ty ~ MyLong
         ) => BG.HasField "myStruct_z" (BG.Ptr MyStruct) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (BG.Proxy @"myStruct_z")

instance HasCBitfield.HasCBitfield MyStruct "myStruct_z" where

  type CBitfieldType MyStruct "myStruct_z" = MyLong

  bitfieldOffset# = \_ -> \_ -> 6

  bitfieldWidth# = \_ -> \_ -> 3
