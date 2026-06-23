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
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Internal.Prelude.CompatHasField as RIP.CompatHasField
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @myInt@

    __defined at:__ @edge-cases\/typedef_bitfield.h 2:13@

    __exported by:__ @edge-cases\/typedef_bitfield.h@
-}
newtype MyInt = MyInt
  { unwrapMyInt :: RIP.CInt
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

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "unwrapMyInt" (RIP.Ptr MyInt) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMyInt")

instance HasCField.HasCField MyInt "unwrapMyInt" where

  type CFieldType MyInt "unwrapMyInt" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "unwrapMyInt" MyInt ty where

  hasField =
    \x0 ->
      (\y1 ->
         MyInt {unwrapMyInt = y1}, RIP.getField @"unwrapMyInt" x0)

{-| __C declaration:__ @myUInt@

    __defined at:__ @edge-cases\/typedef_bitfield.h 3:22@

    __exported by:__ @edge-cases\/typedef_bitfield.h@
-}
newtype MyUInt = MyUInt
  { unwrapMyUInt :: RIP.CUInt
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

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapMyUInt" (RIP.Ptr MyUInt) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMyUInt")

instance HasCField.HasCField MyUInt "unwrapMyUInt" where

  type CFieldType MyUInt "unwrapMyUInt" = RIP.CUInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CUInt
         ) => RIP.CompatHasField.HasField "unwrapMyUInt" MyUInt ty where

  hasField =
    \x0 ->
      (\y1 ->
         MyUInt {unwrapMyUInt = y1}, RIP.getField @"unwrapMyUInt" x0)

{-| __C declaration:__ @myLong@

    __defined at:__ @edge-cases\/typedef_bitfield.h 4:14@

    __exported by:__ @edge-cases\/typedef_bitfield.h@
-}
newtype MyLong = MyLong
  { unwrapMyLong :: RIP.CLong
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

instance ( ty ~ RIP.CLong
         ) => RIP.HasField "unwrapMyLong" (RIP.Ptr MyLong) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMyLong")

instance HasCField.HasCField MyLong "unwrapMyLong" where

  type CFieldType MyLong "unwrapMyLong" = RIP.CLong

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CLong
         ) => RIP.CompatHasField.HasField "unwrapMyLong" MyLong ty where

  hasField =
    \x0 ->
      (\y1 ->
         MyLong {unwrapMyLong = y1}, RIP.getField @"unwrapMyLong" x0)

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
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize MyStruct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw MyStruct where

  readRaw =
    \ptr0 ->
          pure MyStruct
      <*> HasCBitfield.peek (RIP.Proxy @"myStruct_x") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"myStruct_y") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"myStruct_z") ptr0

instance Marshal.WriteRaw MyStruct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyStruct myStruct_x2 myStruct_y3 myStruct_z4 ->
               HasCBitfield.poke (RIP.Proxy @"myStruct_x") ptr0 myStruct_x2
            >> HasCBitfield.poke (RIP.Proxy @"myStruct_y") ptr0 myStruct_y3
            >> HasCBitfield.poke (RIP.Proxy @"myStruct_z") ptr0 myStruct_z4

deriving via Marshal.EquivStorable MyStruct instance RIP.Storable MyStruct

instance HasCBitfield.HasCBitfield MyStruct "myStruct_x" where

  type CBitfieldType MyStruct "myStruct_x" = MyInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 2

instance ( ty ~ MyInt
         ) => RIP.HasField "myStruct_x" (RIP.Ptr MyStruct) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"myStruct_x")

instance ( ty ~ MyInt
         ) => RIP.CompatHasField.HasField "myStruct_x" MyStruct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          MyStruct { myStruct_x = y1
                   , myStruct_y = RIP.getField @"myStruct_y" x0
                   , myStruct_z = RIP.getField @"myStruct_z" x0
                   }
      , RIP.getField @"myStruct_x" x0
      )

instance HasCBitfield.HasCBitfield MyStruct "myStruct_y" where

  type CBitfieldType MyStruct "myStruct_y" = MyUInt

  bitfieldOffset# = \_ -> \_ -> 2

  bitfieldWidth# = \_ -> \_ -> 4

instance ( ty ~ MyUInt
         ) => RIP.HasField "myStruct_y" (RIP.Ptr MyStruct) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"myStruct_y")

instance ( ty ~ MyUInt
         ) => RIP.CompatHasField.HasField "myStruct_y" MyStruct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          MyStruct { myStruct_y = y1
                   , myStruct_x = RIP.getField @"myStruct_x" x0
                   , myStruct_z = RIP.getField @"myStruct_z" x0
                   }
      , RIP.getField @"myStruct_y" x0
      )

instance HasCBitfield.HasCBitfield MyStruct "myStruct_z" where

  type CBitfieldType MyStruct "myStruct_z" = MyLong

  bitfieldOffset# = \_ -> \_ -> 6

  bitfieldWidth# = \_ -> \_ -> 3

instance ( ty ~ MyLong
         ) => RIP.HasField "myStruct_z" (RIP.Ptr MyStruct) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"myStruct_z")

instance ( ty ~ MyLong
         ) => RIP.CompatHasField.HasField "myStruct_z" MyStruct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          MyStruct { myStruct_z = y1
                   , myStruct_x = RIP.getField @"myStruct_x" x0
                   , myStruct_y = RIP.getField @"myStruct_y" x0
                   }
      , RIP.getField @"myStruct_z" x0
      )
