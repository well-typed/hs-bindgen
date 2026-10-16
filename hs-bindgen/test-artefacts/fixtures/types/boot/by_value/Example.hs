{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Colour(..)
    , pattern Example.RED
    , pattern Example.GREEN
    , pattern Example.BLUE
    , Example.Value(..)
    , Example.Pair(..)
    , Example.Sample(..)
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, type (~))

{-| __C declaration:__ @enum colour@

    __defined at:__ @types\/boot\/by_value.h 7:6@

    __exported by:__ @types\/boot\/by_value.h@
-}
newtype Colour = Colour
  { unwrapColour :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize Colour where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Colour where

  readRaw =
    \ptr0 ->
          pure Colour
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Colour where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Colour unwrapColour2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapColour2

deriving via Marshal.EquivStorable Colour instance BG.Storable Colour

deriving via BG.CUInt instance BG.Prim Colour

instance CEnum.CEnum Colour where

  type CEnumZ Colour = BG.CUInt

  toCEnum = Colour

  fromCEnum = BG.getField @"unwrapColour"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "RED"), (1, BG.singleton "GREEN"), (2, BG.singleton "BLUE")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Colour"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Colour"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Colour where

  minDeclaredValue = RED

  maxDeclaredValue = BLUE

instance Show Colour where

  showsPrec = CEnum.shows

instance Read Colour where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapColour" Colour ty where

  hasField =
    \x0 ->
      (\y1 ->
         Colour {unwrapColour = y1}, BG.getField @"unwrapColour" x0)

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapColour" (BG.Ptr Colour) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapColour")

instance HasCField.HasCField Colour "unwrapColour" where

  type CFieldType Colour "unwrapColour" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @RED@

    __defined at:__ @types\/boot\/by_value.h 7:15@

    __exported by:__ @types\/boot\/by_value.h@
-}
pattern RED :: Colour
pattern RED = Colour 0

{-| __C declaration:__ @GREEN@

    __defined at:__ @types\/boot\/by_value.h 7:20@

    __exported by:__ @types\/boot\/by_value.h@
-}
pattern GREEN :: Colour
pattern GREEN = Colour 1

{-| __C declaration:__ @BLUE@

    __defined at:__ @types\/boot\/by_value.h 7:27@

    __exported by:__ @types\/boot\/by_value.h@
-}
pattern BLUE :: Colour
pattern BLUE = Colour 2

{-| __C declaration:__ @union value@

    __defined at:__ @types\/boot\/by_value.h 9:7@

    __exported by:__ @types\/boot\/by_value.h@
-}
newtype Value = Value
  { unwrapValue :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize Value

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw Value

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw Value

deriving via Marshal.EquivStorable Value instance BG.Storable Value

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion Value

{-| __C declaration:__ @i@

    __defined at:__ @types\/boot\/by_value.h 10:7@

    __exported by:__ @types\/boot\/by_value.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "value_i" Value ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @i@

    __defined at:__ @types\/boot\/by_value.h 10:7@

    __exported by:__ @types\/boot\/by_value.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "value_i" Value ty where

  hasField =
    \x0 ->
      (\y1 ->
         BG.setUnionPayload y1 x0, BG.getField @"value_i" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "value_i" (BG.Ptr Value) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"value_i")

instance HasCField.HasCField Value "value_i" where

  type CFieldType Value "value_i" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f@

    __defined at:__ @types\/boot\/by_value.h 11:9@

    __exported by:__ @types\/boot\/by_value.h@
-}
instance (ty ~ BG.CFloat) => BG.HasField "value_f" Value ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @f@

    __defined at:__ @types\/boot\/by_value.h 11:9@

    __exported by:__ @types\/boot\/by_value.h@
-}
instance (ty ~ BG.CFloat) => BG.CompatHasField.HasField "value_f" Value ty where

  hasField =
    \x0 ->
      (\y1 ->
         BG.setUnionPayload y1 x0, BG.getField @"value_f" x0)

instance ( ty ~ BG.CFloat
         ) => BG.HasField "value_f" (BG.Ptr Value) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"value_f")

instance HasCField.HasCField Value "value_f" where

  type CFieldType Value "value_f" = BG.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct pair@

    __defined at:__ @types\/boot\/by_value.h 14:8@

    __exported by:__ @types\/boot\/by_value.h@
-}
data Pair = Pair
  { pair_first :: BG.CInt
    {- ^ __C declaration:__ @first@

         __defined at:__ @types\/boot\/by_value.h 15:7@

         __exported by:__ @types\/boot\/by_value.h@
    -}
  , pair_second :: BG.CInt
    {- ^ __C declaration:__ @second@

         __defined at:__ @types\/boot\/by_value.h 16:7@

         __exported by:__ @types\/boot\/by_value.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Pair where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Pair where

  readRaw =
    \ptr0 ->
          pure Pair
      <*> HasCField.readRaw (BG.Proxy @"pair_first") ptr0
      <*> HasCField.readRaw (BG.Proxy @"pair_second") ptr0

instance Marshal.WriteRaw Pair where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Pair pair_first2 pair_second3 ->
               HasCField.writeRaw (BG.Proxy @"pair_first") ptr0 pair_first2
            >> HasCField.writeRaw (BG.Proxy @"pair_second") ptr0 pair_second3

deriving via Marshal.EquivStorable Pair instance BG.Storable Pair

deriving via Struct.IsStructViaReadRaw Pair instance Struct.IsStruct Pair

{-| __C declaration:__ @first@

    __defined at:__ @types\/boot\/by_value.h 15:7@

    __exported by:__ @types\/boot\/by_value.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "pair_first" Pair ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Pair {pair_first = y1, pair_second = BG.getField @"pair_second" x0}
      , BG.getField @"pair_first" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "pair_first" (BG.Ptr Pair) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"pair_first")

instance HasCField.HasCField Pair "pair_first" where

  type CFieldType Pair "pair_first" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @second@

    __defined at:__ @types\/boot\/by_value.h 16:7@

    __exported by:__ @types\/boot\/by_value.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "pair_second" Pair ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Pair {pair_second = y1, pair_first = BG.getField @"pair_first" x0}
      , BG.getField @"pair_second" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "pair_second" (BG.Ptr Pair) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"pair_second")

instance HasCField.HasCField Pair "pair_second" where

  type CFieldType Pair "pair_second" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @sample@

    __defined at:__ @types\/boot\/by_value.h 21:17@

    __exported by:__ @types\/boot\/by_value.h@
-}
newtype Sample = Sample
  { unwrapSample :: HsBindgen.Runtime.LibC.Int32
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
         ) => BG.CompatHasField.HasField "unwrapSample" Sample ty where

  hasField =
    \x0 ->
      (\y1 ->
         Sample {unwrapSample = y1}, BG.getField @"unwrapSample" x0)

instance ( ty ~ HsBindgen.Runtime.LibC.Int32
         ) => BG.HasField "unwrapSample" (BG.Ptr Sample) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapSample")

instance HasCField.HasCField Sample "unwrapSample" where

  type CFieldType Sample "unwrapSample" =
    HsBindgen.Runtime.LibC.Int32

  offset# = \_ -> \_ -> 0
