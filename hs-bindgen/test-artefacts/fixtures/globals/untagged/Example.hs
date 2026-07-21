{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.AnonPoint(..)
    , Example.AnonPair(..)
    , Example.AnonEnum(..)
    , pattern Example.VAL_A
    , pattern Example.VAL_B
    , Example.AnonEnumCoords(..)
    , pattern Example.X
    , pattern Example.Y
    , pattern Example.Z
    , Example.A(..)
    , pattern Example.A1
    , Example.B(..)
    , Example.C(..)
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @struct \@anonPoint@

    __defined at:__ @globals\/untagged.h 12:1@

    __exported by:__ @globals\/untagged.h@
-}
data AnonPoint = AnonPoint
  { anonPoint_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals\/untagged.h 12:14@

         __exported by:__ @globals\/untagged.h@
    -}
  , anonPoint_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals\/untagged.h 12:21@

         __exported by:__ @globals\/untagged.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize AnonPoint where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw AnonPoint where

  readRaw =
    \ptr0 ->
          pure AnonPoint
      <*> HasCField.readRaw (BG.Proxy @"anonPoint_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"anonPoint_y") ptr0

instance Marshal.WriteRaw AnonPoint where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          AnonPoint anonPoint_x2 anonPoint_y3 ->
               HasCField.writeRaw (BG.Proxy @"anonPoint_x") ptr0 anonPoint_x2
            >> HasCField.writeRaw (BG.Proxy @"anonPoint_y") ptr0 anonPoint_y3

deriving via Marshal.EquivStorable AnonPoint instance BG.Storable AnonPoint

{-| __C declaration:__ @x@

    __defined at:__ @globals\/untagged.h 12:14@

    __exported by:__ @globals\/untagged.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "anonPoint_x" AnonPoint ty where

  hasField =
    \x0 ->
      ( \y1 ->
          AnonPoint {anonPoint_x = y1, anonPoint_y = BG.getField @"anonPoint_y" x0}
      , BG.getField @"anonPoint_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "anonPoint_x" (BG.Ptr AnonPoint) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"anonPoint_x")

instance HasCField.HasCField AnonPoint "anonPoint_x" where

  type CFieldType AnonPoint "anonPoint_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @globals\/untagged.h 12:21@

    __exported by:__ @globals\/untagged.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "anonPoint_y" AnonPoint ty where

  hasField =
    \x0 ->
      ( \y1 ->
          AnonPoint {anonPoint_y = y1, anonPoint_x = BG.getField @"anonPoint_x" x0}
      , BG.getField @"anonPoint_y" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "anonPoint_y" (BG.Ptr AnonPoint) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"anonPoint_y")

instance HasCField.HasCField AnonPoint "anonPoint_y" where

  type CFieldType AnonPoint "anonPoint_y" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct \@anonPair@

    __defined at:__ @globals\/untagged.h 14:1@

    __exported by:__ @globals\/untagged.h@
-}
data AnonPair = AnonPair
  { anonPair_a :: BG.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @globals\/untagged.h 14:14@

         __exported by:__ @globals\/untagged.h@
    -}
  , anonPair_b :: BG.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @globals\/untagged.h 14:21@

         __exported by:__ @globals\/untagged.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize AnonPair where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw AnonPair where

  readRaw =
    \ptr0 ->
          pure AnonPair
      <*> HasCField.readRaw (BG.Proxy @"anonPair_a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"anonPair_b") ptr0

instance Marshal.WriteRaw AnonPair where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          AnonPair anonPair_a2 anonPair_b3 ->
               HasCField.writeRaw (BG.Proxy @"anonPair_a") ptr0 anonPair_a2
            >> HasCField.writeRaw (BG.Proxy @"anonPair_b") ptr0 anonPair_b3

deriving via Marshal.EquivStorable AnonPair instance BG.Storable AnonPair

{-| __C declaration:__ @a@

    __defined at:__ @globals\/untagged.h 14:14@

    __exported by:__ @globals\/untagged.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "anonPair_a" AnonPair ty where

  hasField =
    \x0 ->
      ( \y1 ->
          AnonPair {anonPair_a = y1, anonPair_b = BG.getField @"anonPair_b" x0}
      , BG.getField @"anonPair_a" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "anonPair_a" (BG.Ptr AnonPair) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"anonPair_a")

instance HasCField.HasCField AnonPair "anonPair_a" where

  type CFieldType AnonPair "anonPair_a" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @globals\/untagged.h 14:21@

    __exported by:__ @globals\/untagged.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "anonPair_b" AnonPair ty where

  hasField =
    \x0 ->
      ( \y1 ->
          AnonPair {anonPair_b = y1, anonPair_a = BG.getField @"anonPair_a" x0}
      , BG.getField @"anonPair_b" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "anonPair_b" (BG.Ptr AnonPair) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"anonPair_b")

instance HasCField.HasCField AnonPair "anonPair_b" where

  type CFieldType AnonPair "anonPair_b" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @enum \@anonEnum@

    __defined at:__ @globals\/untagged.h 16:1@

    __exported by:__ @globals\/untagged.h@
-}
newtype AnonEnum = AnonEnum
  { unwrapAnonEnum :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize AnonEnum where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw AnonEnum where

  readRaw =
    \ptr0 ->
          pure AnonEnum
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw AnonEnum where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          AnonEnum unwrapAnonEnum2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapAnonEnum2

deriving via Marshal.EquivStorable AnonEnum instance BG.Storable AnonEnum

deriving via BG.CUInt instance BG.Prim AnonEnum

instance CEnum.CEnum AnonEnum where

  type CEnumZ AnonEnum = BG.CUInt

  toCEnum = AnonEnum

  fromCEnum = BG.getField @"unwrapAnonEnum"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "VAL_A"), (1, BG.singleton "VAL_B")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "AnonEnum"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "AnonEnum"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum AnonEnum where

  minDeclaredValue = VAL_A

  maxDeclaredValue = VAL_B

instance Show AnonEnum where

  showsPrec = CEnum.shows

instance Read AnonEnum where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapAnonEnum" AnonEnum ty where

  hasField =
    \x0 ->
      (\y1 ->
         AnonEnum {unwrapAnonEnum = y1}, BG.getField @"unwrapAnonEnum" x0)

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapAnonEnum" (BG.Ptr AnonEnum) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapAnonEnum")

instance HasCField.HasCField AnonEnum "unwrapAnonEnum" where

  type CFieldType AnonEnum "unwrapAnonEnum" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @VAL_A@

    __defined at:__ @globals\/untagged.h 16:8@

    __exported by:__ @globals\/untagged.h@
-}
pattern VAL_A :: AnonEnum
pattern VAL_A = AnonEnum 0

{-| __C declaration:__ @VAL_B@

    __defined at:__ @globals\/untagged.h 16:19@

    __exported by:__ @globals\/untagged.h@
-}
pattern VAL_B :: AnonEnum
pattern VAL_B = AnonEnum 1

{-| __C declaration:__ @enum \@anonEnumCoords@

    __defined at:__ @globals\/untagged.h 18:1@

    __exported by:__ @globals\/untagged.h@
-}
newtype AnonEnumCoords = AnonEnumCoords
  { unwrapAnonEnumCoords :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize AnonEnumCoords where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw AnonEnumCoords where

  readRaw =
    \ptr0 ->
          pure AnonEnumCoords
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw AnonEnumCoords where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          AnonEnumCoords unwrapAnonEnumCoords2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapAnonEnumCoords2

deriving via Marshal.EquivStorable AnonEnumCoords instance BG.Storable AnonEnumCoords

deriving via BG.CUInt instance BG.Prim AnonEnumCoords

instance CEnum.CEnum AnonEnumCoords where

  type CEnumZ AnonEnumCoords = BG.CUInt

  toCEnum = AnonEnumCoords

  fromCEnum = BG.getField @"unwrapAnonEnumCoords"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(10, BG.singleton "X"), (20, BG.singleton "Y"), (30, BG.singleton "Z")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "AnonEnumCoords"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "AnonEnumCoords"

instance Show AnonEnumCoords where

  showsPrec = CEnum.shows

instance Read AnonEnumCoords where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapAnonEnumCoords" AnonEnumCoords ty where

  hasField =
    \x0 ->
      ( \y1 -> AnonEnumCoords {unwrapAnonEnumCoords = y1}
      , BG.getField @"unwrapAnonEnumCoords" x0
      )

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapAnonEnumCoords" (BG.Ptr AnonEnumCoords) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapAnonEnumCoords")

instance HasCField.HasCField AnonEnumCoords "unwrapAnonEnumCoords" where

  type CFieldType AnonEnumCoords "unwrapAnonEnumCoords" =
    BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @X@

    __defined at:__ @globals\/untagged.h 18:8@

    __exported by:__ @globals\/untagged.h@
-}
pattern X :: AnonEnumCoords
pattern X = AnonEnumCoords 10

{-| __C declaration:__ @Y@

    __defined at:__ @globals\/untagged.h 18:16@

    __exported by:__ @globals\/untagged.h@
-}
pattern Y :: AnonEnumCoords
pattern Y = AnonEnumCoords 20

{-| __C declaration:__ @Z@

    __defined at:__ @globals\/untagged.h 18:24@

    __exported by:__ @globals\/untagged.h@
-}
pattern Z :: AnonEnumCoords
pattern Z = AnonEnumCoords 30

{-| __C declaration:__ @enum \@A@

    __defined at:__ @globals\/untagged.h 24:1@

    __exported by:__ @globals\/untagged.h@
-}
newtype A = A
  { unwrapA :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize A where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw A where

  readRaw =
    \ptr0 ->
          pure A
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw A where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A unwrapA2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapA2

deriving via Marshal.EquivStorable A instance BG.Storable A

deriving via BG.CUInt instance BG.Prim A

instance CEnum.CEnum A where

  type CEnumZ A = BG.CUInt

  toCEnum = A

  fromCEnum = BG.getField @"unwrapA"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "A1")]

  showsUndeclared = CEnum.showsWrappedUndeclared "A"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "A"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum A where

  minDeclaredValue = A1

  maxDeclaredValue = A1

instance Show A where

  showsPrec = CEnum.shows

instance Read A where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance (ty ~ BG.CUInt) => BG.CompatHasField.HasField "unwrapA" A ty where

  hasField =
    \x0 ->
      (\y1 -> A {unwrapA = y1}, BG.getField @"unwrapA" x0)

instance (ty ~ BG.CUInt) => BG.HasField "unwrapA" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @a1@

    __defined at:__ @globals\/untagged.h 24:8@

    __exported by:__ @globals\/untagged.h@
-}
pattern A1 :: A
pattern A1 = A 0

{-| __C declaration:__ @union \@B@

    __defined at:__ @globals\/untagged.h 27:7@

    __exported by:__ @globals\/untagged.h@
-}
newtype B = B
  { unwrapB :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize B

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw B

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw B

deriving via Marshal.EquivStorable B instance BG.Storable B

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion B

{-| __C declaration:__ @x@

    __defined at:__ @globals\/untagged.h 27:19@

    __exported by:__ @globals\/untagged.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "b_x" B ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @globals\/untagged.h 27:19@

    __exported by:__ @globals\/untagged.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "b_x" B ty where

  hasField =
    \x0 -> (BG.setUnionPayload, BG.getField @"b_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "b_x" (BG.Ptr B) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"b_x")

instance HasCField.HasCField B "b_x" where

  type CFieldType B "b_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@C@

    __defined at:__ @globals\/untagged.h 30:1@

    __exported by:__ @globals\/untagged.h@
-}
data C = C
  { c_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals\/untagged.h 30:14@

         __exported by:__ @globals\/untagged.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize C where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw C where

  readRaw =
    \ptr0 ->
          pure C
      <*> HasCField.readRaw (BG.Proxy @"c_x") ptr0

instance Marshal.WriteRaw C where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          C c_x2 ->
            HasCField.writeRaw (BG.Proxy @"c_x") ptr0 c_x2

deriving via Marshal.EquivStorable C instance BG.Storable C

{-| __C declaration:__ @x@

    __defined at:__ @globals\/untagged.h 30:14@

    __exported by:__ @globals\/untagged.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "c_x" C ty where

  hasField =
    \x0 -> (\y1 -> C {c_x = y1}, BG.getField @"c_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "c_x" (BG.Ptr C) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"c_x")

instance HasCField.HasCField C "c_x" where

  type CFieldType C "c_x" = BG.CInt

  offset# = \_ -> \_ -> 0
