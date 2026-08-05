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
    ( Example.UntaggedPoint(..)
    , Example.UntaggedPair(..)
    , Example.UntaggedEnum(..)
    , pattern Example.VAL_A
    , pattern Example.VAL_B
    , Example.UntaggedEnumCoords(..)
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
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @struct \@untaggedPoint@

    __defined at:__ @globals\/untagged.h 12:1@

    __exported by:__ @globals\/untagged.h@
-}
data UntaggedPoint = UntaggedPoint
  { untaggedPoint_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals\/untagged.h 12:14@

         __exported by:__ @globals\/untagged.h@
    -}
  , untaggedPoint_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals\/untagged.h 12:21@

         __exported by:__ @globals\/untagged.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize UntaggedPoint where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw UntaggedPoint where

  readRaw =
    \ptr0 ->
          pure UntaggedPoint
      <*> HasCField.readRaw (BG.Proxy @"untaggedPoint_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"untaggedPoint_y") ptr0

instance Marshal.WriteRaw UntaggedPoint where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          UntaggedPoint untaggedPoint_x2 untaggedPoint_y3 ->
               HasCField.writeRaw (BG.Proxy @"untaggedPoint_x") ptr0 untaggedPoint_x2
            >> HasCField.writeRaw (BG.Proxy @"untaggedPoint_y") ptr0 untaggedPoint_y3

deriving via Marshal.EquivStorable UntaggedPoint instance BG.Storable UntaggedPoint

deriving via Struct.IsStructViaReadRaw UntaggedPoint instance Struct.IsStruct UntaggedPoint

{-| __C declaration:__ @x@

    __defined at:__ @globals\/untagged.h 12:14@

    __exported by:__ @globals\/untagged.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "untaggedPoint_x" UntaggedPoint ty where

  hasField =
    \x0 ->
      ( \y1 ->
          UntaggedPoint {untaggedPoint_x = y1, untaggedPoint_y = BG.getField @"untaggedPoint_y" x0}
      , BG.getField @"untaggedPoint_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "untaggedPoint_x" (BG.Ptr UntaggedPoint) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"untaggedPoint_x")

instance HasCField.HasCField UntaggedPoint "untaggedPoint_x" where

  type CFieldType UntaggedPoint "untaggedPoint_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @globals\/untagged.h 12:21@

    __exported by:__ @globals\/untagged.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "untaggedPoint_y" UntaggedPoint ty where

  hasField =
    \x0 ->
      ( \y1 ->
          UntaggedPoint {untaggedPoint_y = y1, untaggedPoint_x = BG.getField @"untaggedPoint_x" x0}
      , BG.getField @"untaggedPoint_y" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "untaggedPoint_y" (BG.Ptr UntaggedPoint) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"untaggedPoint_y")

instance HasCField.HasCField UntaggedPoint "untaggedPoint_y" where

  type CFieldType UntaggedPoint "untaggedPoint_y" =
    BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct \@untaggedPair@

    __defined at:__ @globals\/untagged.h 14:1@

    __exported by:__ @globals\/untagged.h@
-}
data UntaggedPair = UntaggedPair
  { untaggedPair_a :: BG.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @globals\/untagged.h 14:14@

         __exported by:__ @globals\/untagged.h@
    -}
  , untaggedPair_b :: BG.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @globals\/untagged.h 14:21@

         __exported by:__ @globals\/untagged.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize UntaggedPair where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw UntaggedPair where

  readRaw =
    \ptr0 ->
          pure UntaggedPair
      <*> HasCField.readRaw (BG.Proxy @"untaggedPair_a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"untaggedPair_b") ptr0

instance Marshal.WriteRaw UntaggedPair where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          UntaggedPair untaggedPair_a2 untaggedPair_b3 ->
               HasCField.writeRaw (BG.Proxy @"untaggedPair_a") ptr0 untaggedPair_a2
            >> HasCField.writeRaw (BG.Proxy @"untaggedPair_b") ptr0 untaggedPair_b3

deriving via Marshal.EquivStorable UntaggedPair instance BG.Storable UntaggedPair

deriving via Struct.IsStructViaReadRaw UntaggedPair instance Struct.IsStruct UntaggedPair

{-| __C declaration:__ @a@

    __defined at:__ @globals\/untagged.h 14:14@

    __exported by:__ @globals\/untagged.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "untaggedPair_a" UntaggedPair ty where

  hasField =
    \x0 ->
      ( \y1 ->
          UntaggedPair {untaggedPair_a = y1, untaggedPair_b = BG.getField @"untaggedPair_b" x0}
      , BG.getField @"untaggedPair_a" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "untaggedPair_a" (BG.Ptr UntaggedPair) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"untaggedPair_a")

instance HasCField.HasCField UntaggedPair "untaggedPair_a" where

  type CFieldType UntaggedPair "untaggedPair_a" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @globals\/untagged.h 14:21@

    __exported by:__ @globals\/untagged.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "untaggedPair_b" UntaggedPair ty where

  hasField =
    \x0 ->
      ( \y1 ->
          UntaggedPair {untaggedPair_b = y1, untaggedPair_a = BG.getField @"untaggedPair_a" x0}
      , BG.getField @"untaggedPair_b" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "untaggedPair_b" (BG.Ptr UntaggedPair) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"untaggedPair_b")

instance HasCField.HasCField UntaggedPair "untaggedPair_b" where

  type CFieldType UntaggedPair "untaggedPair_b" =
    BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @enum \@untaggedEnum@

    __defined at:__ @globals\/untagged.h 16:1@

    __exported by:__ @globals\/untagged.h@
-}
newtype UntaggedEnum = UntaggedEnum
  { unwrapUntaggedEnum :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize UntaggedEnum where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw UntaggedEnum where

  readRaw =
    \ptr0 ->
          pure UntaggedEnum
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw UntaggedEnum where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          UntaggedEnum unwrapUntaggedEnum2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapUntaggedEnum2

deriving via Marshal.EquivStorable UntaggedEnum instance BG.Storable UntaggedEnum

deriving via BG.CUInt instance BG.Prim UntaggedEnum

instance CEnum.CEnum UntaggedEnum where

  type CEnumZ UntaggedEnum = BG.CUInt

  toCEnum = UntaggedEnum

  fromCEnum = BG.getField @"unwrapUntaggedEnum"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "VAL_A"), (1, BG.singleton "VAL_B")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "UntaggedEnum"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "UntaggedEnum"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum UntaggedEnum where

  minDeclaredValue = VAL_A

  maxDeclaredValue = VAL_B

instance Show UntaggedEnum where

  showsPrec = CEnum.shows

instance Read UntaggedEnum where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapUntaggedEnum" UntaggedEnum ty where

  hasField =
    \x0 ->
      ( \y1 -> UntaggedEnum {unwrapUntaggedEnum = y1}
      , BG.getField @"unwrapUntaggedEnum" x0
      )

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapUntaggedEnum" (BG.Ptr UntaggedEnum) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapUntaggedEnum")

instance HasCField.HasCField UntaggedEnum "unwrapUntaggedEnum" where

  type CFieldType UntaggedEnum "unwrapUntaggedEnum" =
    BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @VAL_A@

    __defined at:__ @globals\/untagged.h 16:8@

    __exported by:__ @globals\/untagged.h@
-}
pattern VAL_A :: UntaggedEnum
pattern VAL_A = UntaggedEnum 0

{-| __C declaration:__ @VAL_B@

    __defined at:__ @globals\/untagged.h 16:19@

    __exported by:__ @globals\/untagged.h@
-}
pattern VAL_B :: UntaggedEnum
pattern VAL_B = UntaggedEnum 1

{-| __C declaration:__ @enum \@untaggedEnumCoords@

    __defined at:__ @globals\/untagged.h 18:1@

    __exported by:__ @globals\/untagged.h@
-}
newtype UntaggedEnumCoords = UntaggedEnumCoords
  { unwrapUntaggedEnumCoords :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize UntaggedEnumCoords where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw UntaggedEnumCoords where

  readRaw =
    \ptr0 ->
          pure UntaggedEnumCoords
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw UntaggedEnumCoords where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          UntaggedEnumCoords unwrapUntaggedEnumCoords2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapUntaggedEnumCoords2

deriving via Marshal.EquivStorable UntaggedEnumCoords instance BG.Storable UntaggedEnumCoords

deriving via BG.CUInt instance BG.Prim UntaggedEnumCoords

instance CEnum.CEnum UntaggedEnumCoords where

  type CEnumZ UntaggedEnumCoords = BG.CUInt

  toCEnum = UntaggedEnumCoords

  fromCEnum = BG.getField @"unwrapUntaggedEnumCoords"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(10, BG.singleton "X"), (20, BG.singleton "Y"), (30, BG.singleton "Z")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "UntaggedEnumCoords"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "UntaggedEnumCoords"

instance Show UntaggedEnumCoords where

  showsPrec = CEnum.shows

instance Read UntaggedEnumCoords where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapUntaggedEnumCoords" UntaggedEnumCoords ty where

  hasField =
    \x0 ->
      ( \y1 ->
          UntaggedEnumCoords {unwrapUntaggedEnumCoords = y1}
      , BG.getField @"unwrapUntaggedEnumCoords" x0
      )

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapUntaggedEnumCoords" (BG.Ptr UntaggedEnumCoords) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapUntaggedEnumCoords")

instance HasCField.HasCField UntaggedEnumCoords "unwrapUntaggedEnumCoords" where

  type CFieldType UntaggedEnumCoords "unwrapUntaggedEnumCoords" =
    BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @X@

    __defined at:__ @globals\/untagged.h 18:8@

    __exported by:__ @globals\/untagged.h@
-}
pattern X :: UntaggedEnumCoords
pattern X = UntaggedEnumCoords 10

{-| __C declaration:__ @Y@

    __defined at:__ @globals\/untagged.h 18:16@

    __exported by:__ @globals\/untagged.h@
-}
pattern Y :: UntaggedEnumCoords
pattern Y = UntaggedEnumCoords 20

{-| __C declaration:__ @Z@

    __defined at:__ @globals\/untagged.h 18:24@

    __exported by:__ @globals\/untagged.h@
-}
pattern Z :: UntaggedEnumCoords
pattern Z = UntaggedEnumCoords 30

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
    \x0 ->
      (\y1 ->
         BG.setUnionPayload y1 x0, BG.getField @"b_x" x0)

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

deriving via Struct.IsStructViaReadRaw C instance Struct.IsStruct C

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
