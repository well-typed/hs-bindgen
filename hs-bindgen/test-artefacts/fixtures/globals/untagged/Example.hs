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
    , Example.get_b_x
    , Example.set_b_x
    , Example.C(..)
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Internal.Prelude.CompatHasField as RIP.CompatHasField
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct \@anonPoint@

    __defined at:__ @globals\/untagged.h 12:1@

    __exported by:__ @globals\/untagged.h@
-}
data AnonPoint = AnonPoint
  { anonPoint_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals\/untagged.h 12:14@

         __exported by:__ @globals\/untagged.h@
    -}
  , anonPoint_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals\/untagged.h 12:21@

         __exported by:__ @globals\/untagged.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize AnonPoint where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw AnonPoint where

  readRaw =
    \ptr0 ->
          pure AnonPoint
      <*> HasCField.readRaw (RIP.Proxy @"anonPoint_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"anonPoint_y") ptr0

instance Marshal.WriteRaw AnonPoint where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          AnonPoint anonPoint_x2 anonPoint_y3 ->
               HasCField.writeRaw (RIP.Proxy @"anonPoint_x") ptr0 anonPoint_x2
            >> HasCField.writeRaw (RIP.Proxy @"anonPoint_y") ptr0 anonPoint_y3

deriving via Marshal.EquivStorable AnonPoint instance RIP.Storable AnonPoint

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "anonPoint_x" AnonPoint ty where

  hasField =
    \x0 ->
      ( \y1 ->
          AnonPoint {anonPoint_x = y1, anonPoint_y = RIP.getField @"anonPoint_y" x0}
      , RIP.getField @"anonPoint_x" x0
      )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "anonPoint_x" (RIP.Ptr AnonPoint) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"anonPoint_x")

instance HasCField.HasCField AnonPoint "anonPoint_x" where

  type CFieldType AnonPoint "anonPoint_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "anonPoint_y" AnonPoint ty where

  hasField =
    \x0 ->
      ( \y1 ->
          AnonPoint {anonPoint_y = y1, anonPoint_x = RIP.getField @"anonPoint_x" x0}
      , RIP.getField @"anonPoint_y" x0
      )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "anonPoint_y" (RIP.Ptr AnonPoint) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"anonPoint_y")

instance HasCField.HasCField AnonPoint "anonPoint_y" where

  type CFieldType AnonPoint "anonPoint_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct \@anonPair@

    __defined at:__ @globals\/untagged.h 14:1@

    __exported by:__ @globals\/untagged.h@
-}
data AnonPair = AnonPair
  { anonPair_a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @globals\/untagged.h 14:14@

         __exported by:__ @globals\/untagged.h@
    -}
  , anonPair_b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @globals\/untagged.h 14:21@

         __exported by:__ @globals\/untagged.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize AnonPair where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw AnonPair where

  readRaw =
    \ptr0 ->
          pure AnonPair
      <*> HasCField.readRaw (RIP.Proxy @"anonPair_a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"anonPair_b") ptr0

instance Marshal.WriteRaw AnonPair where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          AnonPair anonPair_a2 anonPair_b3 ->
               HasCField.writeRaw (RIP.Proxy @"anonPair_a") ptr0 anonPair_a2
            >> HasCField.writeRaw (RIP.Proxy @"anonPair_b") ptr0 anonPair_b3

deriving via Marshal.EquivStorable AnonPair instance RIP.Storable AnonPair

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "anonPair_a" AnonPair ty where

  hasField =
    \x0 ->
      ( \y1 ->
          AnonPair {anonPair_a = y1, anonPair_b = RIP.getField @"anonPair_b" x0}
      , RIP.getField @"anonPair_a" x0
      )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "anonPair_a" (RIP.Ptr AnonPair) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"anonPair_a")

instance HasCField.HasCField AnonPair "anonPair_a" where

  type CFieldType AnonPair "anonPair_a" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "anonPair_b" AnonPair ty where

  hasField =
    \x0 ->
      ( \y1 ->
          AnonPair {anonPair_b = y1, anonPair_a = RIP.getField @"anonPair_a" x0}
      , RIP.getField @"anonPair_b" x0
      )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "anonPair_b" (RIP.Ptr AnonPair) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"anonPair_b")

instance HasCField.HasCField AnonPair "anonPair_b" where

  type CFieldType AnonPair "anonPair_b" = RIP.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @enum \@anonEnum@

    __defined at:__ @globals\/untagged.h 16:1@

    __exported by:__ @globals\/untagged.h@
-}
newtype AnonEnum = AnonEnum
  { unwrapAnonEnum :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

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

deriving via Marshal.EquivStorable AnonEnum instance RIP.Storable AnonEnum

deriving via RIP.CUInt instance RIP.Prim AnonEnum

instance CEnum.CEnum AnonEnum where

  type CEnumZ AnonEnum = RIP.CUInt

  toCEnum = AnonEnum

  fromCEnum = RIP.getField @"unwrapAnonEnum"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "VAL_A"), (1, RIP.singleton "VAL_B")]

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

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.CompatHasField.HasField "unwrapAnonEnum" AnonEnum ty where

  hasField =
    \x0 ->
      (\y1 ->
         AnonEnum {unwrapAnonEnum = y1}, RIP.getField @"unwrapAnonEnum" x0)

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapAnonEnum" (RIP.Ptr AnonEnum) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapAnonEnum")

instance HasCField.HasCField AnonEnum "unwrapAnonEnum" where

  type CFieldType AnonEnum "unwrapAnonEnum" = RIP.CUInt

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
  { unwrapAnonEnumCoords :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

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

deriving via Marshal.EquivStorable AnonEnumCoords instance RIP.Storable AnonEnumCoords

deriving via RIP.CUInt instance RIP.Prim AnonEnumCoords

instance CEnum.CEnum AnonEnumCoords where

  type CEnumZ AnonEnumCoords = RIP.CUInt

  toCEnum = AnonEnumCoords

  fromCEnum = RIP.getField @"unwrapAnonEnumCoords"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(10, RIP.singleton "X"), (20, RIP.singleton "Y"), (30, RIP.singleton "Z")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "AnonEnumCoords"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "AnonEnumCoords"

instance Show AnonEnumCoords where

  showsPrec = CEnum.shows

instance Read AnonEnumCoords where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.CompatHasField.HasField "unwrapAnonEnumCoords" AnonEnumCoords ty where

  hasField =
    \x0 ->
      ( \y1 -> AnonEnumCoords {unwrapAnonEnumCoords = y1}
      , RIP.getField @"unwrapAnonEnumCoords" x0
      )

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapAnonEnumCoords" (RIP.Ptr AnonEnumCoords) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapAnonEnumCoords")

instance HasCField.HasCField AnonEnumCoords "unwrapAnonEnumCoords" where

  type CFieldType AnonEnumCoords "unwrapAnonEnumCoords" =
    RIP.CUInt

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
  { unwrapA :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

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

deriving via Marshal.EquivStorable A instance RIP.Storable A

deriving via RIP.CUInt instance RIP.Prim A

instance CEnum.CEnum A where

  type CEnumZ A = RIP.CUInt

  toCEnum = A

  fromCEnum = RIP.getField @"unwrapA"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "A1")]

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

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance (ty ~ RIP.CUInt) => RIP.CompatHasField.HasField "unwrapA" A ty where

  hasField =
    \x0 ->
      (\y1 -> A {unwrapA = y1}, RIP.getField @"unwrapA" x0)

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapA" (RIP.Ptr A) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = RIP.CUInt

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
  { unwrapB :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize B

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw B

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw B

deriving via Marshal.EquivStorable B instance RIP.Storable B

{-|

    __See:__ 'set_b_x'

    __C declaration:__ @x@

    __defined at:__ @globals\/untagged.h 27:19@

    __exported by:__ @globals\/untagged.h@
-}
get_b_x ::
     B
  -> RIP.CInt
get_b_x = RIP.getUnionPayload

{-|

    __See:__ 'get_b_x'

-}
set_b_x ::
     RIP.CInt
  -> B
set_b_x = RIP.setUnionPayload

instance (ty ~ RIP.CInt) => RIP.HasField "b_x" (RIP.Ptr B) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"b_x")

instance HasCField.HasCField B "b_x" where

  type CFieldType B "b_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@C@

    __defined at:__ @globals\/untagged.h 30:1@

    __exported by:__ @globals\/untagged.h@
-}
data C = C
  { c_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals\/untagged.h 30:14@

         __exported by:__ @globals\/untagged.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize C where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw C where

  readRaw =
    \ptr0 ->
          pure C
      <*> HasCField.readRaw (RIP.Proxy @"c_x") ptr0

instance Marshal.WriteRaw C where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          C c_x2 ->
            HasCField.writeRaw (RIP.Proxy @"c_x") ptr0 c_x2

deriving via Marshal.EquivStorable C instance RIP.Storable C

instance (ty ~ RIP.CInt) => RIP.CompatHasField.HasField "c_x" C ty where

  hasField =
    \x0 -> (\y1 -> C {c_x = y1}, RIP.getField @"c_x" x0)

instance (ty ~ RIP.CInt) => RIP.HasField "c_x" (RIP.Ptr C) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"c_x")

instance HasCField.HasCField C "c_x" where

  type CFieldType C "c_x" = RIP.CInt

  offset# = \_ -> \_ -> 0
