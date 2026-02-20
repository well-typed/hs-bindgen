{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
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

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct config@

    __defined at:__ @globals\/globals.h 12:8@

    __exported by:__ @globals\/globals.h@
-}
data Config = Config
  { config_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals\/globals.h 13:7@

         __exported by:__ @globals\/globals.h@
    -}
  , config_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals\/globals.h 14:7@

         __exported by:__ @globals\/globals.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Config where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Config where

  readRaw =
    \ptr0 ->
          pure Config
      <*> HasCField.readRaw (RIP.Proxy @"config_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"config_y") ptr0

instance Marshal.WriteRaw Config where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Config config_x2 config_y3 ->
               HasCField.writeRaw (RIP.Proxy @"config_x") ptr0 config_x2
            >> HasCField.writeRaw (RIP.Proxy @"config_y") ptr0 config_y3

deriving via Marshal.EquivStorable Config instance RIP.Storable Config

instance HasCField.HasCField Config "config_x" where

  type CFieldType Config "config_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "config_x" (RIP.Ptr Config) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"config_x")

instance HasCField.HasCField Config "config_y" where

  type CFieldType Config "config_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "config_y" (RIP.Ptr Config) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"config_y")

{-| __C declaration:__ @struct inline_struct@

    __defined at:__ @globals\/globals.h 19:15@

    __exported by:__ @globals\/globals.h@
-}
data Inline_struct = Inline_struct
  { inline_struct_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals\/globals.h 19:35@

         __exported by:__ @globals\/globals.h@
    -}
  , inline_struct_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals\/globals.h 19:42@

         __exported by:__ @globals\/globals.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Inline_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Inline_struct where

  readRaw =
    \ptr0 ->
          pure Inline_struct
      <*> HasCField.readRaw (RIP.Proxy @"inline_struct_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"inline_struct_y") ptr0

instance Marshal.WriteRaw Inline_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Inline_struct inline_struct_x2 inline_struct_y3 ->
               HasCField.writeRaw (RIP.Proxy @"inline_struct_x") ptr0 inline_struct_x2
            >> HasCField.writeRaw (RIP.Proxy @"inline_struct_y") ptr0 inline_struct_y3

deriving via Marshal.EquivStorable Inline_struct instance RIP.Storable Inline_struct

instance HasCField.HasCField Inline_struct "inline_struct_x" where

  type CFieldType Inline_struct "inline_struct_x" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "inline_struct_x" (RIP.Ptr Inline_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"inline_struct_x")

instance HasCField.HasCField Inline_struct "inline_struct_y" where

  type CFieldType Inline_struct "inline_struct_y" =
    RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "inline_struct_y" (RIP.Ptr Inline_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"inline_struct_y")

{-| __C declaration:__ @struct version_t@

    __defined at:__ @globals\/globals.h 406:9@

    __exported by:__ @globals\/globals.h@
-}
data Version_t = Version_t
  { version_t_major :: HsBindgen.Runtime.LibC.Word8
    {- ^ __C declaration:__ @major@

         __defined at:__ @globals\/globals.h 408:12@

         __exported by:__ @globals\/globals.h@
    -}
  , version_t_minor :: HsBindgen.Runtime.LibC.Word16
    {- ^ __C declaration:__ @minor@

         __defined at:__ @globals\/globals.h 409:12@

         __exported by:__ @globals\/globals.h@
    -}
  , version_t_patch :: HsBindgen.Runtime.LibC.Word8
    {- ^ __C declaration:__ @patch@

         __defined at:__ @globals\/globals.h 410:12@

         __exported by:__ @globals\/globals.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Version_t where

  staticSizeOf = \_ -> (6 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Version_t where

  readRaw =
    \ptr0 ->
          pure Version_t
      <*> HasCField.readRaw (RIP.Proxy @"version_t_major") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"version_t_minor") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"version_t_patch") ptr0

instance Marshal.WriteRaw Version_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Version_t version_t_major2 version_t_minor3 version_t_patch4 ->
               HasCField.writeRaw (RIP.Proxy @"version_t_major") ptr0 version_t_major2
            >> HasCField.writeRaw (RIP.Proxy @"version_t_minor") ptr0 version_t_minor3
            >> HasCField.writeRaw (RIP.Proxy @"version_t_patch") ptr0 version_t_patch4

deriving via Marshal.EquivStorable Version_t instance RIP.Storable Version_t

instance HasCField.HasCField Version_t "version_t_major" where

  type CFieldType Version_t "version_t_major" =
    HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) HsBindgen.Runtime.LibC.Word8
         ) => RIP.HasField "version_t_major" (RIP.Ptr Version_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"version_t_major")

instance HasCField.HasCField Version_t "version_t_minor" where

  type CFieldType Version_t "version_t_minor" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 2

instance ( ((~) ty) HsBindgen.Runtime.LibC.Word16
         ) => RIP.HasField "version_t_minor" (RIP.Ptr Version_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"version_t_minor")

instance HasCField.HasCField Version_t "version_t_patch" where

  type CFieldType Version_t "version_t_patch" =
    HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) HsBindgen.Runtime.LibC.Word8
         ) => RIP.HasField "version_t_patch" (RIP.Ptr Version_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"version_t_patch")

{-| __C declaration:__ @struct struct1_t@

    __defined at:__ @globals\/globals.h 413:9@

    __exported by:__ @globals\/globals.h@
-}
data Struct1_t = Struct1_t
  { struct1_t_x :: HsBindgen.Runtime.LibC.Word16
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals\/globals.h 415:13@

         __exported by:__ @globals\/globals.h@
    -}
  , struct1_t_y :: RIP.CBool
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals\/globals.h 416:13@

         __exported by:__ @globals\/globals.h@
    -}
  , struct1_t_version :: Version_t
    {- ^ __C declaration:__ @version@

         __defined at:__ @globals\/globals.h 417:13@

         __exported by:__ @globals\/globals.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct1_t where

  staticSizeOf = \_ -> (10 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Struct1_t where

  readRaw =
    \ptr0 ->
          pure Struct1_t
      <*> HasCField.readRaw (RIP.Proxy @"struct1_t_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"struct1_t_y") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"struct1_t_version") ptr0

instance Marshal.WriteRaw Struct1_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct1_t struct1_t_x2 struct1_t_y3 struct1_t_version4 ->
               HasCField.writeRaw (RIP.Proxy @"struct1_t_x") ptr0 struct1_t_x2
            >> HasCField.writeRaw (RIP.Proxy @"struct1_t_y") ptr0 struct1_t_y3
            >> HasCField.writeRaw (RIP.Proxy @"struct1_t_version") ptr0 struct1_t_version4

deriving via Marshal.EquivStorable Struct1_t instance RIP.Storable Struct1_t

instance HasCField.HasCField Struct1_t "struct1_t_x" where

  type CFieldType Struct1_t "struct1_t_x" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) HsBindgen.Runtime.LibC.Word16
         ) => RIP.HasField "struct1_t_x" (RIP.Ptr Struct1_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"struct1_t_x")

instance HasCField.HasCField Struct1_t "struct1_t_y" where

  type CFieldType Struct1_t "struct1_t_y" = RIP.CBool

  offset# = \_ -> \_ -> 2

instance ( ((~) ty) RIP.CBool
         ) => RIP.HasField "struct1_t_y" (RIP.Ptr Struct1_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"struct1_t_y")

instance HasCField.HasCField Struct1_t "struct1_t_version" where

  type CFieldType Struct1_t "struct1_t_version" =
    Version_t

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) Version_t
         ) => RIP.HasField "struct1_t_version" (RIP.Ptr Struct1_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"struct1_t_version")

{-| __C declaration:__ @struct struct2_t@

    __defined at:__ @globals\/globals.h 420:9@

    __exported by:__ @globals\/globals.h@
-}
data Struct2_t = Struct2_t
  { struct2_t_field1 :: Struct1_t
    {- ^ __C declaration:__ @field1@

         __defined at:__ @globals\/globals.h 422:13@

         __exported by:__ @globals\/globals.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct2_t where

  staticSizeOf = \_ -> (10 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Struct2_t where

  readRaw =
    \ptr0 ->
          pure Struct2_t
      <*> HasCField.readRaw (RIP.Proxy @"struct2_t_field1") ptr0

instance Marshal.WriteRaw Struct2_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2_t struct2_t_field12 ->
            HasCField.writeRaw (RIP.Proxy @"struct2_t_field1") ptr0 struct2_t_field12

deriving via Marshal.EquivStorable Struct2_t instance RIP.Storable Struct2_t

instance HasCField.HasCField Struct2_t "struct2_t_field1" where

  type CFieldType Struct2_t "struct2_t_field1" =
    Struct1_t

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Struct1_t
         ) => RIP.HasField "struct2_t_field1" (RIP.Ptr Struct2_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"struct2_t_field1")

{-| __C declaration:__ @struct \@anonPoint@

    __defined at:__ @globals\/globals.h 438:1@

    __exported by:__ @globals\/globals.h@
-}
data AnonPoint = AnonPoint
  { anonPoint_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals\/globals.h 438:14@

         __exported by:__ @globals\/globals.h@
    -}
  , anonPoint_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals\/globals.h 438:21@

         __exported by:__ @globals\/globals.h@
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

instance HasCField.HasCField AnonPoint "anonPoint_x" where

  type CFieldType AnonPoint "anonPoint_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "anonPoint_x" (RIP.Ptr AnonPoint) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"anonPoint_x")

instance HasCField.HasCField AnonPoint "anonPoint_y" where

  type CFieldType AnonPoint "anonPoint_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "anonPoint_y" (RIP.Ptr AnonPoint) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"anonPoint_y")

{-| __C declaration:__ @struct \@anonPair@

    __defined at:__ @globals\/globals.h 441:1@

    __exported by:__ @globals\/globals.h@
-}
data AnonPair = AnonPair
  { anonPair_a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @globals\/globals.h 441:14@

         __exported by:__ @globals\/globals.h@
    -}
  , anonPair_b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @globals\/globals.h 441:21@

         __exported by:__ @globals\/globals.h@
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

instance HasCField.HasCField AnonPair "anonPair_a" where

  type CFieldType AnonPair "anonPair_a" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "anonPair_a" (RIP.Ptr AnonPair) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"anonPair_a")

instance HasCField.HasCField AnonPair "anonPair_b" where

  type CFieldType AnonPair "anonPair_b" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "anonPair_b" (RIP.Ptr AnonPair) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"anonPair_b")

{-| __C declaration:__ @enum \@anonEnum@

    __defined at:__ @globals\/globals.h 444:1@

    __exported by:__ @globals\/globals.h@
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

  fromCEnum = unwrapAnonEnum

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

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapAnonEnum" (RIP.Ptr AnonEnum) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapAnonEnum")

instance HasCField.HasCField AnonEnum "unwrapAnonEnum" where

  type CFieldType AnonEnum "unwrapAnonEnum" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @VAL_A@

    __defined at:__ @globals\/globals.h 444:8@

    __exported by:__ @globals\/globals.h@
-}
pattern VAL_A :: AnonEnum
pattern VAL_A = AnonEnum 0

{-| __C declaration:__ @VAL_B@

    __defined at:__ @globals\/globals.h 444:19@

    __exported by:__ @globals\/globals.h@
-}
pattern VAL_B :: AnonEnum
pattern VAL_B = AnonEnum 1

{-| __C declaration:__ @enum \@anonEnumCoords@

    __defined at:__ @globals\/globals.h 447:1@

    __exported by:__ @globals\/globals.h@
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

  fromCEnum = unwrapAnonEnumCoords

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

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapAnonEnumCoords" (RIP.Ptr AnonEnumCoords) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapAnonEnumCoords")

instance HasCField.HasCField AnonEnumCoords "unwrapAnonEnumCoords" where

  type CFieldType AnonEnumCoords "unwrapAnonEnumCoords" =
    RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @X@

    __defined at:__ @globals\/globals.h 447:8@

    __exported by:__ @globals\/globals.h@
-}
pattern X :: AnonEnumCoords
pattern X = AnonEnumCoords 10

{-| __C declaration:__ @Y@

    __defined at:__ @globals\/globals.h 447:16@

    __exported by:__ @globals\/globals.h@
-}
pattern Y :: AnonEnumCoords
pattern Y = AnonEnumCoords 20

{-| __C declaration:__ @Z@

    __defined at:__ @globals\/globals.h 447:24@

    __exported by:__ @globals\/globals.h@
-}
pattern Z :: AnonEnumCoords
pattern Z = AnonEnumCoords 30
