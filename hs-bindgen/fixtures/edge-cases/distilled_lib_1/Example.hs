{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
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
import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct another_typedef_struct_t@

    __defined at:__ @edge-cases\/distilled_lib_1.h 9:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
data Another_typedef_struct_t = Another_typedef_struct_t
  { another_typedef_struct_t_foo :: RIP.CInt
    {- ^ __C declaration:__ @foo@

         __defined at:__ @edge-cases\/distilled_lib_1.h 9:22@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  , another_typedef_struct_t_bar :: RIP.CChar
    {- ^ __C declaration:__ @bar@

         __defined at:__ @edge-cases\/distilled_lib_1.h 9:32@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Another_typedef_struct_t where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Another_typedef_struct_t where

  readRaw =
    \ptr0 ->
          pure Another_typedef_struct_t
      <*> HasCField.readRaw (RIP.Proxy @"another_typedef_struct_t_foo") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"another_typedef_struct_t_bar") ptr0

instance Marshal.WriteRaw Another_typedef_struct_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Another_typedef_struct_t
            another_typedef_struct_t_foo2
            another_typedef_struct_t_bar3 ->
                 HasCField.writeRaw (RIP.Proxy @"another_typedef_struct_t_foo") ptr0 another_typedef_struct_t_foo2
              >> HasCField.writeRaw (RIP.Proxy @"another_typedef_struct_t_bar") ptr0 another_typedef_struct_t_bar3

deriving via Marshal.EquivStorable Another_typedef_struct_t instance RIP.Storable Another_typedef_struct_t

instance HasCField.HasCField Another_typedef_struct_t "another_typedef_struct_t_foo" where

  type CFieldType Another_typedef_struct_t "another_typedef_struct_t_foo" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "another_typedef_struct_t_foo" (RIP.Ptr Another_typedef_struct_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"another_typedef_struct_t_foo")

instance HasCField.HasCField Another_typedef_struct_t "another_typedef_struct_t_bar" where

  type CFieldType Another_typedef_struct_t "another_typedef_struct_t_bar" =
    RIP.CChar

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "another_typedef_struct_t_bar" (RIP.Ptr Another_typedef_struct_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"another_typedef_struct_t_bar")

{-| __C declaration:__ @enum another_typedef_enum_e@

    __defined at:__ @edge-cases\/distilled_lib_1.h 10:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
newtype Another_typedef_enum_e = Another_typedef_enum_e
  { unwrapAnother_typedef_enum_e :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize Another_typedef_enum_e where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Another_typedef_enum_e where

  readRaw =
    \ptr0 ->
          pure Another_typedef_enum_e
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Another_typedef_enum_e where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Another_typedef_enum_e unwrapAnother_typedef_enum_e2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapAnother_typedef_enum_e2

deriving via Marshal.EquivStorable Another_typedef_enum_e instance RIP.Storable Another_typedef_enum_e

deriving via RIP.CUInt instance RIP.Prim Another_typedef_enum_e

instance CEnum.CEnum Another_typedef_enum_e where

  type CEnumZ Another_typedef_enum_e = RIP.CUInt

  toCEnum = Another_typedef_enum_e

  fromCEnum = unwrapAnother_typedef_enum_e

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "FOO"), (1, RIP.singleton "BAR")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Another_typedef_enum_e"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Another_typedef_enum_e"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Another_typedef_enum_e where

  minDeclaredValue = FOO

  maxDeclaredValue = BAR

instance Show Another_typedef_enum_e where

  showsPrec = CEnum.shows

instance Read Another_typedef_enum_e where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapAnother_typedef_enum_e" (RIP.Ptr Another_typedef_enum_e) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapAnother_typedef_enum_e")

instance HasCField.HasCField Another_typedef_enum_e "unwrapAnother_typedef_enum_e" where

  type CFieldType Another_typedef_enum_e "unwrapAnother_typedef_enum_e" =
    RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @FOO@

    __defined at:__ @edge-cases\/distilled_lib_1.h 10:16@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
pattern FOO :: Another_typedef_enum_e
pattern FOO = Another_typedef_enum_e 0

{-| __C declaration:__ @BAR@

    __defined at:__ @edge-cases\/distilled_lib_1.h 10:21@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
pattern BAR :: Another_typedef_enum_e
pattern BAR = Another_typedef_enum_e 1

{-| __C declaration:__ @A@

    __defined at:__ @edge-cases\/distilled_lib_1.h 11:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
a :: RIP.CInt
a = (5 :: RIP.CInt)

{-| __C declaration:__ @B@

    __defined at:__ @edge-cases\/distilled_lib_1.h 12:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
b :: RIP.CInt
b = (3 :: RIP.CInt)

{-| __C declaration:__ @SOME_DEFINED_CONSTANT@

    __defined at:__ @edge-cases\/distilled_lib_1.h 13:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
sOME_DEFINED_CONSTANT :: RIP.CInt
sOME_DEFINED_CONSTANT = (4 :: RIP.CInt)

{-| __C declaration:__ @a_type_t@

    __defined at:__ @edge-cases\/distilled_lib_1.h 14:13@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
newtype A_type_t = A_type_t
  { unwrapA_type_t :: RIP.CInt
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

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapA_type_t" (RIP.Ptr A_type_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapA_type_t")

instance HasCField.HasCField A_type_t "unwrapA_type_t" where

  type CFieldType A_type_t "unwrapA_type_t" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @var_t@

    __defined at:__ @edge-cases\/distilled_lib_1.h 15:13@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
newtype Var_t = Var_t
  { unwrapVar_t :: RIP.CInt
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

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapVar_t" (RIP.Ptr Var_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapVar_t")

instance HasCField.HasCField Var_t "unwrapVar_t" where

  type CFieldType Var_t "unwrapVar_t" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct a_typedef_struct@

    __defined at:__ @edge-cases\/distilled_lib_1.h 35:16@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
data A_typedef_struct_t = A_typedef_struct_t
  { a_typedef_struct_t_field_0 :: RIP.CBool
    {- ^ __C declaration:__ @field_0@

         __defined at:__ @edge-cases\/distilled_lib_1.h 37:31@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_1 :: HsBindgen.Runtime.LibC.Word8
    {- ^ __C declaration:__ @field_1@

         __defined at:__ @edge-cases\/distilled_lib_1.h 38:31@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_2 :: HsBindgen.Runtime.LibC.Word16
    {- ^ __C declaration:__ @field_2@

         __defined at:__ @edge-cases\/distilled_lib_1.h 39:31@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_3 :: HsBindgen.Runtime.LibC.Word32
    {- ^ __C declaration:__ @field_3@

         __defined at:__ @edge-cases\/distilled_lib_1.h 40:31@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_4 :: Another_typedef_struct_t
    {- ^ __C declaration:__ @field_4@

         __defined at:__ @edge-cases\/distilled_lib_1.h 41:31@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_5 :: RIP.Ptr Another_typedef_struct_t
    {- ^ __C declaration:__ @field_5@

         __defined at:__ @edge-cases\/distilled_lib_1.h 42:31@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_6 :: RIP.Ptr RIP.Void
    {- ^ __C declaration:__ @field_6@

         __defined at:__ @edge-cases\/distilled_lib_1.h 43:31@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_7 :: (CA.ConstantArray 7) HsBindgen.Runtime.LibC.Word32
    {- ^ __C declaration:__ @field_7@

         __defined at:__ @edge-cases\/distilled_lib_1.h 44:31@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_8 :: Another_typedef_enum_e
    {- ^ __C declaration:__ @field_8@

         __defined at:__ @edge-cases\/distilled_lib_1.h 45:31@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_9 :: (CA.ConstantArray 4) Another_typedef_enum_e
    {- ^ __C declaration:__ @field_9@

         __defined at:__ @edge-cases\/distilled_lib_1.h 46:31@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_10 :: (CA.ConstantArray 5) ((CA.ConstantArray 3) Another_typedef_enum_e)
    {- ^ __C declaration:__ @field_10@

         __defined at:__ @edge-cases\/distilled_lib_1.h 47:31@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize A_typedef_struct_t where

  staticSizeOf = \_ -> (140 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw A_typedef_struct_t where

  readRaw =
    \ptr0 ->
          pure A_typedef_struct_t
      <*> HasCField.readRaw (RIP.Proxy @"a_typedef_struct_t_field_0") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"a_typedef_struct_t_field_1") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"a_typedef_struct_t_field_2") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"a_typedef_struct_t_field_3") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"a_typedef_struct_t_field_4") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"a_typedef_struct_t_field_5") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"a_typedef_struct_t_field_6") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"a_typedef_struct_t_field_7") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"a_typedef_struct_t_field_8") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"a_typedef_struct_t_field_9") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"a_typedef_struct_t_field_10") ptr0

instance Marshal.WriteRaw A_typedef_struct_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A_typedef_struct_t
            a_typedef_struct_t_field_02
            a_typedef_struct_t_field_13
            a_typedef_struct_t_field_24
            a_typedef_struct_t_field_35
            a_typedef_struct_t_field_46
            a_typedef_struct_t_field_57
            a_typedef_struct_t_field_68
            a_typedef_struct_t_field_79
            a_typedef_struct_t_field_810
            a_typedef_struct_t_field_911
            a_typedef_struct_t_field_1012 ->
                 HasCField.writeRaw (RIP.Proxy @"a_typedef_struct_t_field_0") ptr0 a_typedef_struct_t_field_02
              >> HasCField.writeRaw (RIP.Proxy @"a_typedef_struct_t_field_1") ptr0 a_typedef_struct_t_field_13
              >> HasCField.writeRaw (RIP.Proxy @"a_typedef_struct_t_field_2") ptr0 a_typedef_struct_t_field_24
              >> HasCField.writeRaw (RIP.Proxy @"a_typedef_struct_t_field_3") ptr0 a_typedef_struct_t_field_35
              >> HasCField.writeRaw (RIP.Proxy @"a_typedef_struct_t_field_4") ptr0 a_typedef_struct_t_field_46
              >> HasCField.writeRaw (RIP.Proxy @"a_typedef_struct_t_field_5") ptr0 a_typedef_struct_t_field_57
              >> HasCField.writeRaw (RIP.Proxy @"a_typedef_struct_t_field_6") ptr0 a_typedef_struct_t_field_68
              >> HasCField.writeRaw (RIP.Proxy @"a_typedef_struct_t_field_7") ptr0 a_typedef_struct_t_field_79
              >> HasCField.writeRaw (RIP.Proxy @"a_typedef_struct_t_field_8") ptr0 a_typedef_struct_t_field_810
              >> HasCField.writeRaw (RIP.Proxy @"a_typedef_struct_t_field_9") ptr0 a_typedef_struct_t_field_911
              >> HasCField.writeRaw (RIP.Proxy @"a_typedef_struct_t_field_10") ptr0 a_typedef_struct_t_field_1012

deriving via Marshal.EquivStorable A_typedef_struct_t instance RIP.Storable A_typedef_struct_t

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_0" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_0" =
    RIP.CBool

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CBool
         ) => RIP.HasField "a_typedef_struct_t_field_0" (RIP.Ptr A_typedef_struct_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"a_typedef_struct_t_field_0")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_1" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_1" =
    HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 1

instance ( ((~) ty) HsBindgen.Runtime.LibC.Word8
         ) => RIP.HasField "a_typedef_struct_t_field_1" (RIP.Ptr A_typedef_struct_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"a_typedef_struct_t_field_1")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_2" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_2" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 2

instance ( ((~) ty) HsBindgen.Runtime.LibC.Word16
         ) => RIP.HasField "a_typedef_struct_t_field_2" (RIP.Ptr A_typedef_struct_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"a_typedef_struct_t_field_2")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_3" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_3" =
    HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) HsBindgen.Runtime.LibC.Word32
         ) => RIP.HasField "a_typedef_struct_t_field_3" (RIP.Ptr A_typedef_struct_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"a_typedef_struct_t_field_3")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_4" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_4" =
    Another_typedef_struct_t

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) Another_typedef_struct_t
         ) => RIP.HasField "a_typedef_struct_t_field_4" (RIP.Ptr A_typedef_struct_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"a_typedef_struct_t_field_4")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_5" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_5" =
    RIP.Ptr Another_typedef_struct_t

  offset# = \_ -> \_ -> 16

instance ( ((~) ty) (RIP.Ptr Another_typedef_struct_t)
         ) => RIP.HasField "a_typedef_struct_t_field_5" (RIP.Ptr A_typedef_struct_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"a_typedef_struct_t_field_5")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_6" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_6" =
    RIP.Ptr RIP.Void

  offset# = \_ -> \_ -> 24

instance ( ((~) ty) (RIP.Ptr RIP.Void)
         ) => RIP.HasField "a_typedef_struct_t_field_6" (RIP.Ptr A_typedef_struct_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"a_typedef_struct_t_field_6")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_7" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_7" =
    (CA.ConstantArray 7) HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 32

instance ( ((~) ty) ((CA.ConstantArray 7) HsBindgen.Runtime.LibC.Word32)
         ) => RIP.HasField "a_typedef_struct_t_field_7" (RIP.Ptr A_typedef_struct_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"a_typedef_struct_t_field_7")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_8" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_8" =
    Another_typedef_enum_e

  offset# = \_ -> \_ -> 60

instance ( ((~) ty) Another_typedef_enum_e
         ) => RIP.HasField "a_typedef_struct_t_field_8" (RIP.Ptr A_typedef_struct_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"a_typedef_struct_t_field_8")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_9" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_9" =
    (CA.ConstantArray 4) Another_typedef_enum_e

  offset# = \_ -> \_ -> 64

instance ( ((~) ty) ((CA.ConstantArray 4) Another_typedef_enum_e)
         ) => RIP.HasField "a_typedef_struct_t_field_9" (RIP.Ptr A_typedef_struct_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"a_typedef_struct_t_field_9")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_10" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_10" =
    (CA.ConstantArray 5) ((CA.ConstantArray 3) Another_typedef_enum_e)

  offset# = \_ -> \_ -> 80

instance ( ((~) ty) ((CA.ConstantArray 5) ((CA.ConstantArray 3) Another_typedef_enum_e))
         ) => RIP.HasField "a_typedef_struct_t_field_10" (RIP.Ptr A_typedef_struct_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"a_typedef_struct_t_field_10")

{-| __C declaration:__ @A_DEFINE_0@

    __defined at:__ @edge-cases\/distilled_lib_1.h 53:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
a_DEFINE_0 :: RIP.CInt
a_DEFINE_0 = (0 :: RIP.CInt)

{-| __C declaration:__ @A_DEFINE_1@

    __defined at:__ @edge-cases\/distilled_lib_1.h 54:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
a_DEFINE_1 :: RIP.CUInt
a_DEFINE_1 = (20560 :: RIP.CUInt)

{-| __C declaration:__ @A_DEFINE_2@

    __defined at:__ @edge-cases\/distilled_lib_1.h 55:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
a_DEFINE_2 :: RIP.CInt
a_DEFINE_2 = (2 :: RIP.CInt)

{-| __C declaration:__ @TWO_ARGS@

    __defined at:__ @edge-cases\/distilled_lib_1.h 56:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
tWO_ARGS :: ((,) RIP.CInt) RIP.CInt
tWO_ARGS =
  (,) (13398 :: RIP.CInt) (30874 :: RIP.CInt)

{-| __C declaration:__ @enum a_typedef_enum_e@

    __defined at:__ @edge-cases\/distilled_lib_1.h 61:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
newtype A_typedef_enum_e = A_typedef_enum_e
  { unwrapA_typedef_enum_e :: RIP.CUChar
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize A_typedef_enum_e where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw A_typedef_enum_e where

  readRaw =
    \ptr0 ->
          pure A_typedef_enum_e
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw A_typedef_enum_e where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A_typedef_enum_e unwrapA_typedef_enum_e2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapA_typedef_enum_e2

deriving via Marshal.EquivStorable A_typedef_enum_e instance RIP.Storable A_typedef_enum_e

deriving via RIP.CUChar instance RIP.Prim A_typedef_enum_e

instance CEnum.CEnum A_typedef_enum_e where

  type CEnumZ A_typedef_enum_e = RIP.CUChar

  toCEnum = A_typedef_enum_e

  fromCEnum = unwrapA_typedef_enum_e

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [ (0, RIP.singleton "ENUM_CASE_0")
                                   , (1, RIP.singleton "ENUM_CASE_1")
                                   , (2, RIP.singleton "ENUM_CASE_2")
                                   , (3, RIP.singleton "ENUM_CASE_3")
                                   ]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "A_typedef_enum_e"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "A_typedef_enum_e"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum A_typedef_enum_e where

  minDeclaredValue = ENUM_CASE_0

  maxDeclaredValue = ENUM_CASE_3

instance Show A_typedef_enum_e where

  showsPrec = CEnum.shows

instance Read A_typedef_enum_e where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUChar
         ) => RIP.HasField "unwrapA_typedef_enum_e" (RIP.Ptr A_typedef_enum_e) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapA_typedef_enum_e")

instance HasCField.HasCField A_typedef_enum_e "unwrapA_typedef_enum_e" where

  type CFieldType A_typedef_enum_e "unwrapA_typedef_enum_e" =
    RIP.CUChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @ENUM_CASE_0@

    __defined at:__ @edge-cases\/distilled_lib_1.h 63:3@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
pattern ENUM_CASE_0 :: A_typedef_enum_e
pattern ENUM_CASE_0 = A_typedef_enum_e 0

{-| __C declaration:__ @ENUM_CASE_1@

    __defined at:__ @edge-cases\/distilled_lib_1.h 64:3@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
pattern ENUM_CASE_1 :: A_typedef_enum_e
pattern ENUM_CASE_1 = A_typedef_enum_e 1

{-| __C declaration:__ @ENUM_CASE_2@

    __defined at:__ @edge-cases\/distilled_lib_1.h 65:3@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
pattern ENUM_CASE_2 :: A_typedef_enum_e
pattern ENUM_CASE_2 = A_typedef_enum_e 2

{-| __C declaration:__ @ENUM_CASE_3@

    __defined at:__ @edge-cases\/distilled_lib_1.h 66:3@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
pattern ENUM_CASE_3 :: A_typedef_enum_e
pattern ENUM_CASE_3 = A_typedef_enum_e 3

{-| Auxiliary type used by 'Callback_t'

__C declaration:__ @callback_t@

__defined at:__ @edge-cases\/distilled_lib_1.h 77:19@

__exported by:__ @edge-cases\/distilled_lib_1.h@
-}
newtype Callback_t_Aux = Callback_t_Aux
  { unwrapCallback_t_Aux :: (RIP.Ptr RIP.Void) -> HsBindgen.Runtime.LibC.Word32 -> IO HsBindgen.Runtime.LibC.Word32
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_b6b6922e35047658_base ::
     ((RIP.Ptr RIP.Void) -> RIP.Word32 -> IO RIP.Word32)
  -> IO (RIP.FunPtr ((RIP.Ptr RIP.Void) -> RIP.Word32 -> IO RIP.Word32))

-- __unique:__ @toCallback_t_Aux@
hs_bindgen_b6b6922e35047658 ::
     Callback_t_Aux
  -> IO (RIP.FunPtr Callback_t_Aux)
hs_bindgen_b6b6922e35047658 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_b6b6922e35047658_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_d6debb4b8d5bb869_base ::
     RIP.FunPtr ((RIP.Ptr RIP.Void) -> RIP.Word32 -> IO RIP.Word32)
  -> (RIP.Ptr RIP.Void) -> RIP.Word32 -> IO RIP.Word32

-- __unique:__ @fromCallback_t_Aux@
hs_bindgen_d6debb4b8d5bb869 ::
     RIP.FunPtr Callback_t_Aux
  -> Callback_t_Aux
hs_bindgen_d6debb4b8d5bb869 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_d6debb4b8d5bb869_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Callback_t_Aux where

  toFunPtr = hs_bindgen_b6b6922e35047658

instance RIP.FromFunPtr Callback_t_Aux where

  fromFunPtr = hs_bindgen_d6debb4b8d5bb869

instance ( ((~) ty) ((RIP.Ptr RIP.Void) -> HsBindgen.Runtime.LibC.Word32 -> IO HsBindgen.Runtime.LibC.Word32)
         ) => RIP.HasField "unwrapCallback_t_Aux" (RIP.Ptr Callback_t_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapCallback_t_Aux")

instance HasCField.HasCField Callback_t_Aux "unwrapCallback_t_Aux" where

  type CFieldType Callback_t_Aux "unwrapCallback_t_Aux" =
    (RIP.Ptr RIP.Void) -> HsBindgen.Runtime.LibC.Word32 -> IO HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @callback_t@

    __defined at:__ @edge-cases\/distilled_lib_1.h 77:19@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
newtype Callback_t = Callback_t
  { unwrapCallback_t :: RIP.FunPtr Callback_t_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr Callback_t_Aux)
         ) => RIP.HasField "unwrapCallback_t" (RIP.Ptr Callback_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapCallback_t")

instance HasCField.HasCField Callback_t "unwrapCallback_t" where

  type CFieldType Callback_t "unwrapCallback_t" =
    RIP.FunPtr Callback_t_Aux

  offset# = \_ -> \_ -> 0
