{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
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
    ( Example.Another_typedef_struct_t(..)
    , Example.Another_typedef_enum_e(..)
    , pattern Example.FOO
    , pattern Example.BAR
    , Example.a
    , Example.b
    , Example.sOME_DEFINED_CONSTANT
    , Example.A_type_t(..)
    , Example.Var_t(..)
    , Example.A_typedef_struct_t(..)
    , Example.a_DEFINE_0
    , Example.a_DEFINE_1
    , Example.a_DEFINE_2
    , Example.tWO_ARGS
    , Example.A_typedef_enum_e(..)
    , pattern Example.ENUM_CASE_0
    , pattern Example.ENUM_CASE_1
    , pattern Example.ENUM_CASE_2
    , pattern Example.ENUM_CASE_3
    , Example.Callback_t_Aux(..)
    , Example.Callback_t(..)
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct another_typedef_struct_t@

    __defined at:__ @edge-cases\/distilled_lib_1.h 9:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
data Another_typedef_struct_t = Another_typedef_struct_t
  { another_typedef_struct_t_foo :: BG.CInt
    {- ^ __C declaration:__ @foo@

         __defined at:__ @edge-cases\/distilled_lib_1.h 9:22@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  , another_typedef_struct_t_bar :: BG.CChar
    {- ^ __C declaration:__ @bar@

         __defined at:__ @edge-cases\/distilled_lib_1.h 9:32@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Another_typedef_struct_t where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Another_typedef_struct_t where

  readRaw =
    \ptr0 ->
          pure Another_typedef_struct_t
      <*> HasCField.readRaw (BG.Proxy @"another_typedef_struct_t_foo") ptr0
      <*> HasCField.readRaw (BG.Proxy @"another_typedef_struct_t_bar") ptr0

instance Marshal.WriteRaw Another_typedef_struct_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Another_typedef_struct_t
            another_typedef_struct_t_foo2
            another_typedef_struct_t_bar3 ->
                 HasCField.writeRaw (BG.Proxy @"another_typedef_struct_t_foo") ptr0 another_typedef_struct_t_foo2
              >> HasCField.writeRaw (BG.Proxy @"another_typedef_struct_t_bar") ptr0 another_typedef_struct_t_bar3

deriving via Marshal.EquivStorable Another_typedef_struct_t instance BG.Storable Another_typedef_struct_t

{-| __C declaration:__ @foo@

    __defined at:__ @edge-cases\/distilled_lib_1.h 9:22@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "another_typedef_struct_t_foo" Another_typedef_struct_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Another_typedef_struct_t { another_typedef_struct_t_foo = y1
                                   , another_typedef_struct_t_bar = BG.getField @"another_typedef_struct_t_bar" x0
                                   }
      , BG.getField @"another_typedef_struct_t_foo" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "another_typedef_struct_t_foo" (BG.Ptr Another_typedef_struct_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"another_typedef_struct_t_foo")

instance HasCField.HasCField Another_typedef_struct_t "another_typedef_struct_t_foo" where

  type CFieldType Another_typedef_struct_t "another_typedef_struct_t_foo" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @bar@

    __defined at:__ @edge-cases\/distilled_lib_1.h 9:32@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
instance ( ty ~ BG.CChar
         ) => BG.CompatHasField.HasField "another_typedef_struct_t_bar" Another_typedef_struct_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Another_typedef_struct_t { another_typedef_struct_t_bar = y1
                                   , another_typedef_struct_t_foo = BG.getField @"another_typedef_struct_t_foo" x0
                                   }
      , BG.getField @"another_typedef_struct_t_bar" x0
      )

instance ( ty ~ BG.CChar
         ) => BG.HasField "another_typedef_struct_t_bar" (BG.Ptr Another_typedef_struct_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"another_typedef_struct_t_bar")

instance HasCField.HasCField Another_typedef_struct_t "another_typedef_struct_t_bar" where

  type CFieldType Another_typedef_struct_t "another_typedef_struct_t_bar" =
    BG.CChar

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @enum another_typedef_enum_e@

    __defined at:__ @edge-cases\/distilled_lib_1.h 10:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
newtype Another_typedef_enum_e = Another_typedef_enum_e
  { unwrapAnother_typedef_enum_e :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

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

deriving via Marshal.EquivStorable Another_typedef_enum_e instance BG.Storable Another_typedef_enum_e

deriving via BG.CUInt instance BG.Prim Another_typedef_enum_e

instance CEnum.CEnum Another_typedef_enum_e where

  type CEnumZ Another_typedef_enum_e = BG.CUInt

  toCEnum = Another_typedef_enum_e

  fromCEnum =
    BG.getField @"unwrapAnother_typedef_enum_e"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "FOO"), (1, BG.singleton "BAR")]

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

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapAnother_typedef_enum_e" Another_typedef_enum_e ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Another_typedef_enum_e {unwrapAnother_typedef_enum_e = y1}
      , BG.getField @"unwrapAnother_typedef_enum_e" x0
      )

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapAnother_typedef_enum_e" (BG.Ptr Another_typedef_enum_e) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapAnother_typedef_enum_e")

instance HasCField.HasCField Another_typedef_enum_e "unwrapAnother_typedef_enum_e" where

  type CFieldType Another_typedef_enum_e "unwrapAnother_typedef_enum_e" =
    BG.CUInt

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

{-| __C declaration:__ @macro A@

    __defined at:__ @edge-cases\/distilled_lib_1.h 11:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
a :: BG.CInt
a = (5 :: BG.CInt)

{-| __C declaration:__ @macro B@

    __defined at:__ @edge-cases\/distilled_lib_1.h 12:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
b :: BG.CInt
b = (3 :: BG.CInt)

{-| __C declaration:__ @macro SOME_DEFINED_CONSTANT@

    __defined at:__ @edge-cases\/distilled_lib_1.h 13:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
sOME_DEFINED_CONSTANT :: BG.CInt
sOME_DEFINED_CONSTANT = (4 :: BG.CInt)

{-| __C declaration:__ @a_type_t@

    __defined at:__ @edge-cases\/distilled_lib_1.h 14:13@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
newtype A_type_t = A_type_t
  { unwrapA_type_t :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapA_type_t" A_type_t ty where

  hasField =
    \x0 ->
      (\y1 ->
         A_type_t {unwrapA_type_t = y1}, BG.getField @"unwrapA_type_t" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapA_type_t" (BG.Ptr A_type_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapA_type_t")

instance HasCField.HasCField A_type_t "unwrapA_type_t" where

  type CFieldType A_type_t "unwrapA_type_t" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @var_t@

    __defined at:__ @edge-cases\/distilled_lib_1.h 15:13@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
newtype Var_t = Var_t
  { unwrapVar_t :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapVar_t" Var_t ty where

  hasField =
    \x0 ->
      (\y1 ->
         Var_t {unwrapVar_t = y1}, BG.getField @"unwrapVar_t" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapVar_t" (BG.Ptr Var_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapVar_t")

instance HasCField.HasCField Var_t "unwrapVar_t" where

  type CFieldType Var_t "unwrapVar_t" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct a_typedef_struct@

    __defined at:__ @edge-cases\/distilled_lib_1.h 35:16@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
data A_typedef_struct_t = A_typedef_struct_t
  { a_typedef_struct_t_field_0 :: BG.CBool
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
  , a_typedef_struct_t_field_5 :: BG.Ptr Another_typedef_struct_t
    {- ^ __C declaration:__ @field_5@

         __defined at:__ @edge-cases\/distilled_lib_1.h 42:31@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_6 :: BG.Ptr BG.Void
    {- ^ __C declaration:__ @field_6@

         __defined at:__ @edge-cases\/distilled_lib_1.h 43:31@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_7 :: CA.ConstantArray 7 HsBindgen.Runtime.LibC.Word32
    {- ^ __C declaration:__ @field_7@

         __defined at:__ @edge-cases\/distilled_lib_1.h 44:31@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_8 :: Another_typedef_enum_e
    {- ^ __C declaration:__ @field_8@

         __defined at:__ @edge-cases\/distilled_lib_1.h 45:31@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_9 :: CA.ConstantArray 4 Another_typedef_enum_e
    {- ^ __C declaration:__ @field_9@

         __defined at:__ @edge-cases\/distilled_lib_1.h 46:31@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_10 :: CA.ConstantArray 5 (CA.ConstantArray 3 Another_typedef_enum_e)
    {- ^ __C declaration:__ @field_10@

         __defined at:__ @edge-cases\/distilled_lib_1.h 47:31@

         __exported by:__ @edge-cases\/distilled_lib_1.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize A_typedef_struct_t where

  staticSizeOf = \_ -> (140 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw A_typedef_struct_t where

  readRaw =
    \ptr0 ->
          pure A_typedef_struct_t
      <*> HasCField.readRaw (BG.Proxy @"a_typedef_struct_t_field_0") ptr0
      <*> HasCField.readRaw (BG.Proxy @"a_typedef_struct_t_field_1") ptr0
      <*> HasCField.readRaw (BG.Proxy @"a_typedef_struct_t_field_2") ptr0
      <*> HasCField.readRaw (BG.Proxy @"a_typedef_struct_t_field_3") ptr0
      <*> HasCField.readRaw (BG.Proxy @"a_typedef_struct_t_field_4") ptr0
      <*> HasCField.readRaw (BG.Proxy @"a_typedef_struct_t_field_5") ptr0
      <*> HasCField.readRaw (BG.Proxy @"a_typedef_struct_t_field_6") ptr0
      <*> HasCField.readRaw (BG.Proxy @"a_typedef_struct_t_field_7") ptr0
      <*> HasCField.readRaw (BG.Proxy @"a_typedef_struct_t_field_8") ptr0
      <*> HasCField.readRaw (BG.Proxy @"a_typedef_struct_t_field_9") ptr0
      <*> HasCField.readRaw (BG.Proxy @"a_typedef_struct_t_field_10") ptr0

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
                 HasCField.writeRaw (BG.Proxy @"a_typedef_struct_t_field_0") ptr0 a_typedef_struct_t_field_02
              >> HasCField.writeRaw (BG.Proxy @"a_typedef_struct_t_field_1") ptr0 a_typedef_struct_t_field_13
              >> HasCField.writeRaw (BG.Proxy @"a_typedef_struct_t_field_2") ptr0 a_typedef_struct_t_field_24
              >> HasCField.writeRaw (BG.Proxy @"a_typedef_struct_t_field_3") ptr0 a_typedef_struct_t_field_35
              >> HasCField.writeRaw (BG.Proxy @"a_typedef_struct_t_field_4") ptr0 a_typedef_struct_t_field_46
              >> HasCField.writeRaw (BG.Proxy @"a_typedef_struct_t_field_5") ptr0 a_typedef_struct_t_field_57
              >> HasCField.writeRaw (BG.Proxy @"a_typedef_struct_t_field_6") ptr0 a_typedef_struct_t_field_68
              >> HasCField.writeRaw (BG.Proxy @"a_typedef_struct_t_field_7") ptr0 a_typedef_struct_t_field_79
              >> HasCField.writeRaw (BG.Proxy @"a_typedef_struct_t_field_8") ptr0 a_typedef_struct_t_field_810
              >> HasCField.writeRaw (BG.Proxy @"a_typedef_struct_t_field_9") ptr0 a_typedef_struct_t_field_911
              >> HasCField.writeRaw (BG.Proxy @"a_typedef_struct_t_field_10") ptr0 a_typedef_struct_t_field_1012

deriving via Marshal.EquivStorable A_typedef_struct_t instance BG.Storable A_typedef_struct_t

{-| __C declaration:__ @field_0@

    __defined at:__ @edge-cases\/distilled_lib_1.h 37:31@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
instance ( ty ~ BG.CBool
         ) => BG.CompatHasField.HasField "a_typedef_struct_t_field_0" A_typedef_struct_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A_typedef_struct_t { a_typedef_struct_t_field_0 = y1
                             , a_typedef_struct_t_field_1 = BG.getField @"a_typedef_struct_t_field_1" x0
                             , a_typedef_struct_t_field_2 = BG.getField @"a_typedef_struct_t_field_2" x0
                             , a_typedef_struct_t_field_3 = BG.getField @"a_typedef_struct_t_field_3" x0
                             , a_typedef_struct_t_field_4 = BG.getField @"a_typedef_struct_t_field_4" x0
                             , a_typedef_struct_t_field_5 = BG.getField @"a_typedef_struct_t_field_5" x0
                             , a_typedef_struct_t_field_6 = BG.getField @"a_typedef_struct_t_field_6" x0
                             , a_typedef_struct_t_field_7 = BG.getField @"a_typedef_struct_t_field_7" x0
                             , a_typedef_struct_t_field_8 = BG.getField @"a_typedef_struct_t_field_8" x0
                             , a_typedef_struct_t_field_9 = BG.getField @"a_typedef_struct_t_field_9" x0
                             , a_typedef_struct_t_field_10 = BG.getField @"a_typedef_struct_t_field_10" x0
                             }
      , BG.getField @"a_typedef_struct_t_field_0" x0
      )

instance ( ty ~ BG.CBool
         ) => BG.HasField "a_typedef_struct_t_field_0" (BG.Ptr A_typedef_struct_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"a_typedef_struct_t_field_0")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_0" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_0" =
    BG.CBool

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @field_1@

    __defined at:__ @edge-cases\/distilled_lib_1.h 38:31@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
instance ( ty ~ HsBindgen.Runtime.LibC.Word8
         ) => BG.CompatHasField.HasField "a_typedef_struct_t_field_1" A_typedef_struct_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A_typedef_struct_t { a_typedef_struct_t_field_1 = y1
                             , a_typedef_struct_t_field_0 = BG.getField @"a_typedef_struct_t_field_0" x0
                             , a_typedef_struct_t_field_2 = BG.getField @"a_typedef_struct_t_field_2" x0
                             , a_typedef_struct_t_field_3 = BG.getField @"a_typedef_struct_t_field_3" x0
                             , a_typedef_struct_t_field_4 = BG.getField @"a_typedef_struct_t_field_4" x0
                             , a_typedef_struct_t_field_5 = BG.getField @"a_typedef_struct_t_field_5" x0
                             , a_typedef_struct_t_field_6 = BG.getField @"a_typedef_struct_t_field_6" x0
                             , a_typedef_struct_t_field_7 = BG.getField @"a_typedef_struct_t_field_7" x0
                             , a_typedef_struct_t_field_8 = BG.getField @"a_typedef_struct_t_field_8" x0
                             , a_typedef_struct_t_field_9 = BG.getField @"a_typedef_struct_t_field_9" x0
                             , a_typedef_struct_t_field_10 = BG.getField @"a_typedef_struct_t_field_10" x0
                             }
      , BG.getField @"a_typedef_struct_t_field_1" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word8
         ) => BG.HasField "a_typedef_struct_t_field_1" (BG.Ptr A_typedef_struct_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"a_typedef_struct_t_field_1")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_1" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_1" =
    HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 1

{-| __C declaration:__ @field_2@

    __defined at:__ @edge-cases\/distilled_lib_1.h 39:31@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
instance ( ty ~ HsBindgen.Runtime.LibC.Word16
         ) => BG.CompatHasField.HasField "a_typedef_struct_t_field_2" A_typedef_struct_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A_typedef_struct_t { a_typedef_struct_t_field_2 = y1
                             , a_typedef_struct_t_field_0 = BG.getField @"a_typedef_struct_t_field_0" x0
                             , a_typedef_struct_t_field_1 = BG.getField @"a_typedef_struct_t_field_1" x0
                             , a_typedef_struct_t_field_3 = BG.getField @"a_typedef_struct_t_field_3" x0
                             , a_typedef_struct_t_field_4 = BG.getField @"a_typedef_struct_t_field_4" x0
                             , a_typedef_struct_t_field_5 = BG.getField @"a_typedef_struct_t_field_5" x0
                             , a_typedef_struct_t_field_6 = BG.getField @"a_typedef_struct_t_field_6" x0
                             , a_typedef_struct_t_field_7 = BG.getField @"a_typedef_struct_t_field_7" x0
                             , a_typedef_struct_t_field_8 = BG.getField @"a_typedef_struct_t_field_8" x0
                             , a_typedef_struct_t_field_9 = BG.getField @"a_typedef_struct_t_field_9" x0
                             , a_typedef_struct_t_field_10 = BG.getField @"a_typedef_struct_t_field_10" x0
                             }
      , BG.getField @"a_typedef_struct_t_field_2" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word16
         ) => BG.HasField "a_typedef_struct_t_field_2" (BG.Ptr A_typedef_struct_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"a_typedef_struct_t_field_2")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_2" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_2" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 2

{-| __C declaration:__ @field_3@

    __defined at:__ @edge-cases\/distilled_lib_1.h 40:31@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
instance ( ty ~ HsBindgen.Runtime.LibC.Word32
         ) => BG.CompatHasField.HasField "a_typedef_struct_t_field_3" A_typedef_struct_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A_typedef_struct_t { a_typedef_struct_t_field_3 = y1
                             , a_typedef_struct_t_field_0 = BG.getField @"a_typedef_struct_t_field_0" x0
                             , a_typedef_struct_t_field_1 = BG.getField @"a_typedef_struct_t_field_1" x0
                             , a_typedef_struct_t_field_2 = BG.getField @"a_typedef_struct_t_field_2" x0
                             , a_typedef_struct_t_field_4 = BG.getField @"a_typedef_struct_t_field_4" x0
                             , a_typedef_struct_t_field_5 = BG.getField @"a_typedef_struct_t_field_5" x0
                             , a_typedef_struct_t_field_6 = BG.getField @"a_typedef_struct_t_field_6" x0
                             , a_typedef_struct_t_field_7 = BG.getField @"a_typedef_struct_t_field_7" x0
                             , a_typedef_struct_t_field_8 = BG.getField @"a_typedef_struct_t_field_8" x0
                             , a_typedef_struct_t_field_9 = BG.getField @"a_typedef_struct_t_field_9" x0
                             , a_typedef_struct_t_field_10 = BG.getField @"a_typedef_struct_t_field_10" x0
                             }
      , BG.getField @"a_typedef_struct_t_field_3" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word32
         ) => BG.HasField "a_typedef_struct_t_field_3" (BG.Ptr A_typedef_struct_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"a_typedef_struct_t_field_3")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_3" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_3" =
    HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @field_4@

    __defined at:__ @edge-cases\/distilled_lib_1.h 41:31@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
instance ( ty ~ Another_typedef_struct_t
         ) => BG.CompatHasField.HasField "a_typedef_struct_t_field_4" A_typedef_struct_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A_typedef_struct_t { a_typedef_struct_t_field_4 = y1
                             , a_typedef_struct_t_field_0 = BG.getField @"a_typedef_struct_t_field_0" x0
                             , a_typedef_struct_t_field_1 = BG.getField @"a_typedef_struct_t_field_1" x0
                             , a_typedef_struct_t_field_2 = BG.getField @"a_typedef_struct_t_field_2" x0
                             , a_typedef_struct_t_field_3 = BG.getField @"a_typedef_struct_t_field_3" x0
                             , a_typedef_struct_t_field_5 = BG.getField @"a_typedef_struct_t_field_5" x0
                             , a_typedef_struct_t_field_6 = BG.getField @"a_typedef_struct_t_field_6" x0
                             , a_typedef_struct_t_field_7 = BG.getField @"a_typedef_struct_t_field_7" x0
                             , a_typedef_struct_t_field_8 = BG.getField @"a_typedef_struct_t_field_8" x0
                             , a_typedef_struct_t_field_9 = BG.getField @"a_typedef_struct_t_field_9" x0
                             , a_typedef_struct_t_field_10 = BG.getField @"a_typedef_struct_t_field_10" x0
                             }
      , BG.getField @"a_typedef_struct_t_field_4" x0
      )

instance ( ty ~ Another_typedef_struct_t
         ) => BG.HasField "a_typedef_struct_t_field_4" (BG.Ptr A_typedef_struct_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"a_typedef_struct_t_field_4")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_4" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_4" =
    Another_typedef_struct_t

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @field_5@

    __defined at:__ @edge-cases\/distilled_lib_1.h 42:31@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
instance ( ty ~ BG.Ptr Another_typedef_struct_t
         ) => BG.CompatHasField.HasField "a_typedef_struct_t_field_5" A_typedef_struct_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A_typedef_struct_t { a_typedef_struct_t_field_5 = y1
                             , a_typedef_struct_t_field_0 = BG.getField @"a_typedef_struct_t_field_0" x0
                             , a_typedef_struct_t_field_1 = BG.getField @"a_typedef_struct_t_field_1" x0
                             , a_typedef_struct_t_field_2 = BG.getField @"a_typedef_struct_t_field_2" x0
                             , a_typedef_struct_t_field_3 = BG.getField @"a_typedef_struct_t_field_3" x0
                             , a_typedef_struct_t_field_4 = BG.getField @"a_typedef_struct_t_field_4" x0
                             , a_typedef_struct_t_field_6 = BG.getField @"a_typedef_struct_t_field_6" x0
                             , a_typedef_struct_t_field_7 = BG.getField @"a_typedef_struct_t_field_7" x0
                             , a_typedef_struct_t_field_8 = BG.getField @"a_typedef_struct_t_field_8" x0
                             , a_typedef_struct_t_field_9 = BG.getField @"a_typedef_struct_t_field_9" x0
                             , a_typedef_struct_t_field_10 = BG.getField @"a_typedef_struct_t_field_10" x0
                             }
      , BG.getField @"a_typedef_struct_t_field_5" x0
      )

instance ( ty ~ BG.Ptr Another_typedef_struct_t
         ) => BG.HasField "a_typedef_struct_t_field_5" (BG.Ptr A_typedef_struct_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"a_typedef_struct_t_field_5")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_5" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_5" =
    BG.Ptr Another_typedef_struct_t

  offset# = \_ -> \_ -> 16

{-| __C declaration:__ @field_6@

    __defined at:__ @edge-cases\/distilled_lib_1.h 43:31@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
instance ( ty ~ BG.Ptr BG.Void
         ) => BG.CompatHasField.HasField "a_typedef_struct_t_field_6" A_typedef_struct_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A_typedef_struct_t { a_typedef_struct_t_field_6 = y1
                             , a_typedef_struct_t_field_0 = BG.getField @"a_typedef_struct_t_field_0" x0
                             , a_typedef_struct_t_field_1 = BG.getField @"a_typedef_struct_t_field_1" x0
                             , a_typedef_struct_t_field_2 = BG.getField @"a_typedef_struct_t_field_2" x0
                             , a_typedef_struct_t_field_3 = BG.getField @"a_typedef_struct_t_field_3" x0
                             , a_typedef_struct_t_field_4 = BG.getField @"a_typedef_struct_t_field_4" x0
                             , a_typedef_struct_t_field_5 = BG.getField @"a_typedef_struct_t_field_5" x0
                             , a_typedef_struct_t_field_7 = BG.getField @"a_typedef_struct_t_field_7" x0
                             , a_typedef_struct_t_field_8 = BG.getField @"a_typedef_struct_t_field_8" x0
                             , a_typedef_struct_t_field_9 = BG.getField @"a_typedef_struct_t_field_9" x0
                             , a_typedef_struct_t_field_10 = BG.getField @"a_typedef_struct_t_field_10" x0
                             }
      , BG.getField @"a_typedef_struct_t_field_6" x0
      )

instance ( ty ~ BG.Ptr BG.Void
         ) => BG.HasField "a_typedef_struct_t_field_6" (BG.Ptr A_typedef_struct_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"a_typedef_struct_t_field_6")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_6" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_6" =
    BG.Ptr BG.Void

  offset# = \_ -> \_ -> 24

{-| __C declaration:__ @field_7@

    __defined at:__ @edge-cases\/distilled_lib_1.h 44:31@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
instance ( ty ~ CA.ConstantArray 7 HsBindgen.Runtime.LibC.Word32
         ) => BG.CompatHasField.HasField "a_typedef_struct_t_field_7" A_typedef_struct_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A_typedef_struct_t { a_typedef_struct_t_field_7 = y1
                             , a_typedef_struct_t_field_0 = BG.getField @"a_typedef_struct_t_field_0" x0
                             , a_typedef_struct_t_field_1 = BG.getField @"a_typedef_struct_t_field_1" x0
                             , a_typedef_struct_t_field_2 = BG.getField @"a_typedef_struct_t_field_2" x0
                             , a_typedef_struct_t_field_3 = BG.getField @"a_typedef_struct_t_field_3" x0
                             , a_typedef_struct_t_field_4 = BG.getField @"a_typedef_struct_t_field_4" x0
                             , a_typedef_struct_t_field_5 = BG.getField @"a_typedef_struct_t_field_5" x0
                             , a_typedef_struct_t_field_6 = BG.getField @"a_typedef_struct_t_field_6" x0
                             , a_typedef_struct_t_field_8 = BG.getField @"a_typedef_struct_t_field_8" x0
                             , a_typedef_struct_t_field_9 = BG.getField @"a_typedef_struct_t_field_9" x0
                             , a_typedef_struct_t_field_10 = BG.getField @"a_typedef_struct_t_field_10" x0
                             }
      , BG.getField @"a_typedef_struct_t_field_7" x0
      )

instance ( ty ~ CA.ConstantArray 7 HsBindgen.Runtime.LibC.Word32
         ) => BG.HasField "a_typedef_struct_t_field_7" (BG.Ptr A_typedef_struct_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"a_typedef_struct_t_field_7")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_7" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_7" =
    CA.ConstantArray 7 HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 32

{-| __C declaration:__ @field_8@

    __defined at:__ @edge-cases\/distilled_lib_1.h 45:31@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
instance ( ty ~ Another_typedef_enum_e
         ) => BG.CompatHasField.HasField "a_typedef_struct_t_field_8" A_typedef_struct_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A_typedef_struct_t { a_typedef_struct_t_field_8 = y1
                             , a_typedef_struct_t_field_0 = BG.getField @"a_typedef_struct_t_field_0" x0
                             , a_typedef_struct_t_field_1 = BG.getField @"a_typedef_struct_t_field_1" x0
                             , a_typedef_struct_t_field_2 = BG.getField @"a_typedef_struct_t_field_2" x0
                             , a_typedef_struct_t_field_3 = BG.getField @"a_typedef_struct_t_field_3" x0
                             , a_typedef_struct_t_field_4 = BG.getField @"a_typedef_struct_t_field_4" x0
                             , a_typedef_struct_t_field_5 = BG.getField @"a_typedef_struct_t_field_5" x0
                             , a_typedef_struct_t_field_6 = BG.getField @"a_typedef_struct_t_field_6" x0
                             , a_typedef_struct_t_field_7 = BG.getField @"a_typedef_struct_t_field_7" x0
                             , a_typedef_struct_t_field_9 = BG.getField @"a_typedef_struct_t_field_9" x0
                             , a_typedef_struct_t_field_10 = BG.getField @"a_typedef_struct_t_field_10" x0
                             }
      , BG.getField @"a_typedef_struct_t_field_8" x0
      )

instance ( ty ~ Another_typedef_enum_e
         ) => BG.HasField "a_typedef_struct_t_field_8" (BG.Ptr A_typedef_struct_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"a_typedef_struct_t_field_8")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_8" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_8" =
    Another_typedef_enum_e

  offset# = \_ -> \_ -> 60

{-| __C declaration:__ @field_9@

    __defined at:__ @edge-cases\/distilled_lib_1.h 46:31@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
instance ( ty ~ CA.ConstantArray 4 Another_typedef_enum_e
         ) => BG.CompatHasField.HasField "a_typedef_struct_t_field_9" A_typedef_struct_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A_typedef_struct_t { a_typedef_struct_t_field_9 = y1
                             , a_typedef_struct_t_field_0 = BG.getField @"a_typedef_struct_t_field_0" x0
                             , a_typedef_struct_t_field_1 = BG.getField @"a_typedef_struct_t_field_1" x0
                             , a_typedef_struct_t_field_2 = BG.getField @"a_typedef_struct_t_field_2" x0
                             , a_typedef_struct_t_field_3 = BG.getField @"a_typedef_struct_t_field_3" x0
                             , a_typedef_struct_t_field_4 = BG.getField @"a_typedef_struct_t_field_4" x0
                             , a_typedef_struct_t_field_5 = BG.getField @"a_typedef_struct_t_field_5" x0
                             , a_typedef_struct_t_field_6 = BG.getField @"a_typedef_struct_t_field_6" x0
                             , a_typedef_struct_t_field_7 = BG.getField @"a_typedef_struct_t_field_7" x0
                             , a_typedef_struct_t_field_8 = BG.getField @"a_typedef_struct_t_field_8" x0
                             , a_typedef_struct_t_field_10 = BG.getField @"a_typedef_struct_t_field_10" x0
                             }
      , BG.getField @"a_typedef_struct_t_field_9" x0
      )

instance ( ty ~ CA.ConstantArray 4 Another_typedef_enum_e
         ) => BG.HasField "a_typedef_struct_t_field_9" (BG.Ptr A_typedef_struct_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"a_typedef_struct_t_field_9")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_9" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_9" =
    CA.ConstantArray 4 Another_typedef_enum_e

  offset# = \_ -> \_ -> 64

{-| __C declaration:__ @field_10@

    __defined at:__ @edge-cases\/distilled_lib_1.h 47:31@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
instance ( ty ~ CA.ConstantArray 5 (CA.ConstantArray 3 Another_typedef_enum_e)
         ) => BG.CompatHasField.HasField "a_typedef_struct_t_field_10" A_typedef_struct_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A_typedef_struct_t { a_typedef_struct_t_field_10 = y1
                             , a_typedef_struct_t_field_0 = BG.getField @"a_typedef_struct_t_field_0" x0
                             , a_typedef_struct_t_field_1 = BG.getField @"a_typedef_struct_t_field_1" x0
                             , a_typedef_struct_t_field_2 = BG.getField @"a_typedef_struct_t_field_2" x0
                             , a_typedef_struct_t_field_3 = BG.getField @"a_typedef_struct_t_field_3" x0
                             , a_typedef_struct_t_field_4 = BG.getField @"a_typedef_struct_t_field_4" x0
                             , a_typedef_struct_t_field_5 = BG.getField @"a_typedef_struct_t_field_5" x0
                             , a_typedef_struct_t_field_6 = BG.getField @"a_typedef_struct_t_field_6" x0
                             , a_typedef_struct_t_field_7 = BG.getField @"a_typedef_struct_t_field_7" x0
                             , a_typedef_struct_t_field_8 = BG.getField @"a_typedef_struct_t_field_8" x0
                             , a_typedef_struct_t_field_9 = BG.getField @"a_typedef_struct_t_field_9" x0
                             }
      , BG.getField @"a_typedef_struct_t_field_10" x0
      )

instance ( ty ~ CA.ConstantArray 5 (CA.ConstantArray 3 Another_typedef_enum_e)
         ) => BG.HasField "a_typedef_struct_t_field_10" (BG.Ptr A_typedef_struct_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"a_typedef_struct_t_field_10")

instance HasCField.HasCField A_typedef_struct_t "a_typedef_struct_t_field_10" where

  type CFieldType A_typedef_struct_t "a_typedef_struct_t_field_10" =
    CA.ConstantArray 5 (CA.ConstantArray 3 Another_typedef_enum_e)

  offset# = \_ -> \_ -> 80

{-| __C declaration:__ @macro A_DEFINE_0@

    __defined at:__ @edge-cases\/distilled_lib_1.h 53:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
a_DEFINE_0 :: BG.CInt
a_DEFINE_0 = (0 :: BG.CInt)

{-| __C declaration:__ @macro A_DEFINE_1@

    __defined at:__ @edge-cases\/distilled_lib_1.h 54:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
a_DEFINE_1 :: BG.CUInt
a_DEFINE_1 = (20560 :: BG.CUInt)

{-| __C declaration:__ @macro A_DEFINE_2@

    __defined at:__ @edge-cases\/distilled_lib_1.h 55:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
a_DEFINE_2 :: BG.CInt
a_DEFINE_2 = (2 :: BG.CInt)

{-| __C declaration:__ @macro TWO_ARGS@

    __defined at:__ @edge-cases\/distilled_lib_1.h 56:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
tWO_ARGS :: (BG.CInt, BG.CInt)
tWO_ARGS = ((13398 :: BG.CInt), (30874 :: BG.CInt))

{-| __C declaration:__ @enum a_typedef_enum_e@

    __defined at:__ @edge-cases\/distilled_lib_1.h 61:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
newtype A_typedef_enum_e = A_typedef_enum_e
  { unwrapA_typedef_enum_e :: BG.CUChar
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

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

deriving via Marshal.EquivStorable A_typedef_enum_e instance BG.Storable A_typedef_enum_e

deriving via BG.CUChar instance BG.Prim A_typedef_enum_e

instance CEnum.CEnum A_typedef_enum_e where

  type CEnumZ A_typedef_enum_e = BG.CUChar

  toCEnum = A_typedef_enum_e

  fromCEnum = BG.getField @"unwrapA_typedef_enum_e"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [ (0, BG.singleton "ENUM_CASE_0")
                                   , (1, BG.singleton "ENUM_CASE_1")
                                   , (2, BG.singleton "ENUM_CASE_2")
                                   , (3, BG.singleton "ENUM_CASE_3")
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

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ BG.CUChar
         ) => BG.CompatHasField.HasField "unwrapA_typedef_enum_e" A_typedef_enum_e ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A_typedef_enum_e {unwrapA_typedef_enum_e = y1}
      , BG.getField @"unwrapA_typedef_enum_e" x0
      )

instance ( ty ~ BG.CUChar
         ) => BG.HasField "unwrapA_typedef_enum_e" (BG.Ptr A_typedef_enum_e) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapA_typedef_enum_e")

instance HasCField.HasCField A_typedef_enum_e "unwrapA_typedef_enum_e" where

  type CFieldType A_typedef_enum_e "unwrapA_typedef_enum_e" =
    BG.CUChar

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
  { unwrapCallback_t_Aux :: BG.Ptr BG.Void -> HsBindgen.Runtime.LibC.Word32 -> IO HsBindgen.Runtime.LibC.Word32
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toCallback_t_Aux@
foreign import ccall safe "wrapper" hs_bindgen_b6b6922e35047658_base ::
     (BG.Ptr BG.Void -> BG.Word32 -> IO BG.Word32)
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.Word32 -> IO BG.Word32))

-- __unique:__ @toCallback_t_Aux@
hs_bindgen_b6b6922e35047658 ::
     Callback_t_Aux
  -> IO (BG.FunPtr Callback_t_Aux)
hs_bindgen_b6b6922e35047658 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_b6b6922e35047658_base (BG.toFFIType fun0))

-- __unique:__ @fromCallback_t_Aux@
foreign import ccall safe "dynamic" hs_bindgen_d6debb4b8d5bb869_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.Word32 -> IO BG.Word32)
  -> BG.Ptr BG.Void -> BG.Word32 -> IO BG.Word32

-- __unique:__ @fromCallback_t_Aux@
hs_bindgen_d6debb4b8d5bb869 ::
     BG.FunPtr Callback_t_Aux
  -> Callback_t_Aux
hs_bindgen_d6debb4b8d5bb869 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_d6debb4b8d5bb869_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr Callback_t_Aux where

  toFunPtr = hs_bindgen_b6b6922e35047658

instance BG.FromFunPtr Callback_t_Aux where

  fromFunPtr = hs_bindgen_d6debb4b8d5bb869

instance ( ty ~ (BG.Ptr BG.Void -> HsBindgen.Runtime.LibC.Word32 -> IO HsBindgen.Runtime.LibC.Word32)
         ) => BG.CompatHasField.HasField "unwrapCallback_t_Aux" Callback_t_Aux ty where

  hasField =
    \x0 ->
      ( \y1 -> Callback_t_Aux {unwrapCallback_t_Aux = y1}
      , BG.getField @"unwrapCallback_t_Aux" x0
      )

instance ( ty ~ (BG.Ptr BG.Void -> HsBindgen.Runtime.LibC.Word32 -> IO HsBindgen.Runtime.LibC.Word32)
         ) => BG.HasField "unwrapCallback_t_Aux" (BG.Ptr Callback_t_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapCallback_t_Aux")

instance HasCField.HasCField Callback_t_Aux "unwrapCallback_t_Aux" where

  type CFieldType Callback_t_Aux "unwrapCallback_t_Aux" =
    BG.Ptr BG.Void -> HsBindgen.Runtime.LibC.Word32 -> IO HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @callback_t@

    __defined at:__ @edge-cases\/distilled_lib_1.h 77:19@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
newtype Callback_t = Callback_t
  { unwrapCallback_t :: BG.FunPtr Callback_t_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Callback_t_Aux
         ) => BG.CompatHasField.HasField "unwrapCallback_t" Callback_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Callback_t {unwrapCallback_t = y1}
      , BG.getField @"unwrapCallback_t" x0
      )

instance ( ty ~ BG.FunPtr Callback_t_Aux
         ) => BG.HasField "unwrapCallback_t" (BG.Ptr Callback_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapCallback_t")

instance HasCField.HasCField Callback_t "unwrapCallback_t" where

  type CFieldType Callback_t "unwrapCallback_t" =
    BG.FunPtr Callback_t_Aux

  offset# = \_ -> \_ -> 0
