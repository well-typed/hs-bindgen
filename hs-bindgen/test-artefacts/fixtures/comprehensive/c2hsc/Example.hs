{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.An_pchar(..)
    , Example.MyCoolStruct(..)
    , Example.Foo_Aux(..)
    , Example.Foo(..)
    , Example.Foo_t(..)
    , Example.Foo_10
    , Example.Bar_10(..)
    , pattern Example.BAR
    , Example.St(..)
    , Example.E(..)
    , pattern Example.CONST
    , Example.U(..)
    , Example.MyTypeImpl
    , Example.MyType(..)
    , Example.MyStructType(..)
    , Example.MyStructEmptyType
    , Example.Ordinary_float_struct(..)
    , Example.Ordinary_double_struct(..)
    , Example.Ordinary_signed_char_struct(..)
    , Example.Explicit_signed_char_struct(..)
    , Example.Unsigned_char_struct(..)
    , Example.Ordinary_signed_short_struct(..)
    , Example.Explicit_signed_short_struct(..)
    , Example.Unsigned_short_struct(..)
    , Example.Ordinary_signed_int_struct(..)
    , Example.Explicit_signed_int_struct(..)
    , Example.Unsigned_int_struct(..)
    , Example.Ordinary_signed_long_struct(..)
    , Example.Explicit_signed_long_struct(..)
    , Example.Unsigned_long_struct(..)
    , Example.Ordinary_signed_long_long_struct(..)
    , Example.Explicit_signed_long_long_struct(..)
    , Example.Unsigned_long_long_struct(..)
    , Example.Ordinary_void_pointer_struct(..)
    , Example.Ordinary_float_pointer_struct(..)
    , Example.Ordinary_double_pointer_struct(..)
    , Example.Ordinary_signed_char_pointer_struct(..)
    , Example.Explicit_signed_char_pointer_struct(..)
    , Example.Unsigned_char_pointer_struct(..)
    , Example.Ordinary_signed_short_pointer_struct(..)
    , Example.Explicit_signed_short_pointer_struct(..)
    , Example.Unsigned_short_pointer_struct(..)
    , Example.Ordinary_signed_int_pointer_struct(..)
    , Example.Explicit_signed_int_pointer_struct(..)
    , Example.Unsigned_int_pointer_struct(..)
    , Example.Ordinary_signed_long_pointer_struct(..)
    , Example.Explicit_signed_long_pointer_struct(..)
    , Example.Unsigned_long_pointer_struct(..)
    , Example.Ordinary_signed_long_long_pointer_struct(..)
    , Example.Explicit_signed_long_long_pointer_struct(..)
    , Example.Unsigned_long_long_pointer_struct(..)
    , Example.Ordinary_float_array_struct(..)
    , Example.Ordinary_double_array_struct(..)
    , Example.Ordinary_signed_char_array_struct(..)
    , Example.Explicit_signed_char_array_struct(..)
    , Example.Unsigned_char_array_struct(..)
    , Example.Ordinary_signed_short_array_struct(..)
    , Example.Explicit_signed_short_array_struct(..)
    , Example.Unsigned_short_array_struct(..)
    , Example.Ordinary_signed_int_array_struct(..)
    , Example.Explicit_signed_int_array_struct(..)
    , Example.Unsigned_int_array_struct(..)
    , Example.Ordinary_signed_long_array_struct(..)
    , Example.Explicit_signed_long_array_struct(..)
    , Example.Unsigned_long_array_struct(..)
    , Example.Ordinary_signed_long_long_array_struct(..)
    , Example.Explicit_signed_long_long_array_struct(..)
    , Example.Unsigned_long_long_array_struct(..)
    , Example.Ordinary_void_pointer_array_struct(..)
    , Example.Ordinary_float_pointer_array_struct(..)
    , Example.Ordinary_double_pointer_array_struct(..)
    , Example.Ordinary_signed_char_pointer_array_struct(..)
    , Example.Explicit_signed_char_pointer_array_struct(..)
    , Example.Unsigned_char_pointer_array_struct(..)
    , Example.Ordinary_signed_short_pointer_array_struct(..)
    , Example.Explicit_signed_short_pointer_array_struct(..)
    , Example.Unsigned_short_pointer_array_struct(..)
    , Example.Ordinary_signed_int_pointer_array_struct(..)
    , Example.Explicit_signed_int_pointer_array_struct(..)
    , Example.Unsigned_int_pointer_array_struct(..)
    , Example.Ordinary_signed_long_pointer_array_struct(..)
    , Example.Explicit_signed_long_pointer_array_struct(..)
    , Example.Unsigned_long_pointer_array_struct(..)
    , Example.Ordinary_signed_long_long_pointer_array_struct(..)
    , Example.Explicit_signed_long_long_pointer_array_struct(..)
    , Example.Unsigned_long_long_pointer_array_struct(..)
    , Example.An_int(..)
    , Example.Cal_table_table(..)
    , Example.Cal_table(..)
    , Example.Elf32_External_Dyn_d_un(..)
    , Example.Elf32_External_Dyn(..)
    , Example.Bug_24(..)
    , Example.Bug_24_2(..)
    , Example.MyArray_27(..)
    , Example.MyStruct_27(..)
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union

{-| Issues

    __C declaration:__ @an_pchar@

    __defined at:__ @comprehensive\/c2hsc.h 12:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype An_pchar = An_pchar
  { unwrap :: PtrConst.PtrConst BG.CChar
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ PtrConst.PtrConst BG.CChar
         ) => BG.CompatHasField.HasField "unwrap" An_pchar ty where

  hasField =
    \x0 ->
      (\y1 ->
         An_pchar {unwrap = y1}, BG.getField @"unwrap" x0)

instance ( ty ~ PtrConst.PtrConst BG.CChar
         ) => BG.HasField "unwrap" (BG.Ptr An_pchar) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrap")

instance HasCField.HasCField An_pchar "unwrap" where

  type CFieldType An_pchar "unwrap" =
    PtrConst.PtrConst BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct MyCoolStruct@

    __defined at:__ @comprehensive\/c2hsc.h 15:9@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data MyCoolStruct = MyCoolStruct
  { listOfNames :: CA.ConstantArray 8 (CA.ConstantArray 255 BG.CChar)
    {- ^ __C declaration:__ @listOfNames@

         __defined at:__ @comprehensive\/c2hsc.h 16:10@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize MyCoolStruct where

  staticSizeOf = \_ -> (2040 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw MyCoolStruct where

  readRaw =
    \ptr0 ->
          pure MyCoolStruct
      <*> HasCField.readRaw (BG.Proxy @"listOfNames") ptr0

instance Marshal.WriteRaw MyCoolStruct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyCoolStruct listOfNames2 ->
            HasCField.writeRaw (BG.Proxy @"listOfNames") ptr0 listOfNames2

deriving via Marshal.EquivStorable MyCoolStruct instance BG.Storable MyCoolStruct

deriving via Struct.IsStructViaStorable MyCoolStruct instance Struct.IsStruct MyCoolStruct

{-| __C declaration:__ @listOfNames@

    __defined at:__ @comprehensive\/c2hsc.h 16:10@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 8 (CA.ConstantArray 255 BG.CChar)
         ) => BG.CompatHasField.HasField "listOfNames" MyCoolStruct ty where

  hasField =
    \x0 ->
      (\y1 ->
         MyCoolStruct {listOfNames = y1}, BG.getField @"listOfNames" x0)

instance ( ty ~ CA.ConstantArray 8 (CA.ConstantArray 255 BG.CChar)
         ) => BG.HasField "listOfNames" (BG.Ptr MyCoolStruct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"listOfNames")

instance HasCField.HasCField MyCoolStruct "listOfNames" where

  type CFieldType MyCoolStruct "listOfNames" =
    CA.ConstantArray 8 (CA.ConstantArray 255 BG.CChar)

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Foo'

    __C declaration:__ @foo@

    __defined at:__ @comprehensive\/c2hsc.h 20:15@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype Foo_Aux = Foo_Aux
  { unwrap :: BG.CInt -> IO BG.CInt
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toFoo_Aux@
foreign import ccall safe "wrapper" hs_bindgen_b5a7b5e83ffee6b4_base ::
     (BG.Int32 -> IO BG.Int32)
  -> IO (BG.FunPtr (BG.Int32 -> IO BG.Int32))

-- __unique:__ @toFoo_Aux@
hs_bindgen_b5a7b5e83ffee6b4 ::
     Foo_Aux
  -> IO (BG.FunPtr Foo_Aux)
hs_bindgen_b5a7b5e83ffee6b4 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_b5a7b5e83ffee6b4_base (BG.toFFIType fun0))

-- __unique:__ @fromFoo_Aux@
foreign import ccall safe "dynamic" hs_bindgen_223d08172bb37c01_base ::
     BG.FunPtr (BG.Int32 -> IO BG.Int32)
  -> BG.Int32 -> IO BG.Int32

-- __unique:__ @fromFoo_Aux@
hs_bindgen_223d08172bb37c01 ::
     BG.FunPtr Foo_Aux
  -> Foo_Aux
hs_bindgen_223d08172bb37c01 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_223d08172bb37c01_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr Foo_Aux where

  toFunPtr = hs_bindgen_b5a7b5e83ffee6b4

instance BG.FromFunPtr Foo_Aux where

  fromFunPtr = hs_bindgen_223d08172bb37c01

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "unwrap" Foo_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         Foo_Aux {unwrap = y1}, BG.getField @"unwrap" x0)

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.HasField "unwrap" (BG.Ptr Foo_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrap")

instance HasCField.HasCField Foo_Aux "unwrap" where

  type CFieldType Foo_Aux "unwrap" =
    BG.CInt -> IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @foo@

    __defined at:__ @comprehensive\/c2hsc.h 20:15@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype Foo = Foo
  { unwrap :: BG.FunPtr Foo_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Foo_Aux
         ) => BG.CompatHasField.HasField "unwrap" Foo ty where

  hasField =
    \x0 ->
      (\y1 -> Foo {unwrap = y1}, BG.getField @"unwrap" x0)

instance ( ty ~ BG.FunPtr Foo_Aux
         ) => BG.HasField "unwrap" (BG.Ptr Foo) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrap")

instance HasCField.HasCField Foo "unwrap" where

  type CFieldType Foo "unwrap" = BG.FunPtr Foo_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct foo_t@

    __defined at:__ @comprehensive\/c2hsc.h 31:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Foo_t = Foo_t
  { foo_member :: BG.FunPtr (BG.CInt -> IO BG.CInt)
    {- ^ __C declaration:__ @foo_member@

         __defined at:__ @comprehensive\/c2hsc.h 32:11@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Foo_t where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Foo_t where

  readRaw =
    \ptr0 ->
          pure Foo_t
      <*> HasCField.readRaw (BG.Proxy @"foo_member") ptr0

instance Marshal.WriteRaw Foo_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo_t foo_member2 ->
            HasCField.writeRaw (BG.Proxy @"foo_member") ptr0 foo_member2

deriving via Marshal.EquivStorable Foo_t instance BG.Storable Foo_t

deriving via Struct.IsStructViaStorable Foo_t instance Struct.IsStruct Foo_t

{-| __C declaration:__ @foo_member@

    __defined at:__ @comprehensive\/c2hsc.h 32:11@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.FunPtr (BG.CInt -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "foo_member" Foo_t ty where

  hasField =
    \x0 ->
      (\y1 ->
         Foo_t {foo_member = y1}, BG.getField @"foo_member" x0)

instance ( ty ~ BG.FunPtr (BG.CInt -> IO BG.CInt)
         ) => BG.HasField "foo_member" (BG.Ptr Foo_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"foo_member")

instance HasCField.HasCField Foo_t "foo_member" where

  type CFieldType Foo_t "foo_member" =
    BG.FunPtr (BG.CInt -> IO BG.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct Foo_10_@

    __defined at:__ @comprehensive\/c2hsc.h 44:16@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Foo_10

{-| __C declaration:__ @enum Bar_10_@

    __defined at:__ @comprehensive\/c2hsc.h 45:14@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype Bar_10 = Bar_10
  { unwrap :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize Bar_10 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Bar_10 where

  readRaw =
    \ptr0 ->
          pure Bar_10
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Bar_10 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar_10 unwrap2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrap2

deriving via Marshal.EquivStorable Bar_10 instance BG.Storable Bar_10

deriving via BG.CUInt instance BG.Prim Bar_10

instance CEnum.CEnum Bar_10 where

  type CEnumZ Bar_10 = BG.CUInt

  toCEnum = Bar_10

  fromCEnum = BG.getField @"unwrap"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "BAR")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Bar_10"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Bar_10"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Bar_10 where

  minDeclaredValue = BAR

  maxDeclaredValue = BAR

instance Show Bar_10 where

  showsPrec = CEnum.shows

instance Read Bar_10 where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance (ty ~ BG.CUInt) => BG.CompatHasField.HasField "unwrap" Bar_10 ty where

  hasField =
    \x0 ->
      (\y1 ->
         Bar_10 {unwrap = y1}, BG.getField @"unwrap" x0)

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrap" (BG.Ptr Bar_10) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrap")

instance HasCField.HasCField Bar_10 "unwrap" where

  type CFieldType Bar_10 "unwrap" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @BAR@

    __defined at:__ @comprehensive\/c2hsc.h 45:24@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
pattern BAR :: Bar_10
pattern BAR = Bar_10 0

{-| __C declaration:__ @struct st@

    __defined at:__ @comprehensive\/c2hsc.h 48:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data St = St
  { i :: BG.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @comprehensive\/c2hsc.h 49:7@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize St where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw St where

  readRaw =
    \ptr0 ->
          pure St
      <*> HasCField.readRaw (BG.Proxy @"i") ptr0

instance Marshal.WriteRaw St where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          St i2 -> HasCField.writeRaw (BG.Proxy @"i") ptr0 i2

deriving via Marshal.EquivStorable St instance BG.Storable St

deriving via Struct.IsStructViaStorable St instance Struct.IsStruct St

{-| __C declaration:__ @i@

    __defined at:__ @comprehensive\/c2hsc.h 49:7@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "i" St ty where

  hasField =
    \x0 -> (\y1 -> St {i = y1}, BG.getField @"i" x0)

instance (ty ~ BG.CInt) => BG.HasField "i" (BG.Ptr St) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"i")

instance HasCField.HasCField St "i" where

  type CFieldType St "i" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @enum e@

    __defined at:__ @comprehensive\/c2hsc.h 52:6@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype E = E
  { unwrap :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize E where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E where

  readRaw =
    \ptr0 ->
          pure E
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E unwrap2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrap2

deriving via Marshal.EquivStorable E instance BG.Storable E

deriving via BG.CUInt instance BG.Prim E

instance CEnum.CEnum E where

  type CEnumZ E = BG.CUInt

  toCEnum = E

  fromCEnum = BG.getField @"unwrap"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "CONST")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E where

  minDeclaredValue = CONST

  maxDeclaredValue = CONST

instance Show E where

  showsPrec = CEnum.shows

instance Read E where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance (ty ~ BG.CUInt) => BG.CompatHasField.HasField "unwrap" E ty where

  hasField =
    \x0 ->
      (\y1 -> E {unwrap = y1}, BG.getField @"unwrap" x0)

instance (ty ~ BG.CUInt) => BG.HasField "unwrap" (BG.Ptr E) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrap")

instance HasCField.HasCField E "unwrap" where

  type CFieldType E "unwrap" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @CONST@

    __defined at:__ @comprehensive\/c2hsc.h 53:3@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
pattern CONST :: E
pattern CONST = E 0

{-| __C declaration:__ @union u@

    __defined at:__ @comprehensive\/c2hsc.h 56:7@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype U = U
  { unwrap :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 1 1 instance Marshal.StaticSize U

deriving via BG.SizedByteArray 1 1 instance Marshal.ReadRaw U

deriving via BG.SizedByteArray 1 1 instance Marshal.WriteRaw U

deriving via Marshal.EquivStorable U instance BG.Storable U

deriving via BG.SizedByteArray 1 1 instance Union.IsUnion U

{-| __C declaration:__ @c@

    __defined at:__ @comprehensive\/c2hsc.h 57:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance (ty ~ BG.CChar) => BG.HasField "c" U ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @c@

    __defined at:__ @comprehensive\/c2hsc.h 57:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "c" U ty where

  hasField =
    \x0 -> (BG.setUnionPayload, BG.getField @"c" x0)

instance (ty ~ BG.CChar) => BG.HasField "c" (BG.Ptr U) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"c")

instance HasCField.HasCField U "c" where

  type CFieldType U "c" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct MyTypeImpl@

    __defined at:__ @comprehensive\/c2hsc.h 61:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data MyTypeImpl

{-| __C declaration:__ @MyType@

    __defined at:__ @comprehensive\/c2hsc.h 62:28@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype MyType = MyType
  { unwrap :: BG.Ptr MyTypeImpl
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr MyTypeImpl
         ) => BG.CompatHasField.HasField "unwrap" MyType ty where

  hasField =
    \x0 ->
      (\y1 ->
         MyType {unwrap = y1}, BG.getField @"unwrap" x0)

instance ( ty ~ BG.Ptr MyTypeImpl
         ) => BG.HasField "unwrap" (BG.Ptr MyType) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrap")

instance HasCField.HasCField MyType "unwrap" where

  type CFieldType MyType "unwrap" = BG.Ptr MyTypeImpl

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct MyStruct@

    __defined at:__ @comprehensive\/c2hsc.h 64:16@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data MyStructType = MyStructType
  { x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @comprehensive\/c2hsc.h 65:7@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize MyStructType where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw MyStructType where

  readRaw =
    \ptr0 ->
          pure MyStructType
      <*> HasCField.readRaw (BG.Proxy @"x") ptr0

instance Marshal.WriteRaw MyStructType where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyStructType x2 ->
            HasCField.writeRaw (BG.Proxy @"x") ptr0 x2

deriving via Marshal.EquivStorable MyStructType instance BG.Storable MyStructType

deriving via Struct.IsStructViaStorable MyStructType instance Struct.IsStruct MyStructType

{-| __C declaration:__ @x@

    __defined at:__ @comprehensive\/c2hsc.h 65:7@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "x" MyStructType ty where

  hasField =
    \x0 ->
      (\y1 -> MyStructType {x = y1}, BG.getField @"x" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "x" (BG.Ptr MyStructType) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"x")

instance HasCField.HasCField MyStructType "x" where

  type CFieldType MyStructType "x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct MyStructEmpty@

    __defined at:__ @comprehensive\/c2hsc.h 68:16@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data MyStructEmptyType

{-| Structs: primitive types

    __C declaration:__ @struct ordinary_float_struct@

    __defined at:__ @comprehensive\/c2hsc.h 204:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_float_struct = Ordinary_float_struct
  { ordinary_float_member :: BG.CFloat
    {- ^ __C declaration:__ @ordinary_float_member@

         __defined at:__ @comprehensive\/c2hsc.h 204:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_float_struct where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Ordinary_float_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_float_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_float_member") ptr0

instance Marshal.WriteRaw Ordinary_float_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_float_struct ordinary_float_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_float_member") ptr0 ordinary_float_member2

deriving via Marshal.EquivStorable Ordinary_float_struct instance BG.Storable Ordinary_float_struct

deriving via Struct.IsStructViaStorable Ordinary_float_struct instance Struct.IsStruct Ordinary_float_struct

{-| __C declaration:__ @ordinary_float_member@

    __defined at:__ @comprehensive\/c2hsc.h 204:62@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CFloat
         ) => BG.CompatHasField.HasField "ordinary_float_member" Ordinary_float_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_float_struct {ordinary_float_member = y1}
      , BG.getField @"ordinary_float_member" x0
      )

instance ( ty ~ BG.CFloat
         ) => BG.HasField "ordinary_float_member" (BG.Ptr Ordinary_float_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_float_member")

instance HasCField.HasCField Ordinary_float_struct "ordinary_float_member" where

  type CFieldType Ordinary_float_struct "ordinary_float_member" =
    BG.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_double_struct@

    __defined at:__ @comprehensive\/c2hsc.h 205:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_double_struct = Ordinary_double_struct
  { ordinary_double_member :: BG.CDouble
    {- ^ __C declaration:__ @ordinary_double_member@

         __defined at:__ @comprehensive\/c2hsc.h 205:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_double_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_double_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_double_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_double_member") ptr0

instance Marshal.WriteRaw Ordinary_double_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_double_struct ordinary_double_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_double_member") ptr0 ordinary_double_member2

deriving via Marshal.EquivStorable Ordinary_double_struct instance BG.Storable Ordinary_double_struct

deriving via Struct.IsStructViaStorable Ordinary_double_struct instance Struct.IsStruct Ordinary_double_struct

{-| __C declaration:__ @ordinary_double_member@

    __defined at:__ @comprehensive\/c2hsc.h 205:62@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "ordinary_double_member" Ordinary_double_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_double_struct {ordinary_double_member = y1}
      , BG.getField @"ordinary_double_member" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "ordinary_double_member" (BG.Ptr Ordinary_double_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_double_member")

instance HasCField.HasCField Ordinary_double_struct "ordinary_double_member" where

  type CFieldType Ordinary_double_struct "ordinary_double_member" =
    BG.CDouble

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_char_struct@

    __defined at:__ @comprehensive\/c2hsc.h 208:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_char_struct = Ordinary_signed_char_struct
  { ordinary_signed_char_member :: BG.CChar
    {- ^ __C declaration:__ @ordinary_signed_char_member@

         __defined at:__ @comprehensive\/c2hsc.h 208:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_char_struct where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Ordinary_signed_char_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_char_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_char_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_char_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_char_struct ordinary_signed_char_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_signed_char_member") ptr0 ordinary_signed_char_member2

deriving via Marshal.EquivStorable Ordinary_signed_char_struct instance BG.Storable Ordinary_signed_char_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_char_struct instance Struct.IsStruct Ordinary_signed_char_struct

{-| __C declaration:__ @ordinary_signed_char_member@

    __defined at:__ @comprehensive\/c2hsc.h 208:62@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CChar
         ) => BG.CompatHasField.HasField "ordinary_signed_char_member" Ordinary_signed_char_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_char_struct {ordinary_signed_char_member = y1}
      , BG.getField @"ordinary_signed_char_member" x0
      )

instance ( ty ~ BG.CChar
         ) => BG.HasField "ordinary_signed_char_member" (BG.Ptr Ordinary_signed_char_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_char_member")

instance HasCField.HasCField Ordinary_signed_char_struct "ordinary_signed_char_member" where

  type CFieldType Ordinary_signed_char_struct "ordinary_signed_char_member" =
    BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_char_struct@

    __defined at:__ @comprehensive\/c2hsc.h 209:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_char_struct = Explicit_signed_char_struct
  { explicit_signed_char_member :: BG.CSChar
    {- ^ __C declaration:__ @explicit_signed_char_member@

         __defined at:__ @comprehensive\/c2hsc.h 209:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_char_struct where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Explicit_signed_char_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_char_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_char_member") ptr0

instance Marshal.WriteRaw Explicit_signed_char_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_char_struct explicit_signed_char_member2 ->
            HasCField.writeRaw (BG.Proxy @"explicit_signed_char_member") ptr0 explicit_signed_char_member2

deriving via Marshal.EquivStorable Explicit_signed_char_struct instance BG.Storable Explicit_signed_char_struct

deriving via Struct.IsStructViaStorable Explicit_signed_char_struct instance Struct.IsStruct Explicit_signed_char_struct

{-| __C declaration:__ @explicit_signed_char_member@

    __defined at:__ @comprehensive\/c2hsc.h 209:62@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CSChar
         ) => BG.CompatHasField.HasField "explicit_signed_char_member" Explicit_signed_char_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_char_struct {explicit_signed_char_member = y1}
      , BG.getField @"explicit_signed_char_member" x0
      )

instance ( ty ~ BG.CSChar
         ) => BG.HasField "explicit_signed_char_member" (BG.Ptr Explicit_signed_char_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_char_member")

instance HasCField.HasCField Explicit_signed_char_struct "explicit_signed_char_member" where

  type CFieldType Explicit_signed_char_struct "explicit_signed_char_member" =
    BG.CSChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_char_struct@

    __defined at:__ @comprehensive\/c2hsc.h 210:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_char_struct = Unsigned_char_struct
  { unsigned_char_member :: BG.CUChar
    {- ^ __C declaration:__ @unsigned_char_member@

         __defined at:__ @comprehensive\/c2hsc.h 210:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_char_struct where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Unsigned_char_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_char_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_char_member") ptr0

instance Marshal.WriteRaw Unsigned_char_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_char_struct unsigned_char_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_char_member") ptr0 unsigned_char_member2

deriving via Marshal.EquivStorable Unsigned_char_struct instance BG.Storable Unsigned_char_struct

deriving via Struct.IsStructViaStorable Unsigned_char_struct instance Struct.IsStruct Unsigned_char_struct

{-| __C declaration:__ @unsigned_char_member@

    __defined at:__ @comprehensive\/c2hsc.h 210:62@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CUChar
         ) => BG.CompatHasField.HasField "unsigned_char_member" Unsigned_char_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_char_struct {unsigned_char_member = y1}
      , BG.getField @"unsigned_char_member" x0
      )

instance ( ty ~ BG.CUChar
         ) => BG.HasField "unsigned_char_member" (BG.Ptr Unsigned_char_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_char_member")

instance HasCField.HasCField Unsigned_char_struct "unsigned_char_member" where

  type CFieldType Unsigned_char_struct "unsigned_char_member" =
    BG.CUChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_short_struct@

    __defined at:__ @comprehensive\/c2hsc.h 212:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_short_struct = Ordinary_signed_short_struct
  { ordinary_signed_short_member :: BG.CShort
    {- ^ __C declaration:__ @ordinary_signed_short_member@

         __defined at:__ @comprehensive\/c2hsc.h 212:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_short_struct where

  staticSizeOf = \_ -> (2 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Ordinary_signed_short_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_short_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_short_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_short_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_short_struct ordinary_signed_short_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_signed_short_member") ptr0 ordinary_signed_short_member2

deriving via Marshal.EquivStorable Ordinary_signed_short_struct instance BG.Storable Ordinary_signed_short_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_short_struct instance Struct.IsStruct Ordinary_signed_short_struct

{-| __C declaration:__ @ordinary_signed_short_member@

    __defined at:__ @comprehensive\/c2hsc.h 212:62@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CShort
         ) => BG.CompatHasField.HasField "ordinary_signed_short_member" Ordinary_signed_short_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_short_struct {ordinary_signed_short_member = y1}
      , BG.getField @"ordinary_signed_short_member" x0
      )

instance ( ty ~ BG.CShort
         ) => BG.HasField "ordinary_signed_short_member" (BG.Ptr Ordinary_signed_short_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_short_member")

instance HasCField.HasCField Ordinary_signed_short_struct "ordinary_signed_short_member" where

  type CFieldType Ordinary_signed_short_struct "ordinary_signed_short_member" =
    BG.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_short_struct@

    __defined at:__ @comprehensive\/c2hsc.h 213:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_short_struct = Explicit_signed_short_struct
  { explicit_signed_short_member :: BG.CShort
    {- ^ __C declaration:__ @explicit_signed_short_member@

         __defined at:__ @comprehensive\/c2hsc.h 213:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_short_struct where

  staticSizeOf = \_ -> (2 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Explicit_signed_short_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_short_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_short_member") ptr0

instance Marshal.WriteRaw Explicit_signed_short_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_short_struct explicit_signed_short_member2 ->
            HasCField.writeRaw (BG.Proxy @"explicit_signed_short_member") ptr0 explicit_signed_short_member2

deriving via Marshal.EquivStorable Explicit_signed_short_struct instance BG.Storable Explicit_signed_short_struct

deriving via Struct.IsStructViaStorable Explicit_signed_short_struct instance Struct.IsStruct Explicit_signed_short_struct

{-| __C declaration:__ @explicit_signed_short_member@

    __defined at:__ @comprehensive\/c2hsc.h 213:62@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CShort
         ) => BG.CompatHasField.HasField "explicit_signed_short_member" Explicit_signed_short_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_short_struct {explicit_signed_short_member = y1}
      , BG.getField @"explicit_signed_short_member" x0
      )

instance ( ty ~ BG.CShort
         ) => BG.HasField "explicit_signed_short_member" (BG.Ptr Explicit_signed_short_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_short_member")

instance HasCField.HasCField Explicit_signed_short_struct "explicit_signed_short_member" where

  type CFieldType Explicit_signed_short_struct "explicit_signed_short_member" =
    BG.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_short_struct@

    __defined at:__ @comprehensive\/c2hsc.h 214:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_short_struct = Unsigned_short_struct
  { unsigned_short_member :: BG.CUShort
    {- ^ __C declaration:__ @unsigned_short_member@

         __defined at:__ @comprehensive\/c2hsc.h 214:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_short_struct where

  staticSizeOf = \_ -> (2 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Unsigned_short_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_short_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_short_member") ptr0

instance Marshal.WriteRaw Unsigned_short_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_short_struct unsigned_short_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_short_member") ptr0 unsigned_short_member2

deriving via Marshal.EquivStorable Unsigned_short_struct instance BG.Storable Unsigned_short_struct

deriving via Struct.IsStructViaStorable Unsigned_short_struct instance Struct.IsStruct Unsigned_short_struct

{-| __C declaration:__ @unsigned_short_member@

    __defined at:__ @comprehensive\/c2hsc.h 214:62@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CUShort
         ) => BG.CompatHasField.HasField "unsigned_short_member" Unsigned_short_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_short_struct {unsigned_short_member = y1}
      , BG.getField @"unsigned_short_member" x0
      )

instance ( ty ~ BG.CUShort
         ) => BG.HasField "unsigned_short_member" (BG.Ptr Unsigned_short_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_short_member")

instance HasCField.HasCField Unsigned_short_struct "unsigned_short_member" where

  type CFieldType Unsigned_short_struct "unsigned_short_member" =
    BG.CUShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_int_struct@

    __defined at:__ @comprehensive\/c2hsc.h 216:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_int_struct = Ordinary_signed_int_struct
  { ordinary_signed_int_member :: BG.CInt
    {- ^ __C declaration:__ @ordinary_signed_int_member@

         __defined at:__ @comprehensive\/c2hsc.h 216:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_int_struct where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Ordinary_signed_int_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_int_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_int_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_int_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_int_struct ordinary_signed_int_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_signed_int_member") ptr0 ordinary_signed_int_member2

deriving via Marshal.EquivStorable Ordinary_signed_int_struct instance BG.Storable Ordinary_signed_int_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_int_struct instance Struct.IsStruct Ordinary_signed_int_struct

{-| __C declaration:__ @ordinary_signed_int_member@

    __defined at:__ @comprehensive\/c2hsc.h 216:62@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "ordinary_signed_int_member" Ordinary_signed_int_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_int_struct {ordinary_signed_int_member = y1}
      , BG.getField @"ordinary_signed_int_member" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "ordinary_signed_int_member" (BG.Ptr Ordinary_signed_int_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_int_member")

instance HasCField.HasCField Ordinary_signed_int_struct "ordinary_signed_int_member" where

  type CFieldType Ordinary_signed_int_struct "ordinary_signed_int_member" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_int_struct@

    __defined at:__ @comprehensive\/c2hsc.h 217:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_int_struct = Explicit_signed_int_struct
  { explicit_signed_int_member :: BG.CInt
    {- ^ __C declaration:__ @explicit_signed_int_member@

         __defined at:__ @comprehensive\/c2hsc.h 217:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_int_struct where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Explicit_signed_int_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_int_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_int_member") ptr0

instance Marshal.WriteRaw Explicit_signed_int_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_int_struct explicit_signed_int_member2 ->
            HasCField.writeRaw (BG.Proxy @"explicit_signed_int_member") ptr0 explicit_signed_int_member2

deriving via Marshal.EquivStorable Explicit_signed_int_struct instance BG.Storable Explicit_signed_int_struct

deriving via Struct.IsStructViaStorable Explicit_signed_int_struct instance Struct.IsStruct Explicit_signed_int_struct

{-| __C declaration:__ @explicit_signed_int_member@

    __defined at:__ @comprehensive\/c2hsc.h 217:62@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "explicit_signed_int_member" Explicit_signed_int_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_int_struct {explicit_signed_int_member = y1}
      , BG.getField @"explicit_signed_int_member" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "explicit_signed_int_member" (BG.Ptr Explicit_signed_int_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_int_member")

instance HasCField.HasCField Explicit_signed_int_struct "explicit_signed_int_member" where

  type CFieldType Explicit_signed_int_struct "explicit_signed_int_member" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_int_struct@

    __defined at:__ @comprehensive\/c2hsc.h 218:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_int_struct = Unsigned_int_struct
  { unsigned_int_member :: BG.CUInt
    {- ^ __C declaration:__ @unsigned_int_member@

         __defined at:__ @comprehensive\/c2hsc.h 218:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_int_struct where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Unsigned_int_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_int_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_int_member") ptr0

instance Marshal.WriteRaw Unsigned_int_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_int_struct unsigned_int_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_int_member") ptr0 unsigned_int_member2

deriving via Marshal.EquivStorable Unsigned_int_struct instance BG.Storable Unsigned_int_struct

deriving via Struct.IsStructViaStorable Unsigned_int_struct instance Struct.IsStruct Unsigned_int_struct

{-| __C declaration:__ @unsigned_int_member@

    __defined at:__ @comprehensive\/c2hsc.h 218:62@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unsigned_int_member" Unsigned_int_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_int_struct {unsigned_int_member = y1}
      , BG.getField @"unsigned_int_member" x0
      )

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unsigned_int_member" (BG.Ptr Unsigned_int_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_int_member")

instance HasCField.HasCField Unsigned_int_struct "unsigned_int_member" where

  type CFieldType Unsigned_int_struct "unsigned_int_member" =
    BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_long_struct@

    __defined at:__ @comprehensive\/c2hsc.h 220:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_long_struct = Ordinary_signed_long_struct
  { ordinary_signed_long_member :: BG.CLong
    {- ^ __C declaration:__ @ordinary_signed_long_member@

         __defined at:__ @comprehensive\/c2hsc.h 220:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_long_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_long_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_long_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_long_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_long_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_long_struct ordinary_signed_long_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_signed_long_member") ptr0 ordinary_signed_long_member2

deriving via Marshal.EquivStorable Ordinary_signed_long_struct instance BG.Storable Ordinary_signed_long_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_long_struct instance Struct.IsStruct Ordinary_signed_long_struct

{-| __C declaration:__ @ordinary_signed_long_member@

    __defined at:__ @comprehensive\/c2hsc.h 220:62@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CLong
         ) => BG.CompatHasField.HasField "ordinary_signed_long_member" Ordinary_signed_long_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_long_struct {ordinary_signed_long_member = y1}
      , BG.getField @"ordinary_signed_long_member" x0
      )

instance ( ty ~ BG.CLong
         ) => BG.HasField "ordinary_signed_long_member" (BG.Ptr Ordinary_signed_long_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_long_member")

instance HasCField.HasCField Ordinary_signed_long_struct "ordinary_signed_long_member" where

  type CFieldType Ordinary_signed_long_struct "ordinary_signed_long_member" =
    BG.CLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_long_struct@

    __defined at:__ @comprehensive\/c2hsc.h 221:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_long_struct = Explicit_signed_long_struct
  { explicit_signed_long_member :: BG.CLong
    {- ^ __C declaration:__ @explicit_signed_long_member@

         __defined at:__ @comprehensive\/c2hsc.h 221:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_long_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_long_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_long_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_long_member") ptr0

instance Marshal.WriteRaw Explicit_signed_long_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_long_struct explicit_signed_long_member2 ->
            HasCField.writeRaw (BG.Proxy @"explicit_signed_long_member") ptr0 explicit_signed_long_member2

deriving via Marshal.EquivStorable Explicit_signed_long_struct instance BG.Storable Explicit_signed_long_struct

deriving via Struct.IsStructViaStorable Explicit_signed_long_struct instance Struct.IsStruct Explicit_signed_long_struct

{-| __C declaration:__ @explicit_signed_long_member@

    __defined at:__ @comprehensive\/c2hsc.h 221:62@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CLong
         ) => BG.CompatHasField.HasField "explicit_signed_long_member" Explicit_signed_long_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_long_struct {explicit_signed_long_member = y1}
      , BG.getField @"explicit_signed_long_member" x0
      )

instance ( ty ~ BG.CLong
         ) => BG.HasField "explicit_signed_long_member" (BG.Ptr Explicit_signed_long_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_long_member")

instance HasCField.HasCField Explicit_signed_long_struct "explicit_signed_long_member" where

  type CFieldType Explicit_signed_long_struct "explicit_signed_long_member" =
    BG.CLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_long_struct@

    __defined at:__ @comprehensive\/c2hsc.h 222:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_long_struct = Unsigned_long_struct
  { unsigned_long_member :: BG.CULong
    {- ^ __C declaration:__ @unsigned_long_member@

         __defined at:__ @comprehensive\/c2hsc.h 222:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_long_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_long_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_long_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_long_member") ptr0

instance Marshal.WriteRaw Unsigned_long_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_long_struct unsigned_long_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_long_member") ptr0 unsigned_long_member2

deriving via Marshal.EquivStorable Unsigned_long_struct instance BG.Storable Unsigned_long_struct

deriving via Struct.IsStructViaStorable Unsigned_long_struct instance Struct.IsStruct Unsigned_long_struct

{-| __C declaration:__ @unsigned_long_member@

    __defined at:__ @comprehensive\/c2hsc.h 222:62@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CULong
         ) => BG.CompatHasField.HasField "unsigned_long_member" Unsigned_long_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_long_struct {unsigned_long_member = y1}
      , BG.getField @"unsigned_long_member" x0
      )

instance ( ty ~ BG.CULong
         ) => BG.HasField "unsigned_long_member" (BG.Ptr Unsigned_long_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_long_member")

instance HasCField.HasCField Unsigned_long_struct "unsigned_long_member" where

  type CFieldType Unsigned_long_struct "unsigned_long_member" =
    BG.CULong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_long_long_struct@

    __defined at:__ @comprehensive\/c2hsc.h 224:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_long_long_struct = Ordinary_signed_long_long_struct
  { ordinary_signed_long_long_member :: BG.CLLong
    {- ^ __C declaration:__ @ordinary_signed_long_long_member@

         __defined at:__ @comprehensive\/c2hsc.h 224:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_long_long_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_long_long_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_long_long_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_long_long_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_long_long_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_long_long_struct ordinary_signed_long_long_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_signed_long_long_member") ptr0 ordinary_signed_long_long_member2

deriving via Marshal.EquivStorable Ordinary_signed_long_long_struct instance BG.Storable Ordinary_signed_long_long_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_long_long_struct instance Struct.IsStruct Ordinary_signed_long_long_struct

{-| __C declaration:__ @ordinary_signed_long_long_member@

    __defined at:__ @comprehensive\/c2hsc.h 224:62@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CLLong
         ) => BG.CompatHasField.HasField "ordinary_signed_long_long_member" Ordinary_signed_long_long_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_long_long_struct {ordinary_signed_long_long_member = y1}
      , BG.getField @"ordinary_signed_long_long_member" x0
      )

instance ( ty ~ BG.CLLong
         ) => BG.HasField "ordinary_signed_long_long_member" (BG.Ptr Ordinary_signed_long_long_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_long_long_member")

instance HasCField.HasCField Ordinary_signed_long_long_struct "ordinary_signed_long_long_member" where

  type CFieldType Ordinary_signed_long_long_struct "ordinary_signed_long_long_member" =
    BG.CLLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_long_long_struct@

    __defined at:__ @comprehensive\/c2hsc.h 225:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_long_long_struct = Explicit_signed_long_long_struct
  { explicit_signed_long_long_member :: BG.CLLong
    {- ^ __C declaration:__ @explicit_signed_long_long_member@

         __defined at:__ @comprehensive\/c2hsc.h 225:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_long_long_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_long_long_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_long_long_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_long_long_member") ptr0

instance Marshal.WriteRaw Explicit_signed_long_long_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_long_long_struct explicit_signed_long_long_member2 ->
            HasCField.writeRaw (BG.Proxy @"explicit_signed_long_long_member") ptr0 explicit_signed_long_long_member2

deriving via Marshal.EquivStorable Explicit_signed_long_long_struct instance BG.Storable Explicit_signed_long_long_struct

deriving via Struct.IsStructViaStorable Explicit_signed_long_long_struct instance Struct.IsStruct Explicit_signed_long_long_struct

{-| __C declaration:__ @explicit_signed_long_long_member@

    __defined at:__ @comprehensive\/c2hsc.h 225:62@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CLLong
         ) => BG.CompatHasField.HasField "explicit_signed_long_long_member" Explicit_signed_long_long_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_long_long_struct {explicit_signed_long_long_member = y1}
      , BG.getField @"explicit_signed_long_long_member" x0
      )

instance ( ty ~ BG.CLLong
         ) => BG.HasField "explicit_signed_long_long_member" (BG.Ptr Explicit_signed_long_long_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_long_long_member")

instance HasCField.HasCField Explicit_signed_long_long_struct "explicit_signed_long_long_member" where

  type CFieldType Explicit_signed_long_long_struct "explicit_signed_long_long_member" =
    BG.CLLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_long_long_struct@

    __defined at:__ @comprehensive\/c2hsc.h 226:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_long_long_struct = Unsigned_long_long_struct
  { unsigned_long_long_member :: BG.CULLong
    {- ^ __C declaration:__ @unsigned_long_long_member@

         __defined at:__ @comprehensive\/c2hsc.h 226:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_long_long_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_long_long_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_long_long_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_long_long_member") ptr0

instance Marshal.WriteRaw Unsigned_long_long_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_long_long_struct unsigned_long_long_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_long_long_member") ptr0 unsigned_long_long_member2

deriving via Marshal.EquivStorable Unsigned_long_long_struct instance BG.Storable Unsigned_long_long_struct

deriving via Struct.IsStructViaStorable Unsigned_long_long_struct instance Struct.IsStruct Unsigned_long_long_struct

{-| __C declaration:__ @unsigned_long_long_member@

    __defined at:__ @comprehensive\/c2hsc.h 226:62@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CULLong
         ) => BG.CompatHasField.HasField "unsigned_long_long_member" Unsigned_long_long_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_long_long_struct {unsigned_long_long_member = y1}
      , BG.getField @"unsigned_long_long_member" x0
      )

instance ( ty ~ BG.CULLong
         ) => BG.HasField "unsigned_long_long_member" (BG.Ptr Unsigned_long_long_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_long_long_member")

instance HasCField.HasCField Unsigned_long_long_struct "unsigned_long_long_member" where

  type CFieldType Unsigned_long_long_struct "unsigned_long_long_member" =
    BG.CULLong

  offset# = \_ -> \_ -> 0

{-| Structs: pointers

    NOTE: @'Ordinary_signed_char_pointer_struct'@ is commented out in the original test suite, unclear why (no reason is given).

    __C declaration:__ @struct ordinary_void_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 235:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_void_pointer_struct = Ordinary_void_pointer_struct
  { ordinary_void_pointer_member :: BG.Ptr BG.Void
    {- ^ __C declaration:__ @ordinary_void_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 235:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_void_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_void_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_void_pointer_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_void_pointer_member") ptr0

instance Marshal.WriteRaw Ordinary_void_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_void_pointer_struct ordinary_void_pointer_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_void_pointer_member") ptr0 ordinary_void_pointer_member2

deriving via Marshal.EquivStorable Ordinary_void_pointer_struct instance BG.Storable Ordinary_void_pointer_struct

deriving via Struct.IsStructViaStorable Ordinary_void_pointer_struct instance Struct.IsStruct Ordinary_void_pointer_struct

{-| __C declaration:__ @ordinary_void_pointer_member@

    __defined at:__ @comprehensive\/c2hsc.h 235:71@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.Ptr BG.Void
         ) => BG.CompatHasField.HasField "ordinary_void_pointer_member" Ordinary_void_pointer_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_void_pointer_struct {ordinary_void_pointer_member = y1}
      , BG.getField @"ordinary_void_pointer_member" x0
      )

instance ( ty ~ BG.Ptr BG.Void
         ) => BG.HasField "ordinary_void_pointer_member" (BG.Ptr Ordinary_void_pointer_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_void_pointer_member")

instance HasCField.HasCField Ordinary_void_pointer_struct "ordinary_void_pointer_member" where

  type CFieldType Ordinary_void_pointer_struct "ordinary_void_pointer_member" =
    BG.Ptr BG.Void

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_float_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 237:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_float_pointer_struct = Ordinary_float_pointer_struct
  { ordinary_float_pointer_member :: BG.Ptr BG.CFloat
    {- ^ __C declaration:__ @ordinary_float_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 237:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_float_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_float_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_float_pointer_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_float_pointer_member") ptr0

instance Marshal.WriteRaw Ordinary_float_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_float_pointer_struct ordinary_float_pointer_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_float_pointer_member") ptr0 ordinary_float_pointer_member2

deriving via Marshal.EquivStorable Ordinary_float_pointer_struct instance BG.Storable Ordinary_float_pointer_struct

deriving via Struct.IsStructViaStorable Ordinary_float_pointer_struct instance Struct.IsStruct Ordinary_float_pointer_struct

{-| __C declaration:__ @ordinary_float_pointer_member@

    __defined at:__ @comprehensive\/c2hsc.h 237:71@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.Ptr BG.CFloat
         ) => BG.CompatHasField.HasField "ordinary_float_pointer_member" Ordinary_float_pointer_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_float_pointer_struct {ordinary_float_pointer_member = y1}
      , BG.getField @"ordinary_float_pointer_member" x0
      )

instance ( ty ~ BG.Ptr BG.CFloat
         ) => BG.HasField "ordinary_float_pointer_member" (BG.Ptr Ordinary_float_pointer_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_float_pointer_member")

instance HasCField.HasCField Ordinary_float_pointer_struct "ordinary_float_pointer_member" where

  type CFieldType Ordinary_float_pointer_struct "ordinary_float_pointer_member" =
    BG.Ptr BG.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_double_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 238:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_double_pointer_struct = Ordinary_double_pointer_struct
  { ordinary_double_pointer_member :: BG.Ptr BG.CDouble
    {- ^ __C declaration:__ @ordinary_double_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 238:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_double_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_double_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_double_pointer_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_double_pointer_member") ptr0

instance Marshal.WriteRaw Ordinary_double_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_double_pointer_struct ordinary_double_pointer_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_double_pointer_member") ptr0 ordinary_double_pointer_member2

deriving via Marshal.EquivStorable Ordinary_double_pointer_struct instance BG.Storable Ordinary_double_pointer_struct

deriving via Struct.IsStructViaStorable Ordinary_double_pointer_struct instance Struct.IsStruct Ordinary_double_pointer_struct

{-| __C declaration:__ @ordinary_double_pointer_member@

    __defined at:__ @comprehensive\/c2hsc.h 238:71@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.Ptr BG.CDouble
         ) => BG.CompatHasField.HasField "ordinary_double_pointer_member" Ordinary_double_pointer_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_double_pointer_struct {ordinary_double_pointer_member = y1}
      , BG.getField @"ordinary_double_pointer_member" x0
      )

instance ( ty ~ BG.Ptr BG.CDouble
         ) => BG.HasField "ordinary_double_pointer_member" (BG.Ptr Ordinary_double_pointer_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_double_pointer_member")

instance HasCField.HasCField Ordinary_double_pointer_struct "ordinary_double_pointer_member" where

  type CFieldType Ordinary_double_pointer_struct "ordinary_double_pointer_member" =
    BG.Ptr BG.CDouble

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_char_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 241:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_char_pointer_struct = Ordinary_signed_char_pointer_struct
  { ordinary_signed_char_pointer_member :: BG.Ptr BG.CChar
    {- ^ __C declaration:__ @ordinary_signed_char_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 241:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_char_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_char_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_char_pointer_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_char_pointer_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_char_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_char_pointer_struct ordinary_signed_char_pointer_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_signed_char_pointer_member") ptr0 ordinary_signed_char_pointer_member2

deriving via Marshal.EquivStorable Ordinary_signed_char_pointer_struct instance BG.Storable Ordinary_signed_char_pointer_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_char_pointer_struct instance Struct.IsStruct Ordinary_signed_char_pointer_struct

{-| __C declaration:__ @ordinary_signed_char_pointer_member@

    __defined at:__ @comprehensive\/c2hsc.h 241:71@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.Ptr BG.CChar
         ) => BG.CompatHasField.HasField "ordinary_signed_char_pointer_member" Ordinary_signed_char_pointer_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_char_pointer_struct {ordinary_signed_char_pointer_member = y1}
      , BG.getField @"ordinary_signed_char_pointer_member" x0
      )

instance ( ty ~ BG.Ptr BG.CChar
         ) => BG.HasField "ordinary_signed_char_pointer_member" (BG.Ptr Ordinary_signed_char_pointer_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_char_pointer_member")

instance HasCField.HasCField Ordinary_signed_char_pointer_struct "ordinary_signed_char_pointer_member" where

  type CFieldType Ordinary_signed_char_pointer_struct "ordinary_signed_char_pointer_member" =
    BG.Ptr BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_char_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 242:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_char_pointer_struct = Explicit_signed_char_pointer_struct
  { explicit_signed_char_pointer_member :: BG.Ptr BG.CSChar
    {- ^ __C declaration:__ @explicit_signed_char_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 242:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_char_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_char_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_char_pointer_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_char_pointer_member") ptr0

instance Marshal.WriteRaw Explicit_signed_char_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_char_pointer_struct explicit_signed_char_pointer_member2 ->
            HasCField.writeRaw (BG.Proxy @"explicit_signed_char_pointer_member") ptr0 explicit_signed_char_pointer_member2

deriving via Marshal.EquivStorable Explicit_signed_char_pointer_struct instance BG.Storable Explicit_signed_char_pointer_struct

deriving via Struct.IsStructViaStorable Explicit_signed_char_pointer_struct instance Struct.IsStruct Explicit_signed_char_pointer_struct

{-| __C declaration:__ @explicit_signed_char_pointer_member@

    __defined at:__ @comprehensive\/c2hsc.h 242:71@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.Ptr BG.CSChar
         ) => BG.CompatHasField.HasField "explicit_signed_char_pointer_member" Explicit_signed_char_pointer_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_char_pointer_struct {explicit_signed_char_pointer_member = y1}
      , BG.getField @"explicit_signed_char_pointer_member" x0
      )

instance ( ty ~ BG.Ptr BG.CSChar
         ) => BG.HasField "explicit_signed_char_pointer_member" (BG.Ptr Explicit_signed_char_pointer_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_char_pointer_member")

instance HasCField.HasCField Explicit_signed_char_pointer_struct "explicit_signed_char_pointer_member" where

  type CFieldType Explicit_signed_char_pointer_struct "explicit_signed_char_pointer_member" =
    BG.Ptr BG.CSChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_char_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 243:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_char_pointer_struct = Unsigned_char_pointer_struct
  { unsigned_char_pointer_member :: BG.Ptr BG.CUChar
    {- ^ __C declaration:__ @unsigned_char_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 243:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_char_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_char_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_char_pointer_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_char_pointer_member") ptr0

instance Marshal.WriteRaw Unsigned_char_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_char_pointer_struct unsigned_char_pointer_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_char_pointer_member") ptr0 unsigned_char_pointer_member2

deriving via Marshal.EquivStorable Unsigned_char_pointer_struct instance BG.Storable Unsigned_char_pointer_struct

deriving via Struct.IsStructViaStorable Unsigned_char_pointer_struct instance Struct.IsStruct Unsigned_char_pointer_struct

{-| __C declaration:__ @unsigned_char_pointer_member@

    __defined at:__ @comprehensive\/c2hsc.h 243:71@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.Ptr BG.CUChar
         ) => BG.CompatHasField.HasField "unsigned_char_pointer_member" Unsigned_char_pointer_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_char_pointer_struct {unsigned_char_pointer_member = y1}
      , BG.getField @"unsigned_char_pointer_member" x0
      )

instance ( ty ~ BG.Ptr BG.CUChar
         ) => BG.HasField "unsigned_char_pointer_member" (BG.Ptr Unsigned_char_pointer_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_char_pointer_member")

instance HasCField.HasCField Unsigned_char_pointer_struct "unsigned_char_pointer_member" where

  type CFieldType Unsigned_char_pointer_struct "unsigned_char_pointer_member" =
    BG.Ptr BG.CUChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_short_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 245:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_short_pointer_struct = Ordinary_signed_short_pointer_struct
  { ordinary_signed_short_pointer_member :: BG.Ptr BG.CShort
    {- ^ __C declaration:__ @ordinary_signed_short_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 245:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_short_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_short_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_short_pointer_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_short_pointer_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_short_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_short_pointer_struct ordinary_signed_short_pointer_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_signed_short_pointer_member") ptr0 ordinary_signed_short_pointer_member2

deriving via Marshal.EquivStorable Ordinary_signed_short_pointer_struct instance BG.Storable Ordinary_signed_short_pointer_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_short_pointer_struct instance Struct.IsStruct Ordinary_signed_short_pointer_struct

{-| __C declaration:__ @ordinary_signed_short_pointer_member@

    __defined at:__ @comprehensive\/c2hsc.h 245:71@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.Ptr BG.CShort
         ) => BG.CompatHasField.HasField "ordinary_signed_short_pointer_member" Ordinary_signed_short_pointer_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_short_pointer_struct {ordinary_signed_short_pointer_member = y1}
      , BG.getField @"ordinary_signed_short_pointer_member" x0
      )

instance ( ty ~ BG.Ptr BG.CShort
         ) => BG.HasField "ordinary_signed_short_pointer_member" (BG.Ptr Ordinary_signed_short_pointer_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_short_pointer_member")

instance HasCField.HasCField Ordinary_signed_short_pointer_struct "ordinary_signed_short_pointer_member" where

  type CFieldType Ordinary_signed_short_pointer_struct "ordinary_signed_short_pointer_member" =
    BG.Ptr BG.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_short_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 246:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_short_pointer_struct = Explicit_signed_short_pointer_struct
  { explicit_signed_short_pointer_member :: BG.Ptr BG.CShort
    {- ^ __C declaration:__ @explicit_signed_short_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 246:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_short_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_short_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_short_pointer_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_short_pointer_member") ptr0

instance Marshal.WriteRaw Explicit_signed_short_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_short_pointer_struct explicit_signed_short_pointer_member2 ->
            HasCField.writeRaw (BG.Proxy @"explicit_signed_short_pointer_member") ptr0 explicit_signed_short_pointer_member2

deriving via Marshal.EquivStorable Explicit_signed_short_pointer_struct instance BG.Storable Explicit_signed_short_pointer_struct

deriving via Struct.IsStructViaStorable Explicit_signed_short_pointer_struct instance Struct.IsStruct Explicit_signed_short_pointer_struct

{-| __C declaration:__ @explicit_signed_short_pointer_member@

    __defined at:__ @comprehensive\/c2hsc.h 246:71@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.Ptr BG.CShort
         ) => BG.CompatHasField.HasField "explicit_signed_short_pointer_member" Explicit_signed_short_pointer_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_short_pointer_struct {explicit_signed_short_pointer_member = y1}
      , BG.getField @"explicit_signed_short_pointer_member" x0
      )

instance ( ty ~ BG.Ptr BG.CShort
         ) => BG.HasField "explicit_signed_short_pointer_member" (BG.Ptr Explicit_signed_short_pointer_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_short_pointer_member")

instance HasCField.HasCField Explicit_signed_short_pointer_struct "explicit_signed_short_pointer_member" where

  type CFieldType Explicit_signed_short_pointer_struct "explicit_signed_short_pointer_member" =
    BG.Ptr BG.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_short_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 247:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_short_pointer_struct = Unsigned_short_pointer_struct
  { unsigned_short_pointer_member :: BG.Ptr BG.CUShort
    {- ^ __C declaration:__ @unsigned_short_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 247:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_short_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_short_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_short_pointer_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_short_pointer_member") ptr0

instance Marshal.WriteRaw Unsigned_short_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_short_pointer_struct unsigned_short_pointer_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_short_pointer_member") ptr0 unsigned_short_pointer_member2

deriving via Marshal.EquivStorable Unsigned_short_pointer_struct instance BG.Storable Unsigned_short_pointer_struct

deriving via Struct.IsStructViaStorable Unsigned_short_pointer_struct instance Struct.IsStruct Unsigned_short_pointer_struct

{-| __C declaration:__ @unsigned_short_pointer_member@

    __defined at:__ @comprehensive\/c2hsc.h 247:71@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.Ptr BG.CUShort
         ) => BG.CompatHasField.HasField "unsigned_short_pointer_member" Unsigned_short_pointer_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_short_pointer_struct {unsigned_short_pointer_member = y1}
      , BG.getField @"unsigned_short_pointer_member" x0
      )

instance ( ty ~ BG.Ptr BG.CUShort
         ) => BG.HasField "unsigned_short_pointer_member" (BG.Ptr Unsigned_short_pointer_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_short_pointer_member")

instance HasCField.HasCField Unsigned_short_pointer_struct "unsigned_short_pointer_member" where

  type CFieldType Unsigned_short_pointer_struct "unsigned_short_pointer_member" =
    BG.Ptr BG.CUShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_int_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 249:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_int_pointer_struct = Ordinary_signed_int_pointer_struct
  { ordinary_signed_int_pointer_member :: BG.Ptr BG.CInt
    {- ^ __C declaration:__ @ordinary_signed_int_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 249:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_int_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_int_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_int_pointer_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_int_pointer_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_int_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_int_pointer_struct ordinary_signed_int_pointer_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_signed_int_pointer_member") ptr0 ordinary_signed_int_pointer_member2

deriving via Marshal.EquivStorable Ordinary_signed_int_pointer_struct instance BG.Storable Ordinary_signed_int_pointer_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_int_pointer_struct instance Struct.IsStruct Ordinary_signed_int_pointer_struct

{-| __C declaration:__ @ordinary_signed_int_pointer_member@

    __defined at:__ @comprehensive\/c2hsc.h 249:71@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.CompatHasField.HasField "ordinary_signed_int_pointer_member" Ordinary_signed_int_pointer_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_int_pointer_struct {ordinary_signed_int_pointer_member = y1}
      , BG.getField @"ordinary_signed_int_pointer_member" x0
      )

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.HasField "ordinary_signed_int_pointer_member" (BG.Ptr Ordinary_signed_int_pointer_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_int_pointer_member")

instance HasCField.HasCField Ordinary_signed_int_pointer_struct "ordinary_signed_int_pointer_member" where

  type CFieldType Ordinary_signed_int_pointer_struct "ordinary_signed_int_pointer_member" =
    BG.Ptr BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_int_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 250:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_int_pointer_struct = Explicit_signed_int_pointer_struct
  { explicit_signed_int_pointer_member :: BG.Ptr BG.CInt
    {- ^ __C declaration:__ @explicit_signed_int_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 250:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_int_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_int_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_int_pointer_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_int_pointer_member") ptr0

instance Marshal.WriteRaw Explicit_signed_int_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_int_pointer_struct explicit_signed_int_pointer_member2 ->
            HasCField.writeRaw (BG.Proxy @"explicit_signed_int_pointer_member") ptr0 explicit_signed_int_pointer_member2

deriving via Marshal.EquivStorable Explicit_signed_int_pointer_struct instance BG.Storable Explicit_signed_int_pointer_struct

deriving via Struct.IsStructViaStorable Explicit_signed_int_pointer_struct instance Struct.IsStruct Explicit_signed_int_pointer_struct

{-| __C declaration:__ @explicit_signed_int_pointer_member@

    __defined at:__ @comprehensive\/c2hsc.h 250:71@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.CompatHasField.HasField "explicit_signed_int_pointer_member" Explicit_signed_int_pointer_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_int_pointer_struct {explicit_signed_int_pointer_member = y1}
      , BG.getField @"explicit_signed_int_pointer_member" x0
      )

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.HasField "explicit_signed_int_pointer_member" (BG.Ptr Explicit_signed_int_pointer_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_int_pointer_member")

instance HasCField.HasCField Explicit_signed_int_pointer_struct "explicit_signed_int_pointer_member" where

  type CFieldType Explicit_signed_int_pointer_struct "explicit_signed_int_pointer_member" =
    BG.Ptr BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_int_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 251:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_int_pointer_struct = Unsigned_int_pointer_struct
  { unsigned_int_pointer_member :: BG.Ptr BG.CUInt
    {- ^ __C declaration:__ @unsigned_int_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 251:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_int_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_int_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_int_pointer_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_int_pointer_member") ptr0

instance Marshal.WriteRaw Unsigned_int_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_int_pointer_struct unsigned_int_pointer_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_int_pointer_member") ptr0 unsigned_int_pointer_member2

deriving via Marshal.EquivStorable Unsigned_int_pointer_struct instance BG.Storable Unsigned_int_pointer_struct

deriving via Struct.IsStructViaStorable Unsigned_int_pointer_struct instance Struct.IsStruct Unsigned_int_pointer_struct

{-| __C declaration:__ @unsigned_int_pointer_member@

    __defined at:__ @comprehensive\/c2hsc.h 251:71@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.Ptr BG.CUInt
         ) => BG.CompatHasField.HasField "unsigned_int_pointer_member" Unsigned_int_pointer_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_int_pointer_struct {unsigned_int_pointer_member = y1}
      , BG.getField @"unsigned_int_pointer_member" x0
      )

instance ( ty ~ BG.Ptr BG.CUInt
         ) => BG.HasField "unsigned_int_pointer_member" (BG.Ptr Unsigned_int_pointer_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_int_pointer_member")

instance HasCField.HasCField Unsigned_int_pointer_struct "unsigned_int_pointer_member" where

  type CFieldType Unsigned_int_pointer_struct "unsigned_int_pointer_member" =
    BG.Ptr BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_long_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 253:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_long_pointer_struct = Ordinary_signed_long_pointer_struct
  { ordinary_signed_long_pointer_member :: BG.Ptr BG.CLong
    {- ^ __C declaration:__ @ordinary_signed_long_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 253:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_long_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_long_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_long_pointer_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_long_pointer_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_long_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_long_pointer_struct ordinary_signed_long_pointer_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_signed_long_pointer_member") ptr0 ordinary_signed_long_pointer_member2

deriving via Marshal.EquivStorable Ordinary_signed_long_pointer_struct instance BG.Storable Ordinary_signed_long_pointer_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_long_pointer_struct instance Struct.IsStruct Ordinary_signed_long_pointer_struct

{-| __C declaration:__ @ordinary_signed_long_pointer_member@

    __defined at:__ @comprehensive\/c2hsc.h 253:71@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.Ptr BG.CLong
         ) => BG.CompatHasField.HasField "ordinary_signed_long_pointer_member" Ordinary_signed_long_pointer_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_long_pointer_struct {ordinary_signed_long_pointer_member = y1}
      , BG.getField @"ordinary_signed_long_pointer_member" x0
      )

instance ( ty ~ BG.Ptr BG.CLong
         ) => BG.HasField "ordinary_signed_long_pointer_member" (BG.Ptr Ordinary_signed_long_pointer_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_long_pointer_member")

instance HasCField.HasCField Ordinary_signed_long_pointer_struct "ordinary_signed_long_pointer_member" where

  type CFieldType Ordinary_signed_long_pointer_struct "ordinary_signed_long_pointer_member" =
    BG.Ptr BG.CLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_long_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 254:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_long_pointer_struct = Explicit_signed_long_pointer_struct
  { explicit_signed_long_pointer_member :: BG.Ptr BG.CLong
    {- ^ __C declaration:__ @explicit_signed_long_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 254:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_long_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_long_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_long_pointer_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_long_pointer_member") ptr0

instance Marshal.WriteRaw Explicit_signed_long_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_long_pointer_struct explicit_signed_long_pointer_member2 ->
            HasCField.writeRaw (BG.Proxy @"explicit_signed_long_pointer_member") ptr0 explicit_signed_long_pointer_member2

deriving via Marshal.EquivStorable Explicit_signed_long_pointer_struct instance BG.Storable Explicit_signed_long_pointer_struct

deriving via Struct.IsStructViaStorable Explicit_signed_long_pointer_struct instance Struct.IsStruct Explicit_signed_long_pointer_struct

{-| __C declaration:__ @explicit_signed_long_pointer_member@

    __defined at:__ @comprehensive\/c2hsc.h 254:71@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.Ptr BG.CLong
         ) => BG.CompatHasField.HasField "explicit_signed_long_pointer_member" Explicit_signed_long_pointer_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_long_pointer_struct {explicit_signed_long_pointer_member = y1}
      , BG.getField @"explicit_signed_long_pointer_member" x0
      )

instance ( ty ~ BG.Ptr BG.CLong
         ) => BG.HasField "explicit_signed_long_pointer_member" (BG.Ptr Explicit_signed_long_pointer_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_long_pointer_member")

instance HasCField.HasCField Explicit_signed_long_pointer_struct "explicit_signed_long_pointer_member" where

  type CFieldType Explicit_signed_long_pointer_struct "explicit_signed_long_pointer_member" =
    BG.Ptr BG.CLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_long_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 255:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_long_pointer_struct = Unsigned_long_pointer_struct
  { unsigned_long_pointer_member :: BG.Ptr BG.CULong
    {- ^ __C declaration:__ @unsigned_long_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 255:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_long_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_long_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_long_pointer_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_long_pointer_member") ptr0

instance Marshal.WriteRaw Unsigned_long_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_long_pointer_struct unsigned_long_pointer_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_long_pointer_member") ptr0 unsigned_long_pointer_member2

deriving via Marshal.EquivStorable Unsigned_long_pointer_struct instance BG.Storable Unsigned_long_pointer_struct

deriving via Struct.IsStructViaStorable Unsigned_long_pointer_struct instance Struct.IsStruct Unsigned_long_pointer_struct

{-| __C declaration:__ @unsigned_long_pointer_member@

    __defined at:__ @comprehensive\/c2hsc.h 255:71@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.Ptr BG.CULong
         ) => BG.CompatHasField.HasField "unsigned_long_pointer_member" Unsigned_long_pointer_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_long_pointer_struct {unsigned_long_pointer_member = y1}
      , BG.getField @"unsigned_long_pointer_member" x0
      )

instance ( ty ~ BG.Ptr BG.CULong
         ) => BG.HasField "unsigned_long_pointer_member" (BG.Ptr Unsigned_long_pointer_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_long_pointer_member")

instance HasCField.HasCField Unsigned_long_pointer_struct "unsigned_long_pointer_member" where

  type CFieldType Unsigned_long_pointer_struct "unsigned_long_pointer_member" =
    BG.Ptr BG.CULong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_long_long_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 257:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_long_long_pointer_struct = Ordinary_signed_long_long_pointer_struct
  { ordinary_signed_long_long_pointer_member :: BG.Ptr BG.CLLong
    {- ^ __C declaration:__ @ordinary_signed_long_long_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 257:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_long_long_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_long_long_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_long_long_pointer_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_long_long_pointer_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_long_long_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_long_long_pointer_struct
            ordinary_signed_long_long_pointer_member2 ->
              HasCField.writeRaw (BG.Proxy @"ordinary_signed_long_long_pointer_member") ptr0 ordinary_signed_long_long_pointer_member2

deriving via Marshal.EquivStorable Ordinary_signed_long_long_pointer_struct instance BG.Storable Ordinary_signed_long_long_pointer_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_long_long_pointer_struct instance Struct.IsStruct Ordinary_signed_long_long_pointer_struct

{-| __C declaration:__ @ordinary_signed_long_long_pointer_member@

    __defined at:__ @comprehensive\/c2hsc.h 257:71@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.Ptr BG.CLLong
         ) => BG.CompatHasField.HasField "ordinary_signed_long_long_pointer_member" Ordinary_signed_long_long_pointer_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_long_long_pointer_struct {ordinary_signed_long_long_pointer_member = y1}
      , BG.getField @"ordinary_signed_long_long_pointer_member" x0
      )

instance ( ty ~ BG.Ptr BG.CLLong
         ) => BG.HasField "ordinary_signed_long_long_pointer_member" (BG.Ptr Ordinary_signed_long_long_pointer_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_long_long_pointer_member")

instance HasCField.HasCField Ordinary_signed_long_long_pointer_struct "ordinary_signed_long_long_pointer_member" where

  type CFieldType Ordinary_signed_long_long_pointer_struct "ordinary_signed_long_long_pointer_member" =
    BG.Ptr BG.CLLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_long_long_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 258:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_long_long_pointer_struct = Explicit_signed_long_long_pointer_struct
  { explicit_signed_long_long_pointer_member :: BG.Ptr BG.CLLong
    {- ^ __C declaration:__ @explicit_signed_long_long_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 258:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_long_long_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_long_long_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_long_long_pointer_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_long_long_pointer_member") ptr0

instance Marshal.WriteRaw Explicit_signed_long_long_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_long_long_pointer_struct
            explicit_signed_long_long_pointer_member2 ->
              HasCField.writeRaw (BG.Proxy @"explicit_signed_long_long_pointer_member") ptr0 explicit_signed_long_long_pointer_member2

deriving via Marshal.EquivStorable Explicit_signed_long_long_pointer_struct instance BG.Storable Explicit_signed_long_long_pointer_struct

deriving via Struct.IsStructViaStorable Explicit_signed_long_long_pointer_struct instance Struct.IsStruct Explicit_signed_long_long_pointer_struct

{-| __C declaration:__ @explicit_signed_long_long_pointer_member@

    __defined at:__ @comprehensive\/c2hsc.h 258:71@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.Ptr BG.CLLong
         ) => BG.CompatHasField.HasField "explicit_signed_long_long_pointer_member" Explicit_signed_long_long_pointer_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_long_long_pointer_struct {explicit_signed_long_long_pointer_member = y1}
      , BG.getField @"explicit_signed_long_long_pointer_member" x0
      )

instance ( ty ~ BG.Ptr BG.CLLong
         ) => BG.HasField "explicit_signed_long_long_pointer_member" (BG.Ptr Explicit_signed_long_long_pointer_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_long_long_pointer_member")

instance HasCField.HasCField Explicit_signed_long_long_pointer_struct "explicit_signed_long_long_pointer_member" where

  type CFieldType Explicit_signed_long_long_pointer_struct "explicit_signed_long_long_pointer_member" =
    BG.Ptr BG.CLLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_long_long_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 259:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_long_long_pointer_struct = Unsigned_long_long_pointer_struct
  { unsigned_long_long_pointer_member :: BG.Ptr BG.CULLong
    {- ^ __C declaration:__ @unsigned_long_long_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 259:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_long_long_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_long_long_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_long_long_pointer_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_long_long_pointer_member") ptr0

instance Marshal.WriteRaw Unsigned_long_long_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_long_long_pointer_struct unsigned_long_long_pointer_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_long_long_pointer_member") ptr0 unsigned_long_long_pointer_member2

deriving via Marshal.EquivStorable Unsigned_long_long_pointer_struct instance BG.Storable Unsigned_long_long_pointer_struct

deriving via Struct.IsStructViaStorable Unsigned_long_long_pointer_struct instance Struct.IsStruct Unsigned_long_long_pointer_struct

{-| __C declaration:__ @unsigned_long_long_pointer_member@

    __defined at:__ @comprehensive\/c2hsc.h 259:71@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.Ptr BG.CULLong
         ) => BG.CompatHasField.HasField "unsigned_long_long_pointer_member" Unsigned_long_long_pointer_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_long_long_pointer_struct {unsigned_long_long_pointer_member = y1}
      , BG.getField @"unsigned_long_long_pointer_member" x0
      )

instance ( ty ~ BG.Ptr BG.CULLong
         ) => BG.HasField "unsigned_long_long_pointer_member" (BG.Ptr Unsigned_long_long_pointer_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_long_long_pointer_member")

instance HasCField.HasCField Unsigned_long_long_pointer_struct "unsigned_long_long_pointer_member" where

  type CFieldType Unsigned_long_long_pointer_struct "unsigned_long_long_pointer_member" =
    BG.Ptr BG.CULLong

  offset# = \_ -> \_ -> 0

{-| Structs: arrays

    __C declaration:__ @struct ordinary_float_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 265:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_float_array_struct = Ordinary_float_array_struct
  { ordinary_float_array_member :: CA.ConstantArray 10 BG.CFloat
    {- ^ __C declaration:__ @ordinary_float_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 265:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_float_array_struct where

  staticSizeOf = \_ -> (40 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Ordinary_float_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_float_array_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_float_array_member") ptr0

instance Marshal.WriteRaw Ordinary_float_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_float_array_struct ordinary_float_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_float_array_member") ptr0 ordinary_float_array_member2

deriving via Marshal.EquivStorable Ordinary_float_array_struct instance BG.Storable Ordinary_float_array_struct

deriving via Struct.IsStructViaStorable Ordinary_float_array_struct instance Struct.IsStruct Ordinary_float_array_struct

{-| __C declaration:__ @ordinary_float_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 265:68@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 BG.CFloat
         ) => BG.CompatHasField.HasField "ordinary_float_array_member" Ordinary_float_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_float_array_struct {ordinary_float_array_member = y1}
      , BG.getField @"ordinary_float_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 BG.CFloat
         ) => BG.HasField "ordinary_float_array_member" (BG.Ptr Ordinary_float_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_float_array_member")

instance HasCField.HasCField Ordinary_float_array_struct "ordinary_float_array_member" where

  type CFieldType Ordinary_float_array_struct "ordinary_float_array_member" =
    CA.ConstantArray 10 BG.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_double_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 266:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_double_array_struct = Ordinary_double_array_struct
  { ordinary_double_array_member :: CA.ConstantArray 10 BG.CDouble
    {- ^ __C declaration:__ @ordinary_double_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 266:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_double_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_double_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_double_array_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_double_array_member") ptr0

instance Marshal.WriteRaw Ordinary_double_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_double_array_struct ordinary_double_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_double_array_member") ptr0 ordinary_double_array_member2

deriving via Marshal.EquivStorable Ordinary_double_array_struct instance BG.Storable Ordinary_double_array_struct

deriving via Struct.IsStructViaStorable Ordinary_double_array_struct instance Struct.IsStruct Ordinary_double_array_struct

{-| __C declaration:__ @ordinary_double_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 266:68@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 BG.CDouble
         ) => BG.CompatHasField.HasField "ordinary_double_array_member" Ordinary_double_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_double_array_struct {ordinary_double_array_member = y1}
      , BG.getField @"ordinary_double_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 BG.CDouble
         ) => BG.HasField "ordinary_double_array_member" (BG.Ptr Ordinary_double_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_double_array_member")

instance HasCField.HasCField Ordinary_double_array_struct "ordinary_double_array_member" where

  type CFieldType Ordinary_double_array_struct "ordinary_double_array_member" =
    CA.ConstantArray 10 BG.CDouble

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_char_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 269:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_char_array_struct = Ordinary_signed_char_array_struct
  { ordinary_signed_char_array_member :: CA.ConstantArray 10 BG.CChar
    {- ^ __C declaration:__ @ordinary_signed_char_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 269:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_char_array_struct where

  staticSizeOf = \_ -> (10 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Ordinary_signed_char_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_char_array_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_char_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_char_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_char_array_struct ordinary_signed_char_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_signed_char_array_member") ptr0 ordinary_signed_char_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_char_array_struct instance BG.Storable Ordinary_signed_char_array_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_char_array_struct instance Struct.IsStruct Ordinary_signed_char_array_struct

{-| __C declaration:__ @ordinary_signed_char_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 269:68@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 BG.CChar
         ) => BG.CompatHasField.HasField "ordinary_signed_char_array_member" Ordinary_signed_char_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_char_array_struct {ordinary_signed_char_array_member = y1}
      , BG.getField @"ordinary_signed_char_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 BG.CChar
         ) => BG.HasField "ordinary_signed_char_array_member" (BG.Ptr Ordinary_signed_char_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_char_array_member")

instance HasCField.HasCField Ordinary_signed_char_array_struct "ordinary_signed_char_array_member" where

  type CFieldType Ordinary_signed_char_array_struct "ordinary_signed_char_array_member" =
    CA.ConstantArray 10 BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_char_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 270:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_char_array_struct = Explicit_signed_char_array_struct
  { explicit_signed_char_array_member :: CA.ConstantArray 10 BG.CSChar
    {- ^ __C declaration:__ @explicit_signed_char_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 270:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_char_array_struct where

  staticSizeOf = \_ -> (10 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Explicit_signed_char_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_char_array_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_char_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_char_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_char_array_struct explicit_signed_char_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"explicit_signed_char_array_member") ptr0 explicit_signed_char_array_member2

deriving via Marshal.EquivStorable Explicit_signed_char_array_struct instance BG.Storable Explicit_signed_char_array_struct

deriving via Struct.IsStructViaStorable Explicit_signed_char_array_struct instance Struct.IsStruct Explicit_signed_char_array_struct

{-| __C declaration:__ @explicit_signed_char_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 270:68@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 BG.CSChar
         ) => BG.CompatHasField.HasField "explicit_signed_char_array_member" Explicit_signed_char_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_char_array_struct {explicit_signed_char_array_member = y1}
      , BG.getField @"explicit_signed_char_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 BG.CSChar
         ) => BG.HasField "explicit_signed_char_array_member" (BG.Ptr Explicit_signed_char_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_char_array_member")

instance HasCField.HasCField Explicit_signed_char_array_struct "explicit_signed_char_array_member" where

  type CFieldType Explicit_signed_char_array_struct "explicit_signed_char_array_member" =
    CA.ConstantArray 10 BG.CSChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_char_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 271:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_char_array_struct = Unsigned_char_array_struct
  { unsigned_char_array_member :: CA.ConstantArray 10 BG.CUChar
    {- ^ __C declaration:__ @unsigned_char_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 271:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_char_array_struct where

  staticSizeOf = \_ -> (10 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Unsigned_char_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_char_array_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_char_array_member") ptr0

instance Marshal.WriteRaw Unsigned_char_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_char_array_struct unsigned_char_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_char_array_member") ptr0 unsigned_char_array_member2

deriving via Marshal.EquivStorable Unsigned_char_array_struct instance BG.Storable Unsigned_char_array_struct

deriving via Struct.IsStructViaStorable Unsigned_char_array_struct instance Struct.IsStruct Unsigned_char_array_struct

{-| __C declaration:__ @unsigned_char_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 271:68@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 BG.CUChar
         ) => BG.CompatHasField.HasField "unsigned_char_array_member" Unsigned_char_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_char_array_struct {unsigned_char_array_member = y1}
      , BG.getField @"unsigned_char_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 BG.CUChar
         ) => BG.HasField "unsigned_char_array_member" (BG.Ptr Unsigned_char_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_char_array_member")

instance HasCField.HasCField Unsigned_char_array_struct "unsigned_char_array_member" where

  type CFieldType Unsigned_char_array_struct "unsigned_char_array_member" =
    CA.ConstantArray 10 BG.CUChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_short_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 273:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_short_array_struct = Ordinary_signed_short_array_struct
  { ordinary_signed_short_array_member :: CA.ConstantArray 10 BG.CShort
    {- ^ __C declaration:__ @ordinary_signed_short_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 273:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_short_array_struct where

  staticSizeOf = \_ -> (20 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Ordinary_signed_short_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_short_array_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_short_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_short_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_short_array_struct ordinary_signed_short_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_signed_short_array_member") ptr0 ordinary_signed_short_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_short_array_struct instance BG.Storable Ordinary_signed_short_array_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_short_array_struct instance Struct.IsStruct Ordinary_signed_short_array_struct

{-| __C declaration:__ @ordinary_signed_short_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 273:68@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 BG.CShort
         ) => BG.CompatHasField.HasField "ordinary_signed_short_array_member" Ordinary_signed_short_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_short_array_struct {ordinary_signed_short_array_member = y1}
      , BG.getField @"ordinary_signed_short_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 BG.CShort
         ) => BG.HasField "ordinary_signed_short_array_member" (BG.Ptr Ordinary_signed_short_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_short_array_member")

instance HasCField.HasCField Ordinary_signed_short_array_struct "ordinary_signed_short_array_member" where

  type CFieldType Ordinary_signed_short_array_struct "ordinary_signed_short_array_member" =
    CA.ConstantArray 10 BG.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_short_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 274:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_short_array_struct = Explicit_signed_short_array_struct
  { explicit_signed_short_array_member :: CA.ConstantArray 10 BG.CShort
    {- ^ __C declaration:__ @explicit_signed_short_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 274:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_short_array_struct where

  staticSizeOf = \_ -> (20 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Explicit_signed_short_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_short_array_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_short_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_short_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_short_array_struct explicit_signed_short_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"explicit_signed_short_array_member") ptr0 explicit_signed_short_array_member2

deriving via Marshal.EquivStorable Explicit_signed_short_array_struct instance BG.Storable Explicit_signed_short_array_struct

deriving via Struct.IsStructViaStorable Explicit_signed_short_array_struct instance Struct.IsStruct Explicit_signed_short_array_struct

{-| __C declaration:__ @explicit_signed_short_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 274:68@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 BG.CShort
         ) => BG.CompatHasField.HasField "explicit_signed_short_array_member" Explicit_signed_short_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_short_array_struct {explicit_signed_short_array_member = y1}
      , BG.getField @"explicit_signed_short_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 BG.CShort
         ) => BG.HasField "explicit_signed_short_array_member" (BG.Ptr Explicit_signed_short_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_short_array_member")

instance HasCField.HasCField Explicit_signed_short_array_struct "explicit_signed_short_array_member" where

  type CFieldType Explicit_signed_short_array_struct "explicit_signed_short_array_member" =
    CA.ConstantArray 10 BG.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_short_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 275:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_short_array_struct = Unsigned_short_array_struct
  { unsigned_short_array_member :: CA.ConstantArray 10 BG.CUShort
    {- ^ __C declaration:__ @unsigned_short_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 275:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_short_array_struct where

  staticSizeOf = \_ -> (20 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Unsigned_short_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_short_array_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_short_array_member") ptr0

instance Marshal.WriteRaw Unsigned_short_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_short_array_struct unsigned_short_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_short_array_member") ptr0 unsigned_short_array_member2

deriving via Marshal.EquivStorable Unsigned_short_array_struct instance BG.Storable Unsigned_short_array_struct

deriving via Struct.IsStructViaStorable Unsigned_short_array_struct instance Struct.IsStruct Unsigned_short_array_struct

{-| __C declaration:__ @unsigned_short_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 275:68@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 BG.CUShort
         ) => BG.CompatHasField.HasField "unsigned_short_array_member" Unsigned_short_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_short_array_struct {unsigned_short_array_member = y1}
      , BG.getField @"unsigned_short_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 BG.CUShort
         ) => BG.HasField "unsigned_short_array_member" (BG.Ptr Unsigned_short_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_short_array_member")

instance HasCField.HasCField Unsigned_short_array_struct "unsigned_short_array_member" where

  type CFieldType Unsigned_short_array_struct "unsigned_short_array_member" =
    CA.ConstantArray 10 BG.CUShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_int_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 277:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_int_array_struct = Ordinary_signed_int_array_struct
  { ordinary_signed_int_array_member :: CA.ConstantArray 10 BG.CInt
    {- ^ __C declaration:__ @ordinary_signed_int_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 277:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_int_array_struct where

  staticSizeOf = \_ -> (40 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Ordinary_signed_int_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_int_array_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_int_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_int_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_int_array_struct ordinary_signed_int_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_signed_int_array_member") ptr0 ordinary_signed_int_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_int_array_struct instance BG.Storable Ordinary_signed_int_array_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_int_array_struct instance Struct.IsStruct Ordinary_signed_int_array_struct

{-| __C declaration:__ @ordinary_signed_int_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 277:68@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 BG.CInt
         ) => BG.CompatHasField.HasField "ordinary_signed_int_array_member" Ordinary_signed_int_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_int_array_struct {ordinary_signed_int_array_member = y1}
      , BG.getField @"ordinary_signed_int_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 BG.CInt
         ) => BG.HasField "ordinary_signed_int_array_member" (BG.Ptr Ordinary_signed_int_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_int_array_member")

instance HasCField.HasCField Ordinary_signed_int_array_struct "ordinary_signed_int_array_member" where

  type CFieldType Ordinary_signed_int_array_struct "ordinary_signed_int_array_member" =
    CA.ConstantArray 10 BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_int_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 278:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_int_array_struct = Explicit_signed_int_array_struct
  { explicit_signed_int_array_member :: CA.ConstantArray 10 BG.CInt
    {- ^ __C declaration:__ @explicit_signed_int_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 278:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_int_array_struct where

  staticSizeOf = \_ -> (40 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Explicit_signed_int_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_int_array_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_int_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_int_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_int_array_struct explicit_signed_int_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"explicit_signed_int_array_member") ptr0 explicit_signed_int_array_member2

deriving via Marshal.EquivStorable Explicit_signed_int_array_struct instance BG.Storable Explicit_signed_int_array_struct

deriving via Struct.IsStructViaStorable Explicit_signed_int_array_struct instance Struct.IsStruct Explicit_signed_int_array_struct

{-| __C declaration:__ @explicit_signed_int_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 278:68@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 BG.CInt
         ) => BG.CompatHasField.HasField "explicit_signed_int_array_member" Explicit_signed_int_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_int_array_struct {explicit_signed_int_array_member = y1}
      , BG.getField @"explicit_signed_int_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 BG.CInt
         ) => BG.HasField "explicit_signed_int_array_member" (BG.Ptr Explicit_signed_int_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_int_array_member")

instance HasCField.HasCField Explicit_signed_int_array_struct "explicit_signed_int_array_member" where

  type CFieldType Explicit_signed_int_array_struct "explicit_signed_int_array_member" =
    CA.ConstantArray 10 BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_int_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 279:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_int_array_struct = Unsigned_int_array_struct
  { unsigned_int_array_member :: CA.ConstantArray 10 BG.CUInt
    {- ^ __C declaration:__ @unsigned_int_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 279:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_int_array_struct where

  staticSizeOf = \_ -> (40 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Unsigned_int_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_int_array_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_int_array_member") ptr0

instance Marshal.WriteRaw Unsigned_int_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_int_array_struct unsigned_int_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_int_array_member") ptr0 unsigned_int_array_member2

deriving via Marshal.EquivStorable Unsigned_int_array_struct instance BG.Storable Unsigned_int_array_struct

deriving via Struct.IsStructViaStorable Unsigned_int_array_struct instance Struct.IsStruct Unsigned_int_array_struct

{-| __C declaration:__ @unsigned_int_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 279:68@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 BG.CUInt
         ) => BG.CompatHasField.HasField "unsigned_int_array_member" Unsigned_int_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_int_array_struct {unsigned_int_array_member = y1}
      , BG.getField @"unsigned_int_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 BG.CUInt
         ) => BG.HasField "unsigned_int_array_member" (BG.Ptr Unsigned_int_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_int_array_member")

instance HasCField.HasCField Unsigned_int_array_struct "unsigned_int_array_member" where

  type CFieldType Unsigned_int_array_struct "unsigned_int_array_member" =
    CA.ConstantArray 10 BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_long_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 281:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_long_array_struct = Ordinary_signed_long_array_struct
  { ordinary_signed_long_array_member :: CA.ConstantArray 10 BG.CLong
    {- ^ __C declaration:__ @ordinary_signed_long_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 281:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_long_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_long_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_long_array_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_long_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_long_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_long_array_struct ordinary_signed_long_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_signed_long_array_member") ptr0 ordinary_signed_long_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_long_array_struct instance BG.Storable Ordinary_signed_long_array_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_long_array_struct instance Struct.IsStruct Ordinary_signed_long_array_struct

{-| __C declaration:__ @ordinary_signed_long_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 281:68@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 BG.CLong
         ) => BG.CompatHasField.HasField "ordinary_signed_long_array_member" Ordinary_signed_long_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_long_array_struct {ordinary_signed_long_array_member = y1}
      , BG.getField @"ordinary_signed_long_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 BG.CLong
         ) => BG.HasField "ordinary_signed_long_array_member" (BG.Ptr Ordinary_signed_long_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_long_array_member")

instance HasCField.HasCField Ordinary_signed_long_array_struct "ordinary_signed_long_array_member" where

  type CFieldType Ordinary_signed_long_array_struct "ordinary_signed_long_array_member" =
    CA.ConstantArray 10 BG.CLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_long_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 282:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_long_array_struct = Explicit_signed_long_array_struct
  { explicit_signed_long_array_member :: CA.ConstantArray 10 BG.CLong
    {- ^ __C declaration:__ @explicit_signed_long_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 282:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_long_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_long_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_long_array_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_long_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_long_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_long_array_struct explicit_signed_long_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"explicit_signed_long_array_member") ptr0 explicit_signed_long_array_member2

deriving via Marshal.EquivStorable Explicit_signed_long_array_struct instance BG.Storable Explicit_signed_long_array_struct

deriving via Struct.IsStructViaStorable Explicit_signed_long_array_struct instance Struct.IsStruct Explicit_signed_long_array_struct

{-| __C declaration:__ @explicit_signed_long_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 282:68@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 BG.CLong
         ) => BG.CompatHasField.HasField "explicit_signed_long_array_member" Explicit_signed_long_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_long_array_struct {explicit_signed_long_array_member = y1}
      , BG.getField @"explicit_signed_long_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 BG.CLong
         ) => BG.HasField "explicit_signed_long_array_member" (BG.Ptr Explicit_signed_long_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_long_array_member")

instance HasCField.HasCField Explicit_signed_long_array_struct "explicit_signed_long_array_member" where

  type CFieldType Explicit_signed_long_array_struct "explicit_signed_long_array_member" =
    CA.ConstantArray 10 BG.CLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_long_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 283:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_long_array_struct = Unsigned_long_array_struct
  { unsigned_long_array_member :: CA.ConstantArray 10 BG.CULong
    {- ^ __C declaration:__ @unsigned_long_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 283:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_long_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_long_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_long_array_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_long_array_member") ptr0

instance Marshal.WriteRaw Unsigned_long_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_long_array_struct unsigned_long_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_long_array_member") ptr0 unsigned_long_array_member2

deriving via Marshal.EquivStorable Unsigned_long_array_struct instance BG.Storable Unsigned_long_array_struct

deriving via Struct.IsStructViaStorable Unsigned_long_array_struct instance Struct.IsStruct Unsigned_long_array_struct

{-| __C declaration:__ @unsigned_long_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 283:68@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 BG.CULong
         ) => BG.CompatHasField.HasField "unsigned_long_array_member" Unsigned_long_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_long_array_struct {unsigned_long_array_member = y1}
      , BG.getField @"unsigned_long_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 BG.CULong
         ) => BG.HasField "unsigned_long_array_member" (BG.Ptr Unsigned_long_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_long_array_member")

instance HasCField.HasCField Unsigned_long_array_struct "unsigned_long_array_member" where

  type CFieldType Unsigned_long_array_struct "unsigned_long_array_member" =
    CA.ConstantArray 10 BG.CULong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_long_long_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 285:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_long_long_array_struct = Ordinary_signed_long_long_array_struct
  { ordinary_signed_long_long_array_member :: CA.ConstantArray 10 BG.CLLong
    {- ^ __C declaration:__ @ordinary_signed_long_long_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 285:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_long_long_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_long_long_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_long_long_array_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_long_long_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_long_long_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_long_long_array_struct
            ordinary_signed_long_long_array_member2 ->
              HasCField.writeRaw (BG.Proxy @"ordinary_signed_long_long_array_member") ptr0 ordinary_signed_long_long_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_long_long_array_struct instance BG.Storable Ordinary_signed_long_long_array_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_long_long_array_struct instance Struct.IsStruct Ordinary_signed_long_long_array_struct

{-| __C declaration:__ @ordinary_signed_long_long_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 285:68@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 BG.CLLong
         ) => BG.CompatHasField.HasField "ordinary_signed_long_long_array_member" Ordinary_signed_long_long_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_long_long_array_struct {ordinary_signed_long_long_array_member = y1}
      , BG.getField @"ordinary_signed_long_long_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 BG.CLLong
         ) => BG.HasField "ordinary_signed_long_long_array_member" (BG.Ptr Ordinary_signed_long_long_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_long_long_array_member")

instance HasCField.HasCField Ordinary_signed_long_long_array_struct "ordinary_signed_long_long_array_member" where

  type CFieldType Ordinary_signed_long_long_array_struct "ordinary_signed_long_long_array_member" =
    CA.ConstantArray 10 BG.CLLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_long_long_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 286:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_long_long_array_struct = Explicit_signed_long_long_array_struct
  { explicit_signed_long_long_array_member :: CA.ConstantArray 10 BG.CLLong
    {- ^ __C declaration:__ @explicit_signed_long_long_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 286:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_long_long_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_long_long_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_long_long_array_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_long_long_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_long_long_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_long_long_array_struct
            explicit_signed_long_long_array_member2 ->
              HasCField.writeRaw (BG.Proxy @"explicit_signed_long_long_array_member") ptr0 explicit_signed_long_long_array_member2

deriving via Marshal.EquivStorable Explicit_signed_long_long_array_struct instance BG.Storable Explicit_signed_long_long_array_struct

deriving via Struct.IsStructViaStorable Explicit_signed_long_long_array_struct instance Struct.IsStruct Explicit_signed_long_long_array_struct

{-| __C declaration:__ @explicit_signed_long_long_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 286:68@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 BG.CLLong
         ) => BG.CompatHasField.HasField "explicit_signed_long_long_array_member" Explicit_signed_long_long_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_long_long_array_struct {explicit_signed_long_long_array_member = y1}
      , BG.getField @"explicit_signed_long_long_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 BG.CLLong
         ) => BG.HasField "explicit_signed_long_long_array_member" (BG.Ptr Explicit_signed_long_long_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_long_long_array_member")

instance HasCField.HasCField Explicit_signed_long_long_array_struct "explicit_signed_long_long_array_member" where

  type CFieldType Explicit_signed_long_long_array_struct "explicit_signed_long_long_array_member" =
    CA.ConstantArray 10 BG.CLLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_long_long_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 287:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_long_long_array_struct = Unsigned_long_long_array_struct
  { unsigned_long_long_array_member :: CA.ConstantArray 10 BG.CULLong
    {- ^ __C declaration:__ @unsigned_long_long_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 287:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_long_long_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_long_long_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_long_long_array_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_long_long_array_member") ptr0

instance Marshal.WriteRaw Unsigned_long_long_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_long_long_array_struct unsigned_long_long_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_long_long_array_member") ptr0 unsigned_long_long_array_member2

deriving via Marshal.EquivStorable Unsigned_long_long_array_struct instance BG.Storable Unsigned_long_long_array_struct

deriving via Struct.IsStructViaStorable Unsigned_long_long_array_struct instance Struct.IsStruct Unsigned_long_long_array_struct

{-| __C declaration:__ @unsigned_long_long_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 287:68@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 BG.CULLong
         ) => BG.CompatHasField.HasField "unsigned_long_long_array_member" Unsigned_long_long_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_long_long_array_struct {unsigned_long_long_array_member = y1}
      , BG.getField @"unsigned_long_long_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 BG.CULLong
         ) => BG.HasField "unsigned_long_long_array_member" (BG.Ptr Unsigned_long_long_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_long_long_array_member")

instance HasCField.HasCField Unsigned_long_long_array_struct "unsigned_long_long_array_member" where

  type CFieldType Unsigned_long_long_array_struct "unsigned_long_long_array_member" =
    CA.ConstantArray 10 BG.CULLong

  offset# = \_ -> \_ -> 0

{-| Structs: arrays of pointers

    NOTE: Here too @'Ordinary_signed_char_pointer_array_struct'@ was commented out in the original test suite, with no reason given.

    __C declaration:__ @struct ordinary_void_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 296:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_void_pointer_array_struct = Ordinary_void_pointer_array_struct
  { ordinary_void_pointer_array_member :: CA.ConstantArray 10 (BG.Ptr BG.Void)
    {- ^ __C declaration:__ @ordinary_void_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 296:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_void_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_void_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_void_pointer_array_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_void_pointer_array_member") ptr0

instance Marshal.WriteRaw Ordinary_void_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_void_pointer_array_struct ordinary_void_pointer_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_void_pointer_array_member") ptr0 ordinary_void_pointer_array_member2

deriving via Marshal.EquivStorable Ordinary_void_pointer_array_struct instance BG.Storable Ordinary_void_pointer_array_struct

deriving via Struct.IsStructViaStorable Ordinary_void_pointer_array_struct instance Struct.IsStruct Ordinary_void_pointer_array_struct

{-| __C declaration:__ @ordinary_void_pointer_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 296:77@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.Void)
         ) => BG.CompatHasField.HasField "ordinary_void_pointer_array_member" Ordinary_void_pointer_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_void_pointer_array_struct {ordinary_void_pointer_array_member = y1}
      , BG.getField @"ordinary_void_pointer_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.Void)
         ) => BG.HasField "ordinary_void_pointer_array_member" (BG.Ptr Ordinary_void_pointer_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_void_pointer_array_member")

instance HasCField.HasCField Ordinary_void_pointer_array_struct "ordinary_void_pointer_array_member" where

  type CFieldType Ordinary_void_pointer_array_struct "ordinary_void_pointer_array_member" =
    CA.ConstantArray 10 (BG.Ptr BG.Void)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_float_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 298:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_float_pointer_array_struct = Ordinary_float_pointer_array_struct
  { ordinary_float_pointer_array_member :: CA.ConstantArray 10 (BG.Ptr BG.CFloat)
    {- ^ __C declaration:__ @ordinary_float_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 298:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_float_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_float_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_float_pointer_array_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_float_pointer_array_member") ptr0

instance Marshal.WriteRaw Ordinary_float_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_float_pointer_array_struct ordinary_float_pointer_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_float_pointer_array_member") ptr0 ordinary_float_pointer_array_member2

deriving via Marshal.EquivStorable Ordinary_float_pointer_array_struct instance BG.Storable Ordinary_float_pointer_array_struct

deriving via Struct.IsStructViaStorable Ordinary_float_pointer_array_struct instance Struct.IsStruct Ordinary_float_pointer_array_struct

{-| __C declaration:__ @ordinary_float_pointer_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 298:77@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CFloat)
         ) => BG.CompatHasField.HasField "ordinary_float_pointer_array_member" Ordinary_float_pointer_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_float_pointer_array_struct {ordinary_float_pointer_array_member = y1}
      , BG.getField @"ordinary_float_pointer_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CFloat)
         ) => BG.HasField "ordinary_float_pointer_array_member" (BG.Ptr Ordinary_float_pointer_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_float_pointer_array_member")

instance HasCField.HasCField Ordinary_float_pointer_array_struct "ordinary_float_pointer_array_member" where

  type CFieldType Ordinary_float_pointer_array_struct "ordinary_float_pointer_array_member" =
    CA.ConstantArray 10 (BG.Ptr BG.CFloat)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_double_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 299:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_double_pointer_array_struct = Ordinary_double_pointer_array_struct
  { ordinary_double_pointer_array_member :: CA.ConstantArray 10 (BG.Ptr BG.CDouble)
    {- ^ __C declaration:__ @ordinary_double_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 299:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_double_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_double_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_double_pointer_array_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_double_pointer_array_member") ptr0

instance Marshal.WriteRaw Ordinary_double_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_double_pointer_array_struct ordinary_double_pointer_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"ordinary_double_pointer_array_member") ptr0 ordinary_double_pointer_array_member2

deriving via Marshal.EquivStorable Ordinary_double_pointer_array_struct instance BG.Storable Ordinary_double_pointer_array_struct

deriving via Struct.IsStructViaStorable Ordinary_double_pointer_array_struct instance Struct.IsStruct Ordinary_double_pointer_array_struct

{-| __C declaration:__ @ordinary_double_pointer_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 299:77@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CDouble)
         ) => BG.CompatHasField.HasField "ordinary_double_pointer_array_member" Ordinary_double_pointer_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_double_pointer_array_struct {ordinary_double_pointer_array_member = y1}
      , BG.getField @"ordinary_double_pointer_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CDouble)
         ) => BG.HasField "ordinary_double_pointer_array_member" (BG.Ptr Ordinary_double_pointer_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_double_pointer_array_member")

instance HasCField.HasCField Ordinary_double_pointer_array_struct "ordinary_double_pointer_array_member" where

  type CFieldType Ordinary_double_pointer_array_struct "ordinary_double_pointer_array_member" =
    CA.ConstantArray 10 (BG.Ptr BG.CDouble)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_char_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 302:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_char_pointer_array_struct = Ordinary_signed_char_pointer_array_struct
  { ordinary_signed_char_pointer_array_member :: CA.ConstantArray 10 (BG.Ptr BG.CChar)
    {- ^ __C declaration:__ @ordinary_signed_char_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 302:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_char_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_char_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_char_pointer_array_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_char_pointer_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_char_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_char_pointer_array_struct
            ordinary_signed_char_pointer_array_member2 ->
              HasCField.writeRaw (BG.Proxy @"ordinary_signed_char_pointer_array_member") ptr0 ordinary_signed_char_pointer_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_char_pointer_array_struct instance BG.Storable Ordinary_signed_char_pointer_array_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_char_pointer_array_struct instance Struct.IsStruct Ordinary_signed_char_pointer_array_struct

{-| __C declaration:__ @ordinary_signed_char_pointer_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 302:77@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CChar)
         ) => BG.CompatHasField.HasField "ordinary_signed_char_pointer_array_member" Ordinary_signed_char_pointer_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_char_pointer_array_struct {ordinary_signed_char_pointer_array_member = y1}
      , BG.getField @"ordinary_signed_char_pointer_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CChar)
         ) => BG.HasField "ordinary_signed_char_pointer_array_member" (BG.Ptr Ordinary_signed_char_pointer_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_char_pointer_array_member")

instance HasCField.HasCField Ordinary_signed_char_pointer_array_struct "ordinary_signed_char_pointer_array_member" where

  type CFieldType Ordinary_signed_char_pointer_array_struct "ordinary_signed_char_pointer_array_member" =
    CA.ConstantArray 10 (BG.Ptr BG.CChar)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_char_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 303:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_char_pointer_array_struct = Explicit_signed_char_pointer_array_struct
  { explicit_signed_char_pointer_array_member :: CA.ConstantArray 10 (BG.Ptr BG.CSChar)
    {- ^ __C declaration:__ @explicit_signed_char_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 303:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_char_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_char_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_char_pointer_array_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_char_pointer_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_char_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_char_pointer_array_struct
            explicit_signed_char_pointer_array_member2 ->
              HasCField.writeRaw (BG.Proxy @"explicit_signed_char_pointer_array_member") ptr0 explicit_signed_char_pointer_array_member2

deriving via Marshal.EquivStorable Explicit_signed_char_pointer_array_struct instance BG.Storable Explicit_signed_char_pointer_array_struct

deriving via Struct.IsStructViaStorable Explicit_signed_char_pointer_array_struct instance Struct.IsStruct Explicit_signed_char_pointer_array_struct

{-| __C declaration:__ @explicit_signed_char_pointer_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 303:77@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CSChar)
         ) => BG.CompatHasField.HasField "explicit_signed_char_pointer_array_member" Explicit_signed_char_pointer_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_char_pointer_array_struct {explicit_signed_char_pointer_array_member = y1}
      , BG.getField @"explicit_signed_char_pointer_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CSChar)
         ) => BG.HasField "explicit_signed_char_pointer_array_member" (BG.Ptr Explicit_signed_char_pointer_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_char_pointer_array_member")

instance HasCField.HasCField Explicit_signed_char_pointer_array_struct "explicit_signed_char_pointer_array_member" where

  type CFieldType Explicit_signed_char_pointer_array_struct "explicit_signed_char_pointer_array_member" =
    CA.ConstantArray 10 (BG.Ptr BG.CSChar)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_char_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 304:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_char_pointer_array_struct = Unsigned_char_pointer_array_struct
  { unsigned_char_pointer_array_member :: CA.ConstantArray 10 (BG.Ptr BG.CUChar)
    {- ^ __C declaration:__ @unsigned_char_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 304:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_char_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_char_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_char_pointer_array_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_char_pointer_array_member") ptr0

instance Marshal.WriteRaw Unsigned_char_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_char_pointer_array_struct unsigned_char_pointer_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_char_pointer_array_member") ptr0 unsigned_char_pointer_array_member2

deriving via Marshal.EquivStorable Unsigned_char_pointer_array_struct instance BG.Storable Unsigned_char_pointer_array_struct

deriving via Struct.IsStructViaStorable Unsigned_char_pointer_array_struct instance Struct.IsStruct Unsigned_char_pointer_array_struct

{-| __C declaration:__ @unsigned_char_pointer_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 304:77@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CUChar)
         ) => BG.CompatHasField.HasField "unsigned_char_pointer_array_member" Unsigned_char_pointer_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_char_pointer_array_struct {unsigned_char_pointer_array_member = y1}
      , BG.getField @"unsigned_char_pointer_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CUChar)
         ) => BG.HasField "unsigned_char_pointer_array_member" (BG.Ptr Unsigned_char_pointer_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_char_pointer_array_member")

instance HasCField.HasCField Unsigned_char_pointer_array_struct "unsigned_char_pointer_array_member" where

  type CFieldType Unsigned_char_pointer_array_struct "unsigned_char_pointer_array_member" =
    CA.ConstantArray 10 (BG.Ptr BG.CUChar)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_short_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 306:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_short_pointer_array_struct = Ordinary_signed_short_pointer_array_struct
  { ordinary_signed_short_pointer_array_member :: CA.ConstantArray 10 (BG.Ptr BG.CShort)
    {- ^ __C declaration:__ @ordinary_signed_short_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 306:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_short_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_short_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_short_pointer_array_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_short_pointer_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_short_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_short_pointer_array_struct
            ordinary_signed_short_pointer_array_member2 ->
              HasCField.writeRaw (BG.Proxy @"ordinary_signed_short_pointer_array_member") ptr0 ordinary_signed_short_pointer_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_short_pointer_array_struct instance BG.Storable Ordinary_signed_short_pointer_array_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_short_pointer_array_struct instance Struct.IsStruct Ordinary_signed_short_pointer_array_struct

{-| __C declaration:__ @ordinary_signed_short_pointer_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 306:77@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CShort)
         ) => BG.CompatHasField.HasField "ordinary_signed_short_pointer_array_member" Ordinary_signed_short_pointer_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_short_pointer_array_struct {ordinary_signed_short_pointer_array_member = y1}
      , BG.getField @"ordinary_signed_short_pointer_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CShort)
         ) => BG.HasField "ordinary_signed_short_pointer_array_member" (BG.Ptr Ordinary_signed_short_pointer_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_short_pointer_array_member")

instance HasCField.HasCField Ordinary_signed_short_pointer_array_struct "ordinary_signed_short_pointer_array_member" where

  type CFieldType Ordinary_signed_short_pointer_array_struct "ordinary_signed_short_pointer_array_member" =
    CA.ConstantArray 10 (BG.Ptr BG.CShort)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_short_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 307:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_short_pointer_array_struct = Explicit_signed_short_pointer_array_struct
  { explicit_signed_short_pointer_array_member :: CA.ConstantArray 10 (BG.Ptr BG.CShort)
    {- ^ __C declaration:__ @explicit_signed_short_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 307:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_short_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_short_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_short_pointer_array_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_short_pointer_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_short_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_short_pointer_array_struct
            explicit_signed_short_pointer_array_member2 ->
              HasCField.writeRaw (BG.Proxy @"explicit_signed_short_pointer_array_member") ptr0 explicit_signed_short_pointer_array_member2

deriving via Marshal.EquivStorable Explicit_signed_short_pointer_array_struct instance BG.Storable Explicit_signed_short_pointer_array_struct

deriving via Struct.IsStructViaStorable Explicit_signed_short_pointer_array_struct instance Struct.IsStruct Explicit_signed_short_pointer_array_struct

{-| __C declaration:__ @explicit_signed_short_pointer_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 307:77@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CShort)
         ) => BG.CompatHasField.HasField "explicit_signed_short_pointer_array_member" Explicit_signed_short_pointer_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_short_pointer_array_struct {explicit_signed_short_pointer_array_member = y1}
      , BG.getField @"explicit_signed_short_pointer_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CShort)
         ) => BG.HasField "explicit_signed_short_pointer_array_member" (BG.Ptr Explicit_signed_short_pointer_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_short_pointer_array_member")

instance HasCField.HasCField Explicit_signed_short_pointer_array_struct "explicit_signed_short_pointer_array_member" where

  type CFieldType Explicit_signed_short_pointer_array_struct "explicit_signed_short_pointer_array_member" =
    CA.ConstantArray 10 (BG.Ptr BG.CShort)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_short_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 308:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_short_pointer_array_struct = Unsigned_short_pointer_array_struct
  { unsigned_short_pointer_array_member :: CA.ConstantArray 10 (BG.Ptr BG.CUShort)
    {- ^ __C declaration:__ @unsigned_short_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 308:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_short_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_short_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_short_pointer_array_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_short_pointer_array_member") ptr0

instance Marshal.WriteRaw Unsigned_short_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_short_pointer_array_struct unsigned_short_pointer_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_short_pointer_array_member") ptr0 unsigned_short_pointer_array_member2

deriving via Marshal.EquivStorable Unsigned_short_pointer_array_struct instance BG.Storable Unsigned_short_pointer_array_struct

deriving via Struct.IsStructViaStorable Unsigned_short_pointer_array_struct instance Struct.IsStruct Unsigned_short_pointer_array_struct

{-| __C declaration:__ @unsigned_short_pointer_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 308:77@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CUShort)
         ) => BG.CompatHasField.HasField "unsigned_short_pointer_array_member" Unsigned_short_pointer_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_short_pointer_array_struct {unsigned_short_pointer_array_member = y1}
      , BG.getField @"unsigned_short_pointer_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CUShort)
         ) => BG.HasField "unsigned_short_pointer_array_member" (BG.Ptr Unsigned_short_pointer_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_short_pointer_array_member")

instance HasCField.HasCField Unsigned_short_pointer_array_struct "unsigned_short_pointer_array_member" where

  type CFieldType Unsigned_short_pointer_array_struct "unsigned_short_pointer_array_member" =
    CA.ConstantArray 10 (BG.Ptr BG.CUShort)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_int_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 310:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_int_pointer_array_struct = Ordinary_signed_int_pointer_array_struct
  { ordinary_signed_int_pointer_array_member :: CA.ConstantArray 10 (BG.Ptr BG.CInt)
    {- ^ __C declaration:__ @ordinary_signed_int_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 310:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_int_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_int_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_int_pointer_array_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_int_pointer_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_int_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_int_pointer_array_struct
            ordinary_signed_int_pointer_array_member2 ->
              HasCField.writeRaw (BG.Proxy @"ordinary_signed_int_pointer_array_member") ptr0 ordinary_signed_int_pointer_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_int_pointer_array_struct instance BG.Storable Ordinary_signed_int_pointer_array_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_int_pointer_array_struct instance Struct.IsStruct Ordinary_signed_int_pointer_array_struct

{-| __C declaration:__ @ordinary_signed_int_pointer_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 310:77@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CInt)
         ) => BG.CompatHasField.HasField "ordinary_signed_int_pointer_array_member" Ordinary_signed_int_pointer_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_int_pointer_array_struct {ordinary_signed_int_pointer_array_member = y1}
      , BG.getField @"ordinary_signed_int_pointer_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CInt)
         ) => BG.HasField "ordinary_signed_int_pointer_array_member" (BG.Ptr Ordinary_signed_int_pointer_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_int_pointer_array_member")

instance HasCField.HasCField Ordinary_signed_int_pointer_array_struct "ordinary_signed_int_pointer_array_member" where

  type CFieldType Ordinary_signed_int_pointer_array_struct "ordinary_signed_int_pointer_array_member" =
    CA.ConstantArray 10 (BG.Ptr BG.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_int_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 311:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_int_pointer_array_struct = Explicit_signed_int_pointer_array_struct
  { explicit_signed_int_pointer_array_member :: CA.ConstantArray 10 (BG.Ptr BG.CInt)
    {- ^ __C declaration:__ @explicit_signed_int_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 311:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_int_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_int_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_int_pointer_array_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_int_pointer_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_int_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_int_pointer_array_struct
            explicit_signed_int_pointer_array_member2 ->
              HasCField.writeRaw (BG.Proxy @"explicit_signed_int_pointer_array_member") ptr0 explicit_signed_int_pointer_array_member2

deriving via Marshal.EquivStorable Explicit_signed_int_pointer_array_struct instance BG.Storable Explicit_signed_int_pointer_array_struct

deriving via Struct.IsStructViaStorable Explicit_signed_int_pointer_array_struct instance Struct.IsStruct Explicit_signed_int_pointer_array_struct

{-| __C declaration:__ @explicit_signed_int_pointer_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 311:77@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CInt)
         ) => BG.CompatHasField.HasField "explicit_signed_int_pointer_array_member" Explicit_signed_int_pointer_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_int_pointer_array_struct {explicit_signed_int_pointer_array_member = y1}
      , BG.getField @"explicit_signed_int_pointer_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CInt)
         ) => BG.HasField "explicit_signed_int_pointer_array_member" (BG.Ptr Explicit_signed_int_pointer_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_int_pointer_array_member")

instance HasCField.HasCField Explicit_signed_int_pointer_array_struct "explicit_signed_int_pointer_array_member" where

  type CFieldType Explicit_signed_int_pointer_array_struct "explicit_signed_int_pointer_array_member" =
    CA.ConstantArray 10 (BG.Ptr BG.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_int_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 312:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_int_pointer_array_struct = Unsigned_int_pointer_array_struct
  { unsigned_int_pointer_array_member :: CA.ConstantArray 10 (BG.Ptr BG.CUInt)
    {- ^ __C declaration:__ @unsigned_int_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 312:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_int_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_int_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_int_pointer_array_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_int_pointer_array_member") ptr0

instance Marshal.WriteRaw Unsigned_int_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_int_pointer_array_struct unsigned_int_pointer_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_int_pointer_array_member") ptr0 unsigned_int_pointer_array_member2

deriving via Marshal.EquivStorable Unsigned_int_pointer_array_struct instance BG.Storable Unsigned_int_pointer_array_struct

deriving via Struct.IsStructViaStorable Unsigned_int_pointer_array_struct instance Struct.IsStruct Unsigned_int_pointer_array_struct

{-| __C declaration:__ @unsigned_int_pointer_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 312:77@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CUInt)
         ) => BG.CompatHasField.HasField "unsigned_int_pointer_array_member" Unsigned_int_pointer_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_int_pointer_array_struct {unsigned_int_pointer_array_member = y1}
      , BG.getField @"unsigned_int_pointer_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CUInt)
         ) => BG.HasField "unsigned_int_pointer_array_member" (BG.Ptr Unsigned_int_pointer_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_int_pointer_array_member")

instance HasCField.HasCField Unsigned_int_pointer_array_struct "unsigned_int_pointer_array_member" where

  type CFieldType Unsigned_int_pointer_array_struct "unsigned_int_pointer_array_member" =
    CA.ConstantArray 10 (BG.Ptr BG.CUInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_long_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 314:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_long_pointer_array_struct = Ordinary_signed_long_pointer_array_struct
  { ordinary_signed_long_pointer_array_member :: CA.ConstantArray 10 (BG.Ptr BG.CLong)
    {- ^ __C declaration:__ @ordinary_signed_long_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 314:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_long_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_long_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_long_pointer_array_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_long_pointer_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_long_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_long_pointer_array_struct
            ordinary_signed_long_pointer_array_member2 ->
              HasCField.writeRaw (BG.Proxy @"ordinary_signed_long_pointer_array_member") ptr0 ordinary_signed_long_pointer_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_long_pointer_array_struct instance BG.Storable Ordinary_signed_long_pointer_array_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_long_pointer_array_struct instance Struct.IsStruct Ordinary_signed_long_pointer_array_struct

{-| __C declaration:__ @ordinary_signed_long_pointer_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 314:77@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CLong)
         ) => BG.CompatHasField.HasField "ordinary_signed_long_pointer_array_member" Ordinary_signed_long_pointer_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_long_pointer_array_struct {ordinary_signed_long_pointer_array_member = y1}
      , BG.getField @"ordinary_signed_long_pointer_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CLong)
         ) => BG.HasField "ordinary_signed_long_pointer_array_member" (BG.Ptr Ordinary_signed_long_pointer_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_long_pointer_array_member")

instance HasCField.HasCField Ordinary_signed_long_pointer_array_struct "ordinary_signed_long_pointer_array_member" where

  type CFieldType Ordinary_signed_long_pointer_array_struct "ordinary_signed_long_pointer_array_member" =
    CA.ConstantArray 10 (BG.Ptr BG.CLong)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_long_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 315:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_long_pointer_array_struct = Explicit_signed_long_pointer_array_struct
  { explicit_signed_long_pointer_array_member :: CA.ConstantArray 10 (BG.Ptr BG.CLong)
    {- ^ __C declaration:__ @explicit_signed_long_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 315:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_long_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_long_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_long_pointer_array_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_long_pointer_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_long_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_long_pointer_array_struct
            explicit_signed_long_pointer_array_member2 ->
              HasCField.writeRaw (BG.Proxy @"explicit_signed_long_pointer_array_member") ptr0 explicit_signed_long_pointer_array_member2

deriving via Marshal.EquivStorable Explicit_signed_long_pointer_array_struct instance BG.Storable Explicit_signed_long_pointer_array_struct

deriving via Struct.IsStructViaStorable Explicit_signed_long_pointer_array_struct instance Struct.IsStruct Explicit_signed_long_pointer_array_struct

{-| __C declaration:__ @explicit_signed_long_pointer_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 315:77@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CLong)
         ) => BG.CompatHasField.HasField "explicit_signed_long_pointer_array_member" Explicit_signed_long_pointer_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_long_pointer_array_struct {explicit_signed_long_pointer_array_member = y1}
      , BG.getField @"explicit_signed_long_pointer_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CLong)
         ) => BG.HasField "explicit_signed_long_pointer_array_member" (BG.Ptr Explicit_signed_long_pointer_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_long_pointer_array_member")

instance HasCField.HasCField Explicit_signed_long_pointer_array_struct "explicit_signed_long_pointer_array_member" where

  type CFieldType Explicit_signed_long_pointer_array_struct "explicit_signed_long_pointer_array_member" =
    CA.ConstantArray 10 (BG.Ptr BG.CLong)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_long_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 316:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_long_pointer_array_struct = Unsigned_long_pointer_array_struct
  { unsigned_long_pointer_array_member :: CA.ConstantArray 10 (BG.Ptr BG.CULong)
    {- ^ __C declaration:__ @unsigned_long_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 316:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_long_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_long_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_long_pointer_array_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_long_pointer_array_member") ptr0

instance Marshal.WriteRaw Unsigned_long_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_long_pointer_array_struct unsigned_long_pointer_array_member2 ->
            HasCField.writeRaw (BG.Proxy @"unsigned_long_pointer_array_member") ptr0 unsigned_long_pointer_array_member2

deriving via Marshal.EquivStorable Unsigned_long_pointer_array_struct instance BG.Storable Unsigned_long_pointer_array_struct

deriving via Struct.IsStructViaStorable Unsigned_long_pointer_array_struct instance Struct.IsStruct Unsigned_long_pointer_array_struct

{-| __C declaration:__ @unsigned_long_pointer_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 316:77@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CULong)
         ) => BG.CompatHasField.HasField "unsigned_long_pointer_array_member" Unsigned_long_pointer_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_long_pointer_array_struct {unsigned_long_pointer_array_member = y1}
      , BG.getField @"unsigned_long_pointer_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CULong)
         ) => BG.HasField "unsigned_long_pointer_array_member" (BG.Ptr Unsigned_long_pointer_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_long_pointer_array_member")

instance HasCField.HasCField Unsigned_long_pointer_array_struct "unsigned_long_pointer_array_member" where

  type CFieldType Unsigned_long_pointer_array_struct "unsigned_long_pointer_array_member" =
    CA.ConstantArray 10 (BG.Ptr BG.CULong)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ordinary_signed_long_long_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 318:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_long_long_pointer_array_struct = Ordinary_signed_long_long_pointer_array_struct
  { ordinary_signed_long_long_pointer_array_member :: CA.ConstantArray 10 (BG.Ptr BG.CLLong)
    {- ^ __C declaration:__ @ordinary_signed_long_long_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 318:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_long_long_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_long_long_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_long_long_pointer_array_struct
      <*> HasCField.readRaw (BG.Proxy @"ordinary_signed_long_long_pointer_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_long_long_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_long_long_pointer_array_struct
            ordinary_signed_long_long_pointer_array_member2 ->
              HasCField.writeRaw (BG.Proxy @"ordinary_signed_long_long_pointer_array_member") ptr0 ordinary_signed_long_long_pointer_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_long_long_pointer_array_struct instance BG.Storable Ordinary_signed_long_long_pointer_array_struct

deriving via Struct.IsStructViaStorable Ordinary_signed_long_long_pointer_array_struct instance Struct.IsStruct Ordinary_signed_long_long_pointer_array_struct

{-| __C declaration:__ @ordinary_signed_long_long_pointer_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 318:77@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CLLong)
         ) => BG.CompatHasField.HasField "ordinary_signed_long_long_pointer_array_member" Ordinary_signed_long_long_pointer_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ordinary_signed_long_long_pointer_array_struct {ordinary_signed_long_long_pointer_array_member = y1}
      , BG.getField @"ordinary_signed_long_long_pointer_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CLLong)
         ) => BG.HasField "ordinary_signed_long_long_pointer_array_member" (BG.Ptr Ordinary_signed_long_long_pointer_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ordinary_signed_long_long_pointer_array_member")

instance HasCField.HasCField Ordinary_signed_long_long_pointer_array_struct "ordinary_signed_long_long_pointer_array_member" where

  type CFieldType Ordinary_signed_long_long_pointer_array_struct "ordinary_signed_long_long_pointer_array_member" =
    CA.ConstantArray 10 (BG.Ptr BG.CLLong)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct explicit_signed_long_long_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 319:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_long_long_pointer_array_struct = Explicit_signed_long_long_pointer_array_struct
  { explicit_signed_long_long_pointer_array_member :: CA.ConstantArray 10 (BG.Ptr BG.CLLong)
    {- ^ __C declaration:__ @explicit_signed_long_long_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 319:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Explicit_signed_long_long_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_long_long_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_long_long_pointer_array_struct
      <*> HasCField.readRaw (BG.Proxy @"explicit_signed_long_long_pointer_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_long_long_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_long_long_pointer_array_struct
            explicit_signed_long_long_pointer_array_member2 ->
              HasCField.writeRaw (BG.Proxy @"explicit_signed_long_long_pointer_array_member") ptr0 explicit_signed_long_long_pointer_array_member2

deriving via Marshal.EquivStorable Explicit_signed_long_long_pointer_array_struct instance BG.Storable Explicit_signed_long_long_pointer_array_struct

deriving via Struct.IsStructViaStorable Explicit_signed_long_long_pointer_array_struct instance Struct.IsStruct Explicit_signed_long_long_pointer_array_struct

{-| __C declaration:__ @explicit_signed_long_long_pointer_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 319:77@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CLLong)
         ) => BG.CompatHasField.HasField "explicit_signed_long_long_pointer_array_member" Explicit_signed_long_long_pointer_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Explicit_signed_long_long_pointer_array_struct {explicit_signed_long_long_pointer_array_member = y1}
      , BG.getField @"explicit_signed_long_long_pointer_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CLLong)
         ) => BG.HasField "explicit_signed_long_long_pointer_array_member" (BG.Ptr Explicit_signed_long_long_pointer_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"explicit_signed_long_long_pointer_array_member")

instance HasCField.HasCField Explicit_signed_long_long_pointer_array_struct "explicit_signed_long_long_pointer_array_member" where

  type CFieldType Explicit_signed_long_long_pointer_array_struct "explicit_signed_long_long_pointer_array_member" =
    CA.ConstantArray 10 (BG.Ptr BG.CLLong)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct unsigned_long_long_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 320:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_long_long_pointer_array_struct = Unsigned_long_long_pointer_array_struct
  { unsigned_long_long_pointer_array_member :: CA.ConstantArray 10 (BG.Ptr BG.CULLong)
    {- ^ __C declaration:__ @unsigned_long_long_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 320:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Unsigned_long_long_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_long_long_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_long_long_pointer_array_struct
      <*> HasCField.readRaw (BG.Proxy @"unsigned_long_long_pointer_array_member") ptr0

instance Marshal.WriteRaw Unsigned_long_long_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_long_long_pointer_array_struct
            unsigned_long_long_pointer_array_member2 ->
              HasCField.writeRaw (BG.Proxy @"unsigned_long_long_pointer_array_member") ptr0 unsigned_long_long_pointer_array_member2

deriving via Marshal.EquivStorable Unsigned_long_long_pointer_array_struct instance BG.Storable Unsigned_long_long_pointer_array_struct

deriving via Struct.IsStructViaStorable Unsigned_long_long_pointer_array_struct instance Struct.IsStruct Unsigned_long_long_pointer_array_struct

{-| __C declaration:__ @unsigned_long_long_pointer_array_member@

    __defined at:__ @comprehensive\/c2hsc.h 320:77@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CULLong)
         ) => BG.CompatHasField.HasField "unsigned_long_long_pointer_array_member" Unsigned_long_long_pointer_array_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Unsigned_long_long_pointer_array_struct {unsigned_long_long_pointer_array_member = y1}
      , BG.getField @"unsigned_long_long_pointer_array_member" x0
      )

instance ( ty ~ CA.ConstantArray 10 (BG.Ptr BG.CULLong)
         ) => BG.HasField "unsigned_long_long_pointer_array_member" (BG.Ptr Unsigned_long_long_pointer_array_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unsigned_long_long_pointer_array_member")

instance HasCField.HasCField Unsigned_long_long_pointer_array_struct "unsigned_long_long_pointer_array_member" where

  type CFieldType Unsigned_long_long_pointer_array_struct "unsigned_long_long_pointer_array_member" =
    CA.ConstantArray 10 (BG.Ptr BG.CULLong)

  offset# = \_ -> \_ -> 0

{-| Sanity checks

    NOTE: The @smoke.h@ test is moved to a separate header.

    __C declaration:__ @an_int@

    __defined at:__ @comprehensive\/c2hsc.h 329:13@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype An_int = An_int
  { unwrap :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrap" An_int ty where

  hasField =
    \x0 ->
      (\y1 ->
         An_int {unwrap = y1}, BG.getField @"unwrap" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrap" (BG.Ptr An_int) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrap")

instance HasCField.HasCField An_int "unwrap" where

  type CFieldType An_int "unwrap" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@cal_table_table@

    __defined at:__ @comprehensive\/c2hsc.h 341:5@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Cal_table_table = Cal_table_table
  { raw :: BG.CInt
    {- ^ __C declaration:__ @raw@

         __defined at:__ @comprehensive\/c2hsc.h 342:13@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  , val :: BG.CInt
    {- ^ __C declaration:__ @val@

         __defined at:__ @comprehensive\/c2hsc.h 342:22@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Cal_table_table where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Cal_table_table where

  readRaw =
    \ptr0 ->
          pure Cal_table_table
      <*> HasCField.readRaw (BG.Proxy @"raw") ptr0
      <*> HasCField.readRaw (BG.Proxy @"val") ptr0

instance Marshal.WriteRaw Cal_table_table where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Cal_table_table raw2 val3 ->
               HasCField.writeRaw (BG.Proxy @"raw") ptr0 raw2
            >> HasCField.writeRaw (BG.Proxy @"val") ptr0 val3

deriving via Marshal.EquivStorable Cal_table_table instance BG.Storable Cal_table_table

deriving via Struct.IsStructViaStorable Cal_table_table instance Struct.IsStruct Cal_table_table

{-| __C declaration:__ @raw@

    __defined at:__ @comprehensive\/c2hsc.h 342:13@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "raw" Cal_table_table ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Cal_table_table {raw = y1, val = BG.getField @"val" x0}
      , BG.getField @"raw" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "raw" (BG.Ptr Cal_table_table) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"raw")

instance HasCField.HasCField Cal_table_table "raw" where

  type CFieldType Cal_table_table "raw" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @val@

    __defined at:__ @comprehensive\/c2hsc.h 342:22@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "val" Cal_table_table ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Cal_table_table {val = y1, raw = BG.getField @"raw" x0}
      , BG.getField @"val" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "val" (BG.Ptr Cal_table_table) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"val")

instance HasCField.HasCField Cal_table_table "val" where

  type CFieldType Cal_table_table "val" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| Issues without test cases in the original test suite

    These are examples from open issues on the c2hsc repository, that don't (yet) have corresponding test cases in the c2hsc test suite.

    __C declaration:__ @struct cal_table@

    __defined at:__ @comprehensive\/c2hsc.h 339:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Cal_table = Cal_table
  { size :: BG.CInt
    {- ^ __C declaration:__ @size@

         __defined at:__ @comprehensive\/c2hsc.h 340:9@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  , table :: CA.ConstantArray 32 Cal_table_table
    {- ^ __C declaration:__ @table@

         __defined at:__ @comprehensive\/c2hsc.h 343:7@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Cal_table where

  staticSizeOf = \_ -> (260 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Cal_table where

  readRaw =
    \ptr0 ->
          pure Cal_table
      <*> HasCField.readRaw (BG.Proxy @"size") ptr0
      <*> HasCField.readRaw (BG.Proxy @"table") ptr0

instance Marshal.WriteRaw Cal_table where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Cal_table size2 table3 ->
               HasCField.writeRaw (BG.Proxy @"size") ptr0 size2
            >> HasCField.writeRaw (BG.Proxy @"table") ptr0 table3

deriving via Marshal.EquivStorable Cal_table instance BG.Storable Cal_table

deriving via Struct.IsStructViaStorable Cal_table instance Struct.IsStruct Cal_table

{-| __C declaration:__ @size@

    __defined at:__ @comprehensive\/c2hsc.h 340:9@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "size" Cal_table ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Cal_table {size = y1, table = BG.getField @"table" x0}
      , BG.getField @"size" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "size" (BG.Ptr Cal_table) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"size")

instance HasCField.HasCField Cal_table "size" where

  type CFieldType Cal_table "size" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @table@

    __defined at:__ @comprehensive\/c2hsc.h 343:7@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 32 Cal_table_table
         ) => BG.CompatHasField.HasField "table" Cal_table ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Cal_table {table = y1, size = BG.getField @"size" x0}
      , BG.getField @"table" x0
      )

instance ( ty ~ CA.ConstantArray 32 Cal_table_table
         ) => BG.HasField "table" (BG.Ptr Cal_table) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"table")

instance HasCField.HasCField Cal_table "table" where

  type CFieldType Cal_table "table" =
    CA.ConstantArray 32 Cal_table_table

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @union \@Elf32_External_Dyn_d_un@

    __defined at:__ @comprehensive\/c2hsc.h 349:3@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype Elf32_External_Dyn_d_un = Elf32_External_Dyn_d_un
  { unwrap :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 1 instance Marshal.StaticSize Elf32_External_Dyn_d_un

deriving via BG.SizedByteArray 4 1 instance Marshal.ReadRaw Elf32_External_Dyn_d_un

deriving via BG.SizedByteArray 4 1 instance Marshal.WriteRaw Elf32_External_Dyn_d_un

deriving via Marshal.EquivStorable Elf32_External_Dyn_d_un instance BG.Storable Elf32_External_Dyn_d_un

deriving via BG.SizedByteArray 4 1 instance Union.IsUnion Elf32_External_Dyn_d_un

{-| __C declaration:__ @d_val@

    __defined at:__ @comprehensive\/c2hsc.h 350:19@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 4 BG.CUChar
         ) => BG.HasField "d_val" Elf32_External_Dyn_d_un ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @d_val@

    __defined at:__ @comprehensive\/c2hsc.h 350:19@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 4 BG.CUChar
         ) => BG.CompatHasField.HasField "d_val" Elf32_External_Dyn_d_un ty where

  hasField =
    \x0 -> (BG.setUnionPayload, BG.getField @"d_val" x0)

instance ( ty ~ CA.ConstantArray 4 BG.CUChar
         ) => BG.HasField "d_val" (BG.Ptr Elf32_External_Dyn_d_un) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"d_val")

instance HasCField.HasCField Elf32_External_Dyn_d_un "d_val" where

  type CFieldType Elf32_External_Dyn_d_un "d_val" =
    CA.ConstantArray 4 BG.CUChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @d_ptr@

    __defined at:__ @comprehensive\/c2hsc.h 351:19@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 4 BG.CUChar
         ) => BG.HasField "d_ptr" Elf32_External_Dyn_d_un ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @d_ptr@

    __defined at:__ @comprehensive\/c2hsc.h 351:19@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 4 BG.CUChar
         ) => BG.CompatHasField.HasField "d_ptr" Elf32_External_Dyn_d_un ty where

  hasField =
    \x0 -> (BG.setUnionPayload, BG.getField @"d_ptr" x0)

instance ( ty ~ CA.ConstantArray 4 BG.CUChar
         ) => BG.HasField "d_ptr" (BG.Ptr Elf32_External_Dyn_d_un) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"d_ptr")

instance HasCField.HasCField Elf32_External_Dyn_d_un "d_ptr" where

  type CFieldType Elf32_External_Dyn_d_un "d_ptr" =
    CA.ConstantArray 4 BG.CUChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct Elf32_External_Dyn@

    __defined at:__ @comprehensive\/c2hsc.h 347:9@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Elf32_External_Dyn = Elf32_External_Dyn
  { d_tag :: CA.ConstantArray 4 BG.CUChar
    {- ^ __C declaration:__ @d_tag@

         __defined at:__ @comprehensive\/c2hsc.h 348:17@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  , d_un :: Elf32_External_Dyn_d_un
    {- ^ __C declaration:__ @d_un@

         __defined at:__ @comprehensive\/c2hsc.h 352:5@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (BG.Generic)

instance Marshal.StaticSize Elf32_External_Dyn where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Elf32_External_Dyn where

  readRaw =
    \ptr0 ->
          pure Elf32_External_Dyn
      <*> HasCField.readRaw (BG.Proxy @"d_tag") ptr0
      <*> HasCField.readRaw (BG.Proxy @"d_un") ptr0

instance Marshal.WriteRaw Elf32_External_Dyn where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Elf32_External_Dyn d_tag2 d_un3 ->
               HasCField.writeRaw (BG.Proxy @"d_tag") ptr0 d_tag2
            >> HasCField.writeRaw (BG.Proxy @"d_un") ptr0 d_un3

deriving via Marshal.EquivStorable Elf32_External_Dyn instance BG.Storable Elf32_External_Dyn

deriving via Struct.IsStructViaStorable Elf32_External_Dyn instance Struct.IsStruct Elf32_External_Dyn

{-| __C declaration:__ @d_tag@

    __defined at:__ @comprehensive\/c2hsc.h 348:17@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ CA.ConstantArray 4 BG.CUChar
         ) => BG.CompatHasField.HasField "d_tag" Elf32_External_Dyn ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Elf32_External_Dyn {d_tag = y1, d_un = BG.getField @"d_un" x0}
      , BG.getField @"d_tag" x0
      )

instance ( ty ~ CA.ConstantArray 4 BG.CUChar
         ) => BG.HasField "d_tag" (BG.Ptr Elf32_External_Dyn) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"d_tag")

instance HasCField.HasCField Elf32_External_Dyn "d_tag" where

  type CFieldType Elf32_External_Dyn "d_tag" =
    CA.ConstantArray 4 BG.CUChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @d_un@

    __defined at:__ @comprehensive\/c2hsc.h 352:5@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ Elf32_External_Dyn_d_un
         ) => BG.CompatHasField.HasField "d_un" Elf32_External_Dyn ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Elf32_External_Dyn {d_un = y1, d_tag = BG.getField @"d_tag" x0}
      , BG.getField @"d_un" x0
      )

instance ( ty ~ Elf32_External_Dyn_d_un
         ) => BG.HasField "d_un" (BG.Ptr Elf32_External_Dyn) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"d_un")

instance HasCField.HasCField Elf32_External_Dyn "d_un" where

  type CFieldType Elf32_External_Dyn "d_un" =
    Elf32_External_Dyn_d_un

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @bug_24@

    __defined at:__ @comprehensive\/c2hsc.h 358:15@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype Bug_24 = Bug_24
  { unwrap :: BG.Ptr BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.CompatHasField.HasField "unwrap" Bug_24 ty where

  hasField =
    \x0 ->
      (\y1 ->
         Bug_24 {unwrap = y1}, BG.getField @"unwrap" x0)

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.HasField "unwrap" (BG.Ptr Bug_24) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrap")

instance HasCField.HasCField Bug_24 "unwrap" where

  type CFieldType Bug_24 "unwrap" = BG.Ptr BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @bug_24_2@

    __defined at:__ @comprehensive\/c2hsc.h 359:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype Bug_24_2 = Bug_24_2
  { unwrap :: PtrConst.PtrConst BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.CompatHasField.HasField "unwrap" Bug_24_2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         Bug_24_2 {unwrap = y1}, BG.getField @"unwrap" x0)

instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.HasField "unwrap" (BG.Ptr Bug_24_2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrap")

instance HasCField.HasCField Bug_24_2 "unwrap" where

  type CFieldType Bug_24_2 "unwrap" =
    PtrConst.PtrConst BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MyArray_27@

    __defined at:__ @comprehensive\/c2hsc.h 364:13@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype MyArray_27 = MyArray_27
  { unwrap :: CA.ConstantArray 20 BG.CInt
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 20 BG.CInt
         ) => BG.CompatHasField.HasField "unwrap" MyArray_27 ty where

  hasField =
    \x0 ->
      (\y1 ->
         MyArray_27 {unwrap = y1}, BG.getField @"unwrap" x0)

instance ( ty ~ CA.ConstantArray 20 BG.CInt
         ) => BG.HasField "unwrap" (BG.Ptr MyArray_27) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrap")

instance HasCField.HasCField MyArray_27 "unwrap" where

  type CFieldType MyArray_27 "unwrap" =
    CA.ConstantArray 20 BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct MyStruct_27@

    __defined at:__ @comprehensive\/c2hsc.h 365:9@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data MyStruct_27 = MyStruct_27
  { x :: MyArray_27
    {- ^ __C declaration:__ @x@

         __defined at:__ @comprehensive\/c2hsc.h 366:14@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize MyStruct_27 where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw MyStruct_27 where

  readRaw =
    \ptr0 ->
          pure MyStruct_27
      <*> HasCField.readRaw (BG.Proxy @"x") ptr0

instance Marshal.WriteRaw MyStruct_27 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyStruct_27 x2 ->
            HasCField.writeRaw (BG.Proxy @"x") ptr0 x2

deriving via Marshal.EquivStorable MyStruct_27 instance BG.Storable MyStruct_27

deriving via Struct.IsStructViaStorable MyStruct_27 instance Struct.IsStruct MyStruct_27

{-| __C declaration:__ @x@

    __defined at:__ @comprehensive\/c2hsc.h 366:14@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
instance ( ty ~ MyArray_27
         ) => BG.CompatHasField.HasField "x" MyStruct_27 ty where

  hasField =
    \x0 ->
      (\y1 -> MyStruct_27 {x = y1}, BG.getField @"x" x0)

instance ( ty ~ MyArray_27
         ) => BG.HasField "x" (BG.Ptr MyStruct_27) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"x")

instance HasCField.HasCField MyStruct_27 "x" where

  type CFieldType MyStruct_27 "x" = MyArray_27

  offset# = \_ -> \_ -> 0
