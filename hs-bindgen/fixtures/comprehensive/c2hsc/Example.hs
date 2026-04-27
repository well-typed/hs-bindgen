{-# LANGUAGE CApiFFI #-}
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
    , Example.get_u_c
    , Example.set_u_c
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
    , Example.get_elf32_External_Dyn_d_un_d_val
    , Example.set_elf32_External_Dyn_d_un_d_val
    , Example.get_elf32_External_Dyn_d_un_d_ptr
    , Example.set_elf32_External_Dyn_d_un_d_ptr
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
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.PtrConst as PtrConst

{-| Issues

    __C declaration:__ @an_pchar@

    __defined at:__ @comprehensive\/c2hsc.h 12:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype An_pchar = An_pchar
  { unwrap :: PtrConst.PtrConst RIP.CChar
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ PtrConst.PtrConst RIP.CChar
         ) => RIP.HasField "unwrap" (RIP.Ptr An_pchar) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField An_pchar "unwrap" where

  type CFieldType An_pchar "unwrap" =
    PtrConst.PtrConst RIP.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct MyCoolStruct@

    __defined at:__ @comprehensive\/c2hsc.h 15:9@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data MyCoolStruct = MyCoolStruct
  { listOfNames :: CA.ConstantArray 8 (CA.ConstantArray 255 RIP.CChar)
    {- ^ __C declaration:__ @listOfNames@

         __defined at:__ @comprehensive\/c2hsc.h 16:10@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize MyCoolStruct where

  staticSizeOf = \_ -> (2040 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw MyCoolStruct where

  readRaw =
    \ptr0 ->
          pure MyCoolStruct
      <*> HasCField.readRaw (RIP.Proxy @"listOfNames") ptr0

instance Marshal.WriteRaw MyCoolStruct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyCoolStruct listOfNames2 ->
            HasCField.writeRaw (RIP.Proxy @"listOfNames") ptr0 listOfNames2

deriving via Marshal.EquivStorable MyCoolStruct instance RIP.Storable MyCoolStruct

instance HasCField.HasCField MyCoolStruct "listOfNames" where

  type CFieldType MyCoolStruct "listOfNames" =
    CA.ConstantArray 8 (CA.ConstantArray 255 RIP.CChar)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 8 (CA.ConstantArray 255 RIP.CChar)
         ) => RIP.HasField "listOfNames" (RIP.Ptr MyCoolStruct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"listOfNames")

{-| Auxiliary type used by 'Foo'

    __C declaration:__ @foo@

    __defined at:__ @comprehensive\/c2hsc.h 20:15@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype Foo_Aux = Foo_Aux
  { unwrap :: RIP.CInt -> IO RIP.CInt
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

-- __unique:__ @toFoo_Aux@
foreign import ccall safe "wrapper" hs_bindgen_b5a7b5e83ffee6b4_base ::
     (RIP.Int32 -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int32 -> IO RIP.Int32))

-- __unique:__ @toFoo_Aux@
hs_bindgen_b5a7b5e83ffee6b4 ::
     Foo_Aux
  -> IO (RIP.FunPtr Foo_Aux)
hs_bindgen_b5a7b5e83ffee6b4 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_b5a7b5e83ffee6b4_base (RIP.toFFIType fun0))

-- __unique:__ @fromFoo_Aux@
foreign import ccall safe "dynamic" hs_bindgen_223d08172bb37c01_base ::
     RIP.FunPtr (RIP.Int32 -> IO RIP.Int32)
  -> RIP.Int32 -> IO RIP.Int32

-- __unique:__ @fromFoo_Aux@
hs_bindgen_223d08172bb37c01 ::
     RIP.FunPtr Foo_Aux
  -> Foo_Aux
hs_bindgen_223d08172bb37c01 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_223d08172bb37c01_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Foo_Aux where

  toFunPtr = hs_bindgen_b5a7b5e83ffee6b4

instance RIP.FromFunPtr Foo_Aux where

  fromFunPtr = hs_bindgen_223d08172bb37c01

instance ( ty ~ (RIP.CInt -> IO RIP.CInt)
         ) => RIP.HasField "unwrap" (RIP.Ptr Foo_Aux) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField Foo_Aux "unwrap" where

  type CFieldType Foo_Aux "unwrap" =
    RIP.CInt -> IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @foo@

    __defined at:__ @comprehensive\/c2hsc.h 20:15@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype Foo = Foo
  { unwrap :: RIP.FunPtr Foo_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ RIP.FunPtr Foo_Aux
         ) => RIP.HasField "unwrap" (RIP.Ptr Foo) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField Foo "unwrap" where

  type CFieldType Foo "unwrap" = RIP.FunPtr Foo_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct foo_t@

    __defined at:__ @comprehensive\/c2hsc.h 31:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Foo_t = Foo_t
  { foo_member :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
    {- ^ __C declaration:__ @foo_member@

         __defined at:__ @comprehensive\/c2hsc.h 32:11@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Foo_t where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Foo_t where

  readRaw =
    \ptr0 ->
          pure Foo_t
      <*> HasCField.readRaw (RIP.Proxy @"foo_member") ptr0

instance Marshal.WriteRaw Foo_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo_t foo_member2 ->
            HasCField.writeRaw (RIP.Proxy @"foo_member") ptr0 foo_member2

deriving via Marshal.EquivStorable Foo_t instance RIP.Storable Foo_t

instance HasCField.HasCField Foo_t "foo_member" where

  type CFieldType Foo_t "foo_member" =
    RIP.FunPtr (RIP.CInt -> IO RIP.CInt)

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
         ) => RIP.HasField "foo_member" (RIP.Ptr Foo_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"foo_member")

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
  { unwrap :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

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

deriving via Marshal.EquivStorable Bar_10 instance RIP.Storable Bar_10

deriving via RIP.CUInt instance RIP.Prim Bar_10

instance CEnum.CEnum Bar_10 where

  type CEnumZ Bar_10 = RIP.CUInt

  toCEnum = Bar_10

  fromCEnum = RIP.getField @"unwrap"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "BAR")]

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

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrap" (RIP.Ptr Bar_10) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField Bar_10 "unwrap" where

  type CFieldType Bar_10 "unwrap" = RIP.CUInt

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
  { i :: RIP.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @comprehensive\/c2hsc.h 49:7@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize St where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw St where

  readRaw =
    \ptr0 ->
          pure St
      <*> HasCField.readRaw (RIP.Proxy @"i") ptr0

instance Marshal.WriteRaw St where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          St i2 -> HasCField.writeRaw (RIP.Proxy @"i") ptr0 i2

deriving via Marshal.EquivStorable St instance RIP.Storable St

instance HasCField.HasCField St "i" where

  type CFieldType St "i" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance (ty ~ RIP.CInt) => RIP.HasField "i" (RIP.Ptr St) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"i")

{-| __C declaration:__ @enum e@

    __defined at:__ @comprehensive\/c2hsc.h 52:6@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype E = E
  { unwrap :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

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

deriving via Marshal.EquivStorable E instance RIP.Storable E

deriving via RIP.CUInt instance RIP.Prim E

instance CEnum.CEnum E where

  type CEnumZ E = RIP.CUInt

  toCEnum = E

  fromCEnum = RIP.getField @"unwrap"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "CONST")]

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

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrap" (RIP.Ptr E) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField E "unwrap" where

  type CFieldType E "unwrap" = RIP.CUInt

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
  { unwrap :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 1 1 instance Marshal.StaticSize U

deriving via RIP.SizedByteArray 1 1 instance Marshal.ReadRaw U

deriving via RIP.SizedByteArray 1 1 instance Marshal.WriteRaw U

deriving via Marshal.EquivStorable U instance RIP.Storable U

{-|

    __See:__ 'set_u_c'

    __C declaration:__ @c@

    __defined at:__ @comprehensive\/c2hsc.h 57:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
get_u_c ::
     U
  -> RIP.CChar
get_u_c = RIP.getUnionPayload

{-|

    __See:__ 'get_u_c'

-}
set_u_c ::
     RIP.CChar
  -> U
set_u_c = RIP.setUnionPayload

instance HasCField.HasCField U "c" where

  type CFieldType U "c" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance (ty ~ RIP.CChar) => RIP.HasField "c" (RIP.Ptr U) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"c")

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
  { unwrap :: RIP.Ptr MyTypeImpl
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ RIP.Ptr MyTypeImpl
         ) => RIP.HasField "unwrap" (RIP.Ptr MyType) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField MyType "unwrap" where

  type CFieldType MyType "unwrap" = RIP.Ptr MyTypeImpl

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct MyStruct@

    __defined at:__ @comprehensive\/c2hsc.h 64:16@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data MyStructType = MyStructType
  { x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @comprehensive\/c2hsc.h 65:7@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize MyStructType where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw MyStructType where

  readRaw =
    \ptr0 ->
          pure MyStructType
      <*> HasCField.readRaw (RIP.Proxy @"x") ptr0

instance Marshal.WriteRaw MyStructType where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyStructType x2 ->
            HasCField.writeRaw (RIP.Proxy @"x") ptr0 x2

deriving via Marshal.EquivStorable MyStructType instance RIP.Storable MyStructType

instance HasCField.HasCField MyStructType "x" where

  type CFieldType MyStructType "x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "x" (RIP.Ptr MyStructType) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"x")

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
  { ordinary_float_member :: RIP.CFloat
    {- ^ __C declaration:__ @ordinary_float_member@

         __defined at:__ @comprehensive\/c2hsc.h 204:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_float_struct where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Ordinary_float_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_float_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_float_member") ptr0

instance Marshal.WriteRaw Ordinary_float_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_float_struct ordinary_float_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_float_member") ptr0 ordinary_float_member2

deriving via Marshal.EquivStorable Ordinary_float_struct instance RIP.Storable Ordinary_float_struct

instance HasCField.HasCField Ordinary_float_struct "ordinary_float_member" where

  type CFieldType Ordinary_float_struct "ordinary_float_member" =
    RIP.CFloat

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CFloat
         ) => RIP.HasField "ordinary_float_member" (RIP.Ptr Ordinary_float_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_float_member")

{-| __C declaration:__ @struct ordinary_double_struct@

    __defined at:__ @comprehensive\/c2hsc.h 205:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_double_struct = Ordinary_double_struct
  { ordinary_double_member :: RIP.CDouble
    {- ^ __C declaration:__ @ordinary_double_member@

         __defined at:__ @comprehensive\/c2hsc.h 205:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_double_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_double_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_double_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_double_member") ptr0

instance Marshal.WriteRaw Ordinary_double_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_double_struct ordinary_double_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_double_member") ptr0 ordinary_double_member2

deriving via Marshal.EquivStorable Ordinary_double_struct instance RIP.Storable Ordinary_double_struct

instance HasCField.HasCField Ordinary_double_struct "ordinary_double_member" where

  type CFieldType Ordinary_double_struct "ordinary_double_member" =
    RIP.CDouble

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CDouble
         ) => RIP.HasField "ordinary_double_member" (RIP.Ptr Ordinary_double_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_double_member")

{-| __C declaration:__ @struct ordinary_signed_char_struct@

    __defined at:__ @comprehensive\/c2hsc.h 208:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_char_struct = Ordinary_signed_char_struct
  { ordinary_signed_char_member :: RIP.CChar
    {- ^ __C declaration:__ @ordinary_signed_char_member@

         __defined at:__ @comprehensive\/c2hsc.h 208:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_char_struct where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Ordinary_signed_char_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_char_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_char_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_char_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_char_struct ordinary_signed_char_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_signed_char_member") ptr0 ordinary_signed_char_member2

deriving via Marshal.EquivStorable Ordinary_signed_char_struct instance RIP.Storable Ordinary_signed_char_struct

instance HasCField.HasCField Ordinary_signed_char_struct "ordinary_signed_char_member" where

  type CFieldType Ordinary_signed_char_struct "ordinary_signed_char_member" =
    RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CChar
         ) => RIP.HasField "ordinary_signed_char_member" (RIP.Ptr Ordinary_signed_char_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_char_member")

{-| __C declaration:__ @struct explicit_signed_char_struct@

    __defined at:__ @comprehensive\/c2hsc.h 209:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_char_struct = Explicit_signed_char_struct
  { explicit_signed_char_member :: RIP.CSChar
    {- ^ __C declaration:__ @explicit_signed_char_member@

         __defined at:__ @comprehensive\/c2hsc.h 209:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_char_struct where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Explicit_signed_char_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_char_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_char_member") ptr0

instance Marshal.WriteRaw Explicit_signed_char_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_char_struct explicit_signed_char_member2 ->
            HasCField.writeRaw (RIP.Proxy @"explicit_signed_char_member") ptr0 explicit_signed_char_member2

deriving via Marshal.EquivStorable Explicit_signed_char_struct instance RIP.Storable Explicit_signed_char_struct

instance HasCField.HasCField Explicit_signed_char_struct "explicit_signed_char_member" where

  type CFieldType Explicit_signed_char_struct "explicit_signed_char_member" =
    RIP.CSChar

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CSChar
         ) => RIP.HasField "explicit_signed_char_member" (RIP.Ptr Explicit_signed_char_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_char_member")

{-| __C declaration:__ @struct unsigned_char_struct@

    __defined at:__ @comprehensive\/c2hsc.h 210:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_char_struct = Unsigned_char_struct
  { unsigned_char_member :: RIP.CUChar
    {- ^ __C declaration:__ @unsigned_char_member@

         __defined at:__ @comprehensive\/c2hsc.h 210:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_char_struct where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Unsigned_char_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_char_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_char_member") ptr0

instance Marshal.WriteRaw Unsigned_char_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_char_struct unsigned_char_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_char_member") ptr0 unsigned_char_member2

deriving via Marshal.EquivStorable Unsigned_char_struct instance RIP.Storable Unsigned_char_struct

instance HasCField.HasCField Unsigned_char_struct "unsigned_char_member" where

  type CFieldType Unsigned_char_struct "unsigned_char_member" =
    RIP.CUChar

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CUChar
         ) => RIP.HasField "unsigned_char_member" (RIP.Ptr Unsigned_char_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_char_member")

{-| __C declaration:__ @struct ordinary_signed_short_struct@

    __defined at:__ @comprehensive\/c2hsc.h 212:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_short_struct = Ordinary_signed_short_struct
  { ordinary_signed_short_member :: RIP.CShort
    {- ^ __C declaration:__ @ordinary_signed_short_member@

         __defined at:__ @comprehensive\/c2hsc.h 212:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_short_struct where

  staticSizeOf = \_ -> (2 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Ordinary_signed_short_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_short_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_short_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_short_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_short_struct ordinary_signed_short_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_signed_short_member") ptr0 ordinary_signed_short_member2

deriving via Marshal.EquivStorable Ordinary_signed_short_struct instance RIP.Storable Ordinary_signed_short_struct

instance HasCField.HasCField Ordinary_signed_short_struct "ordinary_signed_short_member" where

  type CFieldType Ordinary_signed_short_struct "ordinary_signed_short_member" =
    RIP.CShort

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CShort
         ) => RIP.HasField "ordinary_signed_short_member" (RIP.Ptr Ordinary_signed_short_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_short_member")

{-| __C declaration:__ @struct explicit_signed_short_struct@

    __defined at:__ @comprehensive\/c2hsc.h 213:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_short_struct = Explicit_signed_short_struct
  { explicit_signed_short_member :: RIP.CShort
    {- ^ __C declaration:__ @explicit_signed_short_member@

         __defined at:__ @comprehensive\/c2hsc.h 213:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_short_struct where

  staticSizeOf = \_ -> (2 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Explicit_signed_short_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_short_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_short_member") ptr0

instance Marshal.WriteRaw Explicit_signed_short_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_short_struct explicit_signed_short_member2 ->
            HasCField.writeRaw (RIP.Proxy @"explicit_signed_short_member") ptr0 explicit_signed_short_member2

deriving via Marshal.EquivStorable Explicit_signed_short_struct instance RIP.Storable Explicit_signed_short_struct

instance HasCField.HasCField Explicit_signed_short_struct "explicit_signed_short_member" where

  type CFieldType Explicit_signed_short_struct "explicit_signed_short_member" =
    RIP.CShort

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CShort
         ) => RIP.HasField "explicit_signed_short_member" (RIP.Ptr Explicit_signed_short_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_short_member")

{-| __C declaration:__ @struct unsigned_short_struct@

    __defined at:__ @comprehensive\/c2hsc.h 214:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_short_struct = Unsigned_short_struct
  { unsigned_short_member :: RIP.CUShort
    {- ^ __C declaration:__ @unsigned_short_member@

         __defined at:__ @comprehensive\/c2hsc.h 214:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_short_struct where

  staticSizeOf = \_ -> (2 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Unsigned_short_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_short_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_short_member") ptr0

instance Marshal.WriteRaw Unsigned_short_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_short_struct unsigned_short_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_short_member") ptr0 unsigned_short_member2

deriving via Marshal.EquivStorable Unsigned_short_struct instance RIP.Storable Unsigned_short_struct

instance HasCField.HasCField Unsigned_short_struct "unsigned_short_member" where

  type CFieldType Unsigned_short_struct "unsigned_short_member" =
    RIP.CUShort

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CUShort
         ) => RIP.HasField "unsigned_short_member" (RIP.Ptr Unsigned_short_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_short_member")

{-| __C declaration:__ @struct ordinary_signed_int_struct@

    __defined at:__ @comprehensive\/c2hsc.h 216:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_int_struct = Ordinary_signed_int_struct
  { ordinary_signed_int_member :: RIP.CInt
    {- ^ __C declaration:__ @ordinary_signed_int_member@

         __defined at:__ @comprehensive\/c2hsc.h 216:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_int_struct where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Ordinary_signed_int_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_int_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_int_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_int_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_int_struct ordinary_signed_int_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_signed_int_member") ptr0 ordinary_signed_int_member2

deriving via Marshal.EquivStorable Ordinary_signed_int_struct instance RIP.Storable Ordinary_signed_int_struct

instance HasCField.HasCField Ordinary_signed_int_struct "ordinary_signed_int_member" where

  type CFieldType Ordinary_signed_int_struct "ordinary_signed_int_member" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "ordinary_signed_int_member" (RIP.Ptr Ordinary_signed_int_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_int_member")

{-| __C declaration:__ @struct explicit_signed_int_struct@

    __defined at:__ @comprehensive\/c2hsc.h 217:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_int_struct = Explicit_signed_int_struct
  { explicit_signed_int_member :: RIP.CInt
    {- ^ __C declaration:__ @explicit_signed_int_member@

         __defined at:__ @comprehensive\/c2hsc.h 217:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_int_struct where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Explicit_signed_int_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_int_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_int_member") ptr0

instance Marshal.WriteRaw Explicit_signed_int_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_int_struct explicit_signed_int_member2 ->
            HasCField.writeRaw (RIP.Proxy @"explicit_signed_int_member") ptr0 explicit_signed_int_member2

deriving via Marshal.EquivStorable Explicit_signed_int_struct instance RIP.Storable Explicit_signed_int_struct

instance HasCField.HasCField Explicit_signed_int_struct "explicit_signed_int_member" where

  type CFieldType Explicit_signed_int_struct "explicit_signed_int_member" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "explicit_signed_int_member" (RIP.Ptr Explicit_signed_int_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_int_member")

{-| __C declaration:__ @struct unsigned_int_struct@

    __defined at:__ @comprehensive\/c2hsc.h 218:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_int_struct = Unsigned_int_struct
  { unsigned_int_member :: RIP.CUInt
    {- ^ __C declaration:__ @unsigned_int_member@

         __defined at:__ @comprehensive\/c2hsc.h 218:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_int_struct where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Unsigned_int_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_int_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_int_member") ptr0

instance Marshal.WriteRaw Unsigned_int_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_int_struct unsigned_int_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_int_member") ptr0 unsigned_int_member2

deriving via Marshal.EquivStorable Unsigned_int_struct instance RIP.Storable Unsigned_int_struct

instance HasCField.HasCField Unsigned_int_struct "unsigned_int_member" where

  type CFieldType Unsigned_int_struct "unsigned_int_member" =
    RIP.CUInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unsigned_int_member" (RIP.Ptr Unsigned_int_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_int_member")

{-| __C declaration:__ @struct ordinary_signed_long_struct@

    __defined at:__ @comprehensive\/c2hsc.h 220:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_long_struct = Ordinary_signed_long_struct
  { ordinary_signed_long_member :: RIP.CLong
    {- ^ __C declaration:__ @ordinary_signed_long_member@

         __defined at:__ @comprehensive\/c2hsc.h 220:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_long_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_long_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_long_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_long_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_long_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_long_struct ordinary_signed_long_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_signed_long_member") ptr0 ordinary_signed_long_member2

deriving via Marshal.EquivStorable Ordinary_signed_long_struct instance RIP.Storable Ordinary_signed_long_struct

instance HasCField.HasCField Ordinary_signed_long_struct "ordinary_signed_long_member" where

  type CFieldType Ordinary_signed_long_struct "ordinary_signed_long_member" =
    RIP.CLong

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CLong
         ) => RIP.HasField "ordinary_signed_long_member" (RIP.Ptr Ordinary_signed_long_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_long_member")

{-| __C declaration:__ @struct explicit_signed_long_struct@

    __defined at:__ @comprehensive\/c2hsc.h 221:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_long_struct = Explicit_signed_long_struct
  { explicit_signed_long_member :: RIP.CLong
    {- ^ __C declaration:__ @explicit_signed_long_member@

         __defined at:__ @comprehensive\/c2hsc.h 221:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_long_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_long_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_long_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_long_member") ptr0

instance Marshal.WriteRaw Explicit_signed_long_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_long_struct explicit_signed_long_member2 ->
            HasCField.writeRaw (RIP.Proxy @"explicit_signed_long_member") ptr0 explicit_signed_long_member2

deriving via Marshal.EquivStorable Explicit_signed_long_struct instance RIP.Storable Explicit_signed_long_struct

instance HasCField.HasCField Explicit_signed_long_struct "explicit_signed_long_member" where

  type CFieldType Explicit_signed_long_struct "explicit_signed_long_member" =
    RIP.CLong

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CLong
         ) => RIP.HasField "explicit_signed_long_member" (RIP.Ptr Explicit_signed_long_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_long_member")

{-| __C declaration:__ @struct unsigned_long_struct@

    __defined at:__ @comprehensive\/c2hsc.h 222:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_long_struct = Unsigned_long_struct
  { unsigned_long_member :: RIP.CULong
    {- ^ __C declaration:__ @unsigned_long_member@

         __defined at:__ @comprehensive\/c2hsc.h 222:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_long_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_long_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_long_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_long_member") ptr0

instance Marshal.WriteRaw Unsigned_long_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_long_struct unsigned_long_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_long_member") ptr0 unsigned_long_member2

deriving via Marshal.EquivStorable Unsigned_long_struct instance RIP.Storable Unsigned_long_struct

instance HasCField.HasCField Unsigned_long_struct "unsigned_long_member" where

  type CFieldType Unsigned_long_struct "unsigned_long_member" =
    RIP.CULong

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CULong
         ) => RIP.HasField "unsigned_long_member" (RIP.Ptr Unsigned_long_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_long_member")

{-| __C declaration:__ @struct ordinary_signed_long_long_struct@

    __defined at:__ @comprehensive\/c2hsc.h 224:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_long_long_struct = Ordinary_signed_long_long_struct
  { ordinary_signed_long_long_member :: RIP.CLLong
    {- ^ __C declaration:__ @ordinary_signed_long_long_member@

         __defined at:__ @comprehensive\/c2hsc.h 224:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_long_long_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_long_long_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_long_long_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_long_long_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_long_long_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_long_long_struct ordinary_signed_long_long_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_signed_long_long_member") ptr0 ordinary_signed_long_long_member2

deriving via Marshal.EquivStorable Ordinary_signed_long_long_struct instance RIP.Storable Ordinary_signed_long_long_struct

instance HasCField.HasCField Ordinary_signed_long_long_struct "ordinary_signed_long_long_member" where

  type CFieldType Ordinary_signed_long_long_struct "ordinary_signed_long_long_member" =
    RIP.CLLong

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CLLong
         ) => RIP.HasField "ordinary_signed_long_long_member" (RIP.Ptr Ordinary_signed_long_long_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_long_long_member")

{-| __C declaration:__ @struct explicit_signed_long_long_struct@

    __defined at:__ @comprehensive\/c2hsc.h 225:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_long_long_struct = Explicit_signed_long_long_struct
  { explicit_signed_long_long_member :: RIP.CLLong
    {- ^ __C declaration:__ @explicit_signed_long_long_member@

         __defined at:__ @comprehensive\/c2hsc.h 225:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_long_long_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_long_long_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_long_long_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_long_long_member") ptr0

instance Marshal.WriteRaw Explicit_signed_long_long_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_long_long_struct explicit_signed_long_long_member2 ->
            HasCField.writeRaw (RIP.Proxy @"explicit_signed_long_long_member") ptr0 explicit_signed_long_long_member2

deriving via Marshal.EquivStorable Explicit_signed_long_long_struct instance RIP.Storable Explicit_signed_long_long_struct

instance HasCField.HasCField Explicit_signed_long_long_struct "explicit_signed_long_long_member" where

  type CFieldType Explicit_signed_long_long_struct "explicit_signed_long_long_member" =
    RIP.CLLong

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CLLong
         ) => RIP.HasField "explicit_signed_long_long_member" (RIP.Ptr Explicit_signed_long_long_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_long_long_member")

{-| __C declaration:__ @struct unsigned_long_long_struct@

    __defined at:__ @comprehensive\/c2hsc.h 226:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_long_long_struct = Unsigned_long_long_struct
  { unsigned_long_long_member :: RIP.CULLong
    {- ^ __C declaration:__ @unsigned_long_long_member@

         __defined at:__ @comprehensive\/c2hsc.h 226:62@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_long_long_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_long_long_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_long_long_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_long_long_member") ptr0

instance Marshal.WriteRaw Unsigned_long_long_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_long_long_struct unsigned_long_long_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_long_long_member") ptr0 unsigned_long_long_member2

deriving via Marshal.EquivStorable Unsigned_long_long_struct instance RIP.Storable Unsigned_long_long_struct

instance HasCField.HasCField Unsigned_long_long_struct "unsigned_long_long_member" where

  type CFieldType Unsigned_long_long_struct "unsigned_long_long_member" =
    RIP.CULLong

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CULLong
         ) => RIP.HasField "unsigned_long_long_member" (RIP.Ptr Unsigned_long_long_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_long_long_member")

{-| Structs: pointers

    NOTE: @'Ordinary_signed_char_pointer_struct'@ is commented out in the original test suite, unclear why (no reason is given).

    __C declaration:__ @struct ordinary_void_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 235:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_void_pointer_struct = Ordinary_void_pointer_struct
  { ordinary_void_pointer_member :: RIP.Ptr RIP.Void
    {- ^ __C declaration:__ @ordinary_void_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 235:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_void_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_void_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_void_pointer_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_void_pointer_member") ptr0

instance Marshal.WriteRaw Ordinary_void_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_void_pointer_struct ordinary_void_pointer_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_void_pointer_member") ptr0 ordinary_void_pointer_member2

deriving via Marshal.EquivStorable Ordinary_void_pointer_struct instance RIP.Storable Ordinary_void_pointer_struct

instance HasCField.HasCField Ordinary_void_pointer_struct "ordinary_void_pointer_member" where

  type CFieldType Ordinary_void_pointer_struct "ordinary_void_pointer_member" =
    RIP.Ptr RIP.Void

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.Void
         ) => RIP.HasField "ordinary_void_pointer_member" (RIP.Ptr Ordinary_void_pointer_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_void_pointer_member")

{-| __C declaration:__ @struct ordinary_float_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 237:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_float_pointer_struct = Ordinary_float_pointer_struct
  { ordinary_float_pointer_member :: RIP.Ptr RIP.CFloat
    {- ^ __C declaration:__ @ordinary_float_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 237:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_float_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_float_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_float_pointer_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_float_pointer_member") ptr0

instance Marshal.WriteRaw Ordinary_float_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_float_pointer_struct ordinary_float_pointer_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_float_pointer_member") ptr0 ordinary_float_pointer_member2

deriving via Marshal.EquivStorable Ordinary_float_pointer_struct instance RIP.Storable Ordinary_float_pointer_struct

instance HasCField.HasCField Ordinary_float_pointer_struct "ordinary_float_pointer_member" where

  type CFieldType Ordinary_float_pointer_struct "ordinary_float_pointer_member" =
    RIP.Ptr RIP.CFloat

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.CFloat
         ) => RIP.HasField "ordinary_float_pointer_member" (RIP.Ptr Ordinary_float_pointer_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_float_pointer_member")

{-| __C declaration:__ @struct ordinary_double_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 238:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_double_pointer_struct = Ordinary_double_pointer_struct
  { ordinary_double_pointer_member :: RIP.Ptr RIP.CDouble
    {- ^ __C declaration:__ @ordinary_double_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 238:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_double_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_double_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_double_pointer_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_double_pointer_member") ptr0

instance Marshal.WriteRaw Ordinary_double_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_double_pointer_struct ordinary_double_pointer_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_double_pointer_member") ptr0 ordinary_double_pointer_member2

deriving via Marshal.EquivStorable Ordinary_double_pointer_struct instance RIP.Storable Ordinary_double_pointer_struct

instance HasCField.HasCField Ordinary_double_pointer_struct "ordinary_double_pointer_member" where

  type CFieldType Ordinary_double_pointer_struct "ordinary_double_pointer_member" =
    RIP.Ptr RIP.CDouble

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.CDouble
         ) => RIP.HasField "ordinary_double_pointer_member" (RIP.Ptr Ordinary_double_pointer_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_double_pointer_member")

{-| __C declaration:__ @struct ordinary_signed_char_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 241:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_char_pointer_struct = Ordinary_signed_char_pointer_struct
  { ordinary_signed_char_pointer_member :: RIP.Ptr RIP.CChar
    {- ^ __C declaration:__ @ordinary_signed_char_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 241:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_char_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_char_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_char_pointer_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_char_pointer_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_char_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_char_pointer_struct ordinary_signed_char_pointer_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_signed_char_pointer_member") ptr0 ordinary_signed_char_pointer_member2

deriving via Marshal.EquivStorable Ordinary_signed_char_pointer_struct instance RIP.Storable Ordinary_signed_char_pointer_struct

instance HasCField.HasCField Ordinary_signed_char_pointer_struct "ordinary_signed_char_pointer_member" where

  type CFieldType Ordinary_signed_char_pointer_struct "ordinary_signed_char_pointer_member" =
    RIP.Ptr RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.CChar
         ) => RIP.HasField "ordinary_signed_char_pointer_member" (RIP.Ptr Ordinary_signed_char_pointer_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_char_pointer_member")

{-| __C declaration:__ @struct explicit_signed_char_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 242:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_char_pointer_struct = Explicit_signed_char_pointer_struct
  { explicit_signed_char_pointer_member :: RIP.Ptr RIP.CSChar
    {- ^ __C declaration:__ @explicit_signed_char_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 242:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_char_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_char_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_char_pointer_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_char_pointer_member") ptr0

instance Marshal.WriteRaw Explicit_signed_char_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_char_pointer_struct explicit_signed_char_pointer_member2 ->
            HasCField.writeRaw (RIP.Proxy @"explicit_signed_char_pointer_member") ptr0 explicit_signed_char_pointer_member2

deriving via Marshal.EquivStorable Explicit_signed_char_pointer_struct instance RIP.Storable Explicit_signed_char_pointer_struct

instance HasCField.HasCField Explicit_signed_char_pointer_struct "explicit_signed_char_pointer_member" where

  type CFieldType Explicit_signed_char_pointer_struct "explicit_signed_char_pointer_member" =
    RIP.Ptr RIP.CSChar

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.CSChar
         ) => RIP.HasField "explicit_signed_char_pointer_member" (RIP.Ptr Explicit_signed_char_pointer_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_char_pointer_member")

{-| __C declaration:__ @struct unsigned_char_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 243:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_char_pointer_struct = Unsigned_char_pointer_struct
  { unsigned_char_pointer_member :: RIP.Ptr RIP.CUChar
    {- ^ __C declaration:__ @unsigned_char_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 243:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_char_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_char_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_char_pointer_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_char_pointer_member") ptr0

instance Marshal.WriteRaw Unsigned_char_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_char_pointer_struct unsigned_char_pointer_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_char_pointer_member") ptr0 unsigned_char_pointer_member2

deriving via Marshal.EquivStorable Unsigned_char_pointer_struct instance RIP.Storable Unsigned_char_pointer_struct

instance HasCField.HasCField Unsigned_char_pointer_struct "unsigned_char_pointer_member" where

  type CFieldType Unsigned_char_pointer_struct "unsigned_char_pointer_member" =
    RIP.Ptr RIP.CUChar

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.CUChar
         ) => RIP.HasField "unsigned_char_pointer_member" (RIP.Ptr Unsigned_char_pointer_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_char_pointer_member")

{-| __C declaration:__ @struct ordinary_signed_short_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 245:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_short_pointer_struct = Ordinary_signed_short_pointer_struct
  { ordinary_signed_short_pointer_member :: RIP.Ptr RIP.CShort
    {- ^ __C declaration:__ @ordinary_signed_short_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 245:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_short_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_short_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_short_pointer_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_short_pointer_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_short_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_short_pointer_struct ordinary_signed_short_pointer_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_signed_short_pointer_member") ptr0 ordinary_signed_short_pointer_member2

deriving via Marshal.EquivStorable Ordinary_signed_short_pointer_struct instance RIP.Storable Ordinary_signed_short_pointer_struct

instance HasCField.HasCField Ordinary_signed_short_pointer_struct "ordinary_signed_short_pointer_member" where

  type CFieldType Ordinary_signed_short_pointer_struct "ordinary_signed_short_pointer_member" =
    RIP.Ptr RIP.CShort

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.CShort
         ) => RIP.HasField "ordinary_signed_short_pointer_member" (RIP.Ptr Ordinary_signed_short_pointer_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_short_pointer_member")

{-| __C declaration:__ @struct explicit_signed_short_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 246:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_short_pointer_struct = Explicit_signed_short_pointer_struct
  { explicit_signed_short_pointer_member :: RIP.Ptr RIP.CShort
    {- ^ __C declaration:__ @explicit_signed_short_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 246:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_short_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_short_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_short_pointer_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_short_pointer_member") ptr0

instance Marshal.WriteRaw Explicit_signed_short_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_short_pointer_struct explicit_signed_short_pointer_member2 ->
            HasCField.writeRaw (RIP.Proxy @"explicit_signed_short_pointer_member") ptr0 explicit_signed_short_pointer_member2

deriving via Marshal.EquivStorable Explicit_signed_short_pointer_struct instance RIP.Storable Explicit_signed_short_pointer_struct

instance HasCField.HasCField Explicit_signed_short_pointer_struct "explicit_signed_short_pointer_member" where

  type CFieldType Explicit_signed_short_pointer_struct "explicit_signed_short_pointer_member" =
    RIP.Ptr RIP.CShort

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.CShort
         ) => RIP.HasField "explicit_signed_short_pointer_member" (RIP.Ptr Explicit_signed_short_pointer_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_short_pointer_member")

{-| __C declaration:__ @struct unsigned_short_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 247:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_short_pointer_struct = Unsigned_short_pointer_struct
  { unsigned_short_pointer_member :: RIP.Ptr RIP.CUShort
    {- ^ __C declaration:__ @unsigned_short_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 247:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_short_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_short_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_short_pointer_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_short_pointer_member") ptr0

instance Marshal.WriteRaw Unsigned_short_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_short_pointer_struct unsigned_short_pointer_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_short_pointer_member") ptr0 unsigned_short_pointer_member2

deriving via Marshal.EquivStorable Unsigned_short_pointer_struct instance RIP.Storable Unsigned_short_pointer_struct

instance HasCField.HasCField Unsigned_short_pointer_struct "unsigned_short_pointer_member" where

  type CFieldType Unsigned_short_pointer_struct "unsigned_short_pointer_member" =
    RIP.Ptr RIP.CUShort

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.CUShort
         ) => RIP.HasField "unsigned_short_pointer_member" (RIP.Ptr Unsigned_short_pointer_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_short_pointer_member")

{-| __C declaration:__ @struct ordinary_signed_int_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 249:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_int_pointer_struct = Ordinary_signed_int_pointer_struct
  { ordinary_signed_int_pointer_member :: RIP.Ptr RIP.CInt
    {- ^ __C declaration:__ @ordinary_signed_int_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 249:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_int_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_int_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_int_pointer_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_int_pointer_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_int_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_int_pointer_struct ordinary_signed_int_pointer_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_signed_int_pointer_member") ptr0 ordinary_signed_int_pointer_member2

deriving via Marshal.EquivStorable Ordinary_signed_int_pointer_struct instance RIP.Storable Ordinary_signed_int_pointer_struct

instance HasCField.HasCField Ordinary_signed_int_pointer_struct "ordinary_signed_int_pointer_member" where

  type CFieldType Ordinary_signed_int_pointer_struct "ordinary_signed_int_pointer_member" =
    RIP.Ptr RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.CInt
         ) => RIP.HasField "ordinary_signed_int_pointer_member" (RIP.Ptr Ordinary_signed_int_pointer_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_int_pointer_member")

{-| __C declaration:__ @struct explicit_signed_int_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 250:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_int_pointer_struct = Explicit_signed_int_pointer_struct
  { explicit_signed_int_pointer_member :: RIP.Ptr RIP.CInt
    {- ^ __C declaration:__ @explicit_signed_int_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 250:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_int_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_int_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_int_pointer_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_int_pointer_member") ptr0

instance Marshal.WriteRaw Explicit_signed_int_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_int_pointer_struct explicit_signed_int_pointer_member2 ->
            HasCField.writeRaw (RIP.Proxy @"explicit_signed_int_pointer_member") ptr0 explicit_signed_int_pointer_member2

deriving via Marshal.EquivStorable Explicit_signed_int_pointer_struct instance RIP.Storable Explicit_signed_int_pointer_struct

instance HasCField.HasCField Explicit_signed_int_pointer_struct "explicit_signed_int_pointer_member" where

  type CFieldType Explicit_signed_int_pointer_struct "explicit_signed_int_pointer_member" =
    RIP.Ptr RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.CInt
         ) => RIP.HasField "explicit_signed_int_pointer_member" (RIP.Ptr Explicit_signed_int_pointer_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_int_pointer_member")

{-| __C declaration:__ @struct unsigned_int_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 251:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_int_pointer_struct = Unsigned_int_pointer_struct
  { unsigned_int_pointer_member :: RIP.Ptr RIP.CUInt
    {- ^ __C declaration:__ @unsigned_int_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 251:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_int_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_int_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_int_pointer_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_int_pointer_member") ptr0

instance Marshal.WriteRaw Unsigned_int_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_int_pointer_struct unsigned_int_pointer_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_int_pointer_member") ptr0 unsigned_int_pointer_member2

deriving via Marshal.EquivStorable Unsigned_int_pointer_struct instance RIP.Storable Unsigned_int_pointer_struct

instance HasCField.HasCField Unsigned_int_pointer_struct "unsigned_int_pointer_member" where

  type CFieldType Unsigned_int_pointer_struct "unsigned_int_pointer_member" =
    RIP.Ptr RIP.CUInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.CUInt
         ) => RIP.HasField "unsigned_int_pointer_member" (RIP.Ptr Unsigned_int_pointer_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_int_pointer_member")

{-| __C declaration:__ @struct ordinary_signed_long_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 253:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_long_pointer_struct = Ordinary_signed_long_pointer_struct
  { ordinary_signed_long_pointer_member :: RIP.Ptr RIP.CLong
    {- ^ __C declaration:__ @ordinary_signed_long_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 253:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_long_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_long_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_long_pointer_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_long_pointer_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_long_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_long_pointer_struct ordinary_signed_long_pointer_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_signed_long_pointer_member") ptr0 ordinary_signed_long_pointer_member2

deriving via Marshal.EquivStorable Ordinary_signed_long_pointer_struct instance RIP.Storable Ordinary_signed_long_pointer_struct

instance HasCField.HasCField Ordinary_signed_long_pointer_struct "ordinary_signed_long_pointer_member" where

  type CFieldType Ordinary_signed_long_pointer_struct "ordinary_signed_long_pointer_member" =
    RIP.Ptr RIP.CLong

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.CLong
         ) => RIP.HasField "ordinary_signed_long_pointer_member" (RIP.Ptr Ordinary_signed_long_pointer_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_long_pointer_member")

{-| __C declaration:__ @struct explicit_signed_long_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 254:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_long_pointer_struct = Explicit_signed_long_pointer_struct
  { explicit_signed_long_pointer_member :: RIP.Ptr RIP.CLong
    {- ^ __C declaration:__ @explicit_signed_long_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 254:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_long_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_long_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_long_pointer_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_long_pointer_member") ptr0

instance Marshal.WriteRaw Explicit_signed_long_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_long_pointer_struct explicit_signed_long_pointer_member2 ->
            HasCField.writeRaw (RIP.Proxy @"explicit_signed_long_pointer_member") ptr0 explicit_signed_long_pointer_member2

deriving via Marshal.EquivStorable Explicit_signed_long_pointer_struct instance RIP.Storable Explicit_signed_long_pointer_struct

instance HasCField.HasCField Explicit_signed_long_pointer_struct "explicit_signed_long_pointer_member" where

  type CFieldType Explicit_signed_long_pointer_struct "explicit_signed_long_pointer_member" =
    RIP.Ptr RIP.CLong

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.CLong
         ) => RIP.HasField "explicit_signed_long_pointer_member" (RIP.Ptr Explicit_signed_long_pointer_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_long_pointer_member")

{-| __C declaration:__ @struct unsigned_long_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 255:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_long_pointer_struct = Unsigned_long_pointer_struct
  { unsigned_long_pointer_member :: RIP.Ptr RIP.CULong
    {- ^ __C declaration:__ @unsigned_long_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 255:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_long_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_long_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_long_pointer_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_long_pointer_member") ptr0

instance Marshal.WriteRaw Unsigned_long_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_long_pointer_struct unsigned_long_pointer_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_long_pointer_member") ptr0 unsigned_long_pointer_member2

deriving via Marshal.EquivStorable Unsigned_long_pointer_struct instance RIP.Storable Unsigned_long_pointer_struct

instance HasCField.HasCField Unsigned_long_pointer_struct "unsigned_long_pointer_member" where

  type CFieldType Unsigned_long_pointer_struct "unsigned_long_pointer_member" =
    RIP.Ptr RIP.CULong

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.CULong
         ) => RIP.HasField "unsigned_long_pointer_member" (RIP.Ptr Unsigned_long_pointer_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_long_pointer_member")

{-| __C declaration:__ @struct ordinary_signed_long_long_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 257:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_long_long_pointer_struct = Ordinary_signed_long_long_pointer_struct
  { ordinary_signed_long_long_pointer_member :: RIP.Ptr RIP.CLLong
    {- ^ __C declaration:__ @ordinary_signed_long_long_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 257:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_long_long_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_long_long_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_long_long_pointer_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_long_long_pointer_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_long_long_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_long_long_pointer_struct
            ordinary_signed_long_long_pointer_member2 ->
              HasCField.writeRaw (RIP.Proxy @"ordinary_signed_long_long_pointer_member") ptr0 ordinary_signed_long_long_pointer_member2

deriving via Marshal.EquivStorable Ordinary_signed_long_long_pointer_struct instance RIP.Storable Ordinary_signed_long_long_pointer_struct

instance HasCField.HasCField Ordinary_signed_long_long_pointer_struct "ordinary_signed_long_long_pointer_member" where

  type CFieldType Ordinary_signed_long_long_pointer_struct "ordinary_signed_long_long_pointer_member" =
    RIP.Ptr RIP.CLLong

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.CLLong
         ) => RIP.HasField "ordinary_signed_long_long_pointer_member" (RIP.Ptr Ordinary_signed_long_long_pointer_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_long_long_pointer_member")

{-| __C declaration:__ @struct explicit_signed_long_long_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 258:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_long_long_pointer_struct = Explicit_signed_long_long_pointer_struct
  { explicit_signed_long_long_pointer_member :: RIP.Ptr RIP.CLLong
    {- ^ __C declaration:__ @explicit_signed_long_long_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 258:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_long_long_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_long_long_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_long_long_pointer_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_long_long_pointer_member") ptr0

instance Marshal.WriteRaw Explicit_signed_long_long_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_long_long_pointer_struct
            explicit_signed_long_long_pointer_member2 ->
              HasCField.writeRaw (RIP.Proxy @"explicit_signed_long_long_pointer_member") ptr0 explicit_signed_long_long_pointer_member2

deriving via Marshal.EquivStorable Explicit_signed_long_long_pointer_struct instance RIP.Storable Explicit_signed_long_long_pointer_struct

instance HasCField.HasCField Explicit_signed_long_long_pointer_struct "explicit_signed_long_long_pointer_member" where

  type CFieldType Explicit_signed_long_long_pointer_struct "explicit_signed_long_long_pointer_member" =
    RIP.Ptr RIP.CLLong

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.CLLong
         ) => RIP.HasField "explicit_signed_long_long_pointer_member" (RIP.Ptr Explicit_signed_long_long_pointer_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_long_long_pointer_member")

{-| __C declaration:__ @struct unsigned_long_long_pointer_struct@

    __defined at:__ @comprehensive\/c2hsc.h 259:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_long_long_pointer_struct = Unsigned_long_long_pointer_struct
  { unsigned_long_long_pointer_member :: RIP.Ptr RIP.CULLong
    {- ^ __C declaration:__ @unsigned_long_long_pointer_member@

         __defined at:__ @comprehensive\/c2hsc.h 259:71@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_long_long_pointer_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_long_long_pointer_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_long_long_pointer_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_long_long_pointer_member") ptr0

instance Marshal.WriteRaw Unsigned_long_long_pointer_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_long_long_pointer_struct unsigned_long_long_pointer_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_long_long_pointer_member") ptr0 unsigned_long_long_pointer_member2

deriving via Marshal.EquivStorable Unsigned_long_long_pointer_struct instance RIP.Storable Unsigned_long_long_pointer_struct

instance HasCField.HasCField Unsigned_long_long_pointer_struct "unsigned_long_long_pointer_member" where

  type CFieldType Unsigned_long_long_pointer_struct "unsigned_long_long_pointer_member" =
    RIP.Ptr RIP.CULLong

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.CULLong
         ) => RIP.HasField "unsigned_long_long_pointer_member" (RIP.Ptr Unsigned_long_long_pointer_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_long_long_pointer_member")

{-| Structs: arrays

    __C declaration:__ @struct ordinary_float_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 265:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_float_array_struct = Ordinary_float_array_struct
  { ordinary_float_array_member :: CA.ConstantArray 10 RIP.CFloat
    {- ^ __C declaration:__ @ordinary_float_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 265:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_float_array_struct where

  staticSizeOf = \_ -> (40 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Ordinary_float_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_float_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_float_array_member") ptr0

instance Marshal.WriteRaw Ordinary_float_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_float_array_struct ordinary_float_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_float_array_member") ptr0 ordinary_float_array_member2

deriving via Marshal.EquivStorable Ordinary_float_array_struct instance RIP.Storable Ordinary_float_array_struct

instance HasCField.HasCField Ordinary_float_array_struct "ordinary_float_array_member" where

  type CFieldType Ordinary_float_array_struct "ordinary_float_array_member" =
    CA.ConstantArray 10 RIP.CFloat

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 RIP.CFloat
         ) => RIP.HasField "ordinary_float_array_member" (RIP.Ptr Ordinary_float_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_float_array_member")

{-| __C declaration:__ @struct ordinary_double_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 266:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_double_array_struct = Ordinary_double_array_struct
  { ordinary_double_array_member :: CA.ConstantArray 10 RIP.CDouble
    {- ^ __C declaration:__ @ordinary_double_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 266:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_double_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_double_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_double_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_double_array_member") ptr0

instance Marshal.WriteRaw Ordinary_double_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_double_array_struct ordinary_double_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_double_array_member") ptr0 ordinary_double_array_member2

deriving via Marshal.EquivStorable Ordinary_double_array_struct instance RIP.Storable Ordinary_double_array_struct

instance HasCField.HasCField Ordinary_double_array_struct "ordinary_double_array_member" where

  type CFieldType Ordinary_double_array_struct "ordinary_double_array_member" =
    CA.ConstantArray 10 RIP.CDouble

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 RIP.CDouble
         ) => RIP.HasField "ordinary_double_array_member" (RIP.Ptr Ordinary_double_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_double_array_member")

{-| __C declaration:__ @struct ordinary_signed_char_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 269:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_char_array_struct = Ordinary_signed_char_array_struct
  { ordinary_signed_char_array_member :: CA.ConstantArray 10 RIP.CChar
    {- ^ __C declaration:__ @ordinary_signed_char_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 269:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_char_array_struct where

  staticSizeOf = \_ -> (10 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Ordinary_signed_char_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_char_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_char_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_char_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_char_array_struct ordinary_signed_char_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_signed_char_array_member") ptr0 ordinary_signed_char_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_char_array_struct instance RIP.Storable Ordinary_signed_char_array_struct

instance HasCField.HasCField Ordinary_signed_char_array_struct "ordinary_signed_char_array_member" where

  type CFieldType Ordinary_signed_char_array_struct "ordinary_signed_char_array_member" =
    CA.ConstantArray 10 RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 RIP.CChar
         ) => RIP.HasField "ordinary_signed_char_array_member" (RIP.Ptr Ordinary_signed_char_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_char_array_member")

{-| __C declaration:__ @struct explicit_signed_char_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 270:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_char_array_struct = Explicit_signed_char_array_struct
  { explicit_signed_char_array_member :: CA.ConstantArray 10 RIP.CSChar
    {- ^ __C declaration:__ @explicit_signed_char_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 270:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_char_array_struct where

  staticSizeOf = \_ -> (10 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Explicit_signed_char_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_char_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_char_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_char_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_char_array_struct explicit_signed_char_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"explicit_signed_char_array_member") ptr0 explicit_signed_char_array_member2

deriving via Marshal.EquivStorable Explicit_signed_char_array_struct instance RIP.Storable Explicit_signed_char_array_struct

instance HasCField.HasCField Explicit_signed_char_array_struct "explicit_signed_char_array_member" where

  type CFieldType Explicit_signed_char_array_struct "explicit_signed_char_array_member" =
    CA.ConstantArray 10 RIP.CSChar

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 RIP.CSChar
         ) => RIP.HasField "explicit_signed_char_array_member" (RIP.Ptr Explicit_signed_char_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_char_array_member")

{-| __C declaration:__ @struct unsigned_char_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 271:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_char_array_struct = Unsigned_char_array_struct
  { unsigned_char_array_member :: CA.ConstantArray 10 RIP.CUChar
    {- ^ __C declaration:__ @unsigned_char_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 271:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_char_array_struct where

  staticSizeOf = \_ -> (10 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Unsigned_char_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_char_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_char_array_member") ptr0

instance Marshal.WriteRaw Unsigned_char_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_char_array_struct unsigned_char_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_char_array_member") ptr0 unsigned_char_array_member2

deriving via Marshal.EquivStorable Unsigned_char_array_struct instance RIP.Storable Unsigned_char_array_struct

instance HasCField.HasCField Unsigned_char_array_struct "unsigned_char_array_member" where

  type CFieldType Unsigned_char_array_struct "unsigned_char_array_member" =
    CA.ConstantArray 10 RIP.CUChar

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 RIP.CUChar
         ) => RIP.HasField "unsigned_char_array_member" (RIP.Ptr Unsigned_char_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_char_array_member")

{-| __C declaration:__ @struct ordinary_signed_short_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 273:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_short_array_struct = Ordinary_signed_short_array_struct
  { ordinary_signed_short_array_member :: CA.ConstantArray 10 RIP.CShort
    {- ^ __C declaration:__ @ordinary_signed_short_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 273:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_short_array_struct where

  staticSizeOf = \_ -> (20 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Ordinary_signed_short_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_short_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_short_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_short_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_short_array_struct ordinary_signed_short_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_signed_short_array_member") ptr0 ordinary_signed_short_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_short_array_struct instance RIP.Storable Ordinary_signed_short_array_struct

instance HasCField.HasCField Ordinary_signed_short_array_struct "ordinary_signed_short_array_member" where

  type CFieldType Ordinary_signed_short_array_struct "ordinary_signed_short_array_member" =
    CA.ConstantArray 10 RIP.CShort

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 RIP.CShort
         ) => RIP.HasField "ordinary_signed_short_array_member" (RIP.Ptr Ordinary_signed_short_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_short_array_member")

{-| __C declaration:__ @struct explicit_signed_short_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 274:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_short_array_struct = Explicit_signed_short_array_struct
  { explicit_signed_short_array_member :: CA.ConstantArray 10 RIP.CShort
    {- ^ __C declaration:__ @explicit_signed_short_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 274:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_short_array_struct where

  staticSizeOf = \_ -> (20 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Explicit_signed_short_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_short_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_short_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_short_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_short_array_struct explicit_signed_short_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"explicit_signed_short_array_member") ptr0 explicit_signed_short_array_member2

deriving via Marshal.EquivStorable Explicit_signed_short_array_struct instance RIP.Storable Explicit_signed_short_array_struct

instance HasCField.HasCField Explicit_signed_short_array_struct "explicit_signed_short_array_member" where

  type CFieldType Explicit_signed_short_array_struct "explicit_signed_short_array_member" =
    CA.ConstantArray 10 RIP.CShort

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 RIP.CShort
         ) => RIP.HasField "explicit_signed_short_array_member" (RIP.Ptr Explicit_signed_short_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_short_array_member")

{-| __C declaration:__ @struct unsigned_short_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 275:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_short_array_struct = Unsigned_short_array_struct
  { unsigned_short_array_member :: CA.ConstantArray 10 RIP.CUShort
    {- ^ __C declaration:__ @unsigned_short_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 275:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_short_array_struct where

  staticSizeOf = \_ -> (20 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Unsigned_short_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_short_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_short_array_member") ptr0

instance Marshal.WriteRaw Unsigned_short_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_short_array_struct unsigned_short_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_short_array_member") ptr0 unsigned_short_array_member2

deriving via Marshal.EquivStorable Unsigned_short_array_struct instance RIP.Storable Unsigned_short_array_struct

instance HasCField.HasCField Unsigned_short_array_struct "unsigned_short_array_member" where

  type CFieldType Unsigned_short_array_struct "unsigned_short_array_member" =
    CA.ConstantArray 10 RIP.CUShort

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 RIP.CUShort
         ) => RIP.HasField "unsigned_short_array_member" (RIP.Ptr Unsigned_short_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_short_array_member")

{-| __C declaration:__ @struct ordinary_signed_int_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 277:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_int_array_struct = Ordinary_signed_int_array_struct
  { ordinary_signed_int_array_member :: CA.ConstantArray 10 RIP.CInt
    {- ^ __C declaration:__ @ordinary_signed_int_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 277:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_int_array_struct where

  staticSizeOf = \_ -> (40 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Ordinary_signed_int_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_int_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_int_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_int_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_int_array_struct ordinary_signed_int_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_signed_int_array_member") ptr0 ordinary_signed_int_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_int_array_struct instance RIP.Storable Ordinary_signed_int_array_struct

instance HasCField.HasCField Ordinary_signed_int_array_struct "ordinary_signed_int_array_member" where

  type CFieldType Ordinary_signed_int_array_struct "ordinary_signed_int_array_member" =
    CA.ConstantArray 10 RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 RIP.CInt
         ) => RIP.HasField "ordinary_signed_int_array_member" (RIP.Ptr Ordinary_signed_int_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_int_array_member")

{-| __C declaration:__ @struct explicit_signed_int_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 278:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_int_array_struct = Explicit_signed_int_array_struct
  { explicit_signed_int_array_member :: CA.ConstantArray 10 RIP.CInt
    {- ^ __C declaration:__ @explicit_signed_int_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 278:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_int_array_struct where

  staticSizeOf = \_ -> (40 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Explicit_signed_int_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_int_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_int_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_int_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_int_array_struct explicit_signed_int_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"explicit_signed_int_array_member") ptr0 explicit_signed_int_array_member2

deriving via Marshal.EquivStorable Explicit_signed_int_array_struct instance RIP.Storable Explicit_signed_int_array_struct

instance HasCField.HasCField Explicit_signed_int_array_struct "explicit_signed_int_array_member" where

  type CFieldType Explicit_signed_int_array_struct "explicit_signed_int_array_member" =
    CA.ConstantArray 10 RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 RIP.CInt
         ) => RIP.HasField "explicit_signed_int_array_member" (RIP.Ptr Explicit_signed_int_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_int_array_member")

{-| __C declaration:__ @struct unsigned_int_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 279:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_int_array_struct = Unsigned_int_array_struct
  { unsigned_int_array_member :: CA.ConstantArray 10 RIP.CUInt
    {- ^ __C declaration:__ @unsigned_int_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 279:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_int_array_struct where

  staticSizeOf = \_ -> (40 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Unsigned_int_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_int_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_int_array_member") ptr0

instance Marshal.WriteRaw Unsigned_int_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_int_array_struct unsigned_int_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_int_array_member") ptr0 unsigned_int_array_member2

deriving via Marshal.EquivStorable Unsigned_int_array_struct instance RIP.Storable Unsigned_int_array_struct

instance HasCField.HasCField Unsigned_int_array_struct "unsigned_int_array_member" where

  type CFieldType Unsigned_int_array_struct "unsigned_int_array_member" =
    CA.ConstantArray 10 RIP.CUInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 RIP.CUInt
         ) => RIP.HasField "unsigned_int_array_member" (RIP.Ptr Unsigned_int_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_int_array_member")

{-| __C declaration:__ @struct ordinary_signed_long_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 281:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_long_array_struct = Ordinary_signed_long_array_struct
  { ordinary_signed_long_array_member :: CA.ConstantArray 10 RIP.CLong
    {- ^ __C declaration:__ @ordinary_signed_long_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 281:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_long_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_long_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_long_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_long_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_long_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_long_array_struct ordinary_signed_long_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_signed_long_array_member") ptr0 ordinary_signed_long_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_long_array_struct instance RIP.Storable Ordinary_signed_long_array_struct

instance HasCField.HasCField Ordinary_signed_long_array_struct "ordinary_signed_long_array_member" where

  type CFieldType Ordinary_signed_long_array_struct "ordinary_signed_long_array_member" =
    CA.ConstantArray 10 RIP.CLong

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 RIP.CLong
         ) => RIP.HasField "ordinary_signed_long_array_member" (RIP.Ptr Ordinary_signed_long_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_long_array_member")

{-| __C declaration:__ @struct explicit_signed_long_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 282:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_long_array_struct = Explicit_signed_long_array_struct
  { explicit_signed_long_array_member :: CA.ConstantArray 10 RIP.CLong
    {- ^ __C declaration:__ @explicit_signed_long_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 282:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_long_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_long_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_long_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_long_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_long_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_long_array_struct explicit_signed_long_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"explicit_signed_long_array_member") ptr0 explicit_signed_long_array_member2

deriving via Marshal.EquivStorable Explicit_signed_long_array_struct instance RIP.Storable Explicit_signed_long_array_struct

instance HasCField.HasCField Explicit_signed_long_array_struct "explicit_signed_long_array_member" where

  type CFieldType Explicit_signed_long_array_struct "explicit_signed_long_array_member" =
    CA.ConstantArray 10 RIP.CLong

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 RIP.CLong
         ) => RIP.HasField "explicit_signed_long_array_member" (RIP.Ptr Explicit_signed_long_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_long_array_member")

{-| __C declaration:__ @struct unsigned_long_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 283:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_long_array_struct = Unsigned_long_array_struct
  { unsigned_long_array_member :: CA.ConstantArray 10 RIP.CULong
    {- ^ __C declaration:__ @unsigned_long_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 283:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_long_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_long_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_long_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_long_array_member") ptr0

instance Marshal.WriteRaw Unsigned_long_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_long_array_struct unsigned_long_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_long_array_member") ptr0 unsigned_long_array_member2

deriving via Marshal.EquivStorable Unsigned_long_array_struct instance RIP.Storable Unsigned_long_array_struct

instance HasCField.HasCField Unsigned_long_array_struct "unsigned_long_array_member" where

  type CFieldType Unsigned_long_array_struct "unsigned_long_array_member" =
    CA.ConstantArray 10 RIP.CULong

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 RIP.CULong
         ) => RIP.HasField "unsigned_long_array_member" (RIP.Ptr Unsigned_long_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_long_array_member")

{-| __C declaration:__ @struct ordinary_signed_long_long_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 285:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_long_long_array_struct = Ordinary_signed_long_long_array_struct
  { ordinary_signed_long_long_array_member :: CA.ConstantArray 10 RIP.CLLong
    {- ^ __C declaration:__ @ordinary_signed_long_long_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 285:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_long_long_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_long_long_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_long_long_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_long_long_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_long_long_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_long_long_array_struct
            ordinary_signed_long_long_array_member2 ->
              HasCField.writeRaw (RIP.Proxy @"ordinary_signed_long_long_array_member") ptr0 ordinary_signed_long_long_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_long_long_array_struct instance RIP.Storable Ordinary_signed_long_long_array_struct

instance HasCField.HasCField Ordinary_signed_long_long_array_struct "ordinary_signed_long_long_array_member" where

  type CFieldType Ordinary_signed_long_long_array_struct "ordinary_signed_long_long_array_member" =
    CA.ConstantArray 10 RIP.CLLong

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 RIP.CLLong
         ) => RIP.HasField "ordinary_signed_long_long_array_member" (RIP.Ptr Ordinary_signed_long_long_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_long_long_array_member")

{-| __C declaration:__ @struct explicit_signed_long_long_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 286:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_long_long_array_struct = Explicit_signed_long_long_array_struct
  { explicit_signed_long_long_array_member :: CA.ConstantArray 10 RIP.CLLong
    {- ^ __C declaration:__ @explicit_signed_long_long_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 286:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_long_long_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_long_long_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_long_long_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_long_long_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_long_long_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_long_long_array_struct
            explicit_signed_long_long_array_member2 ->
              HasCField.writeRaw (RIP.Proxy @"explicit_signed_long_long_array_member") ptr0 explicit_signed_long_long_array_member2

deriving via Marshal.EquivStorable Explicit_signed_long_long_array_struct instance RIP.Storable Explicit_signed_long_long_array_struct

instance HasCField.HasCField Explicit_signed_long_long_array_struct "explicit_signed_long_long_array_member" where

  type CFieldType Explicit_signed_long_long_array_struct "explicit_signed_long_long_array_member" =
    CA.ConstantArray 10 RIP.CLLong

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 RIP.CLLong
         ) => RIP.HasField "explicit_signed_long_long_array_member" (RIP.Ptr Explicit_signed_long_long_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_long_long_array_member")

{-| __C declaration:__ @struct unsigned_long_long_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 287:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_long_long_array_struct = Unsigned_long_long_array_struct
  { unsigned_long_long_array_member :: CA.ConstantArray 10 RIP.CULLong
    {- ^ __C declaration:__ @unsigned_long_long_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 287:68@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_long_long_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_long_long_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_long_long_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_long_long_array_member") ptr0

instance Marshal.WriteRaw Unsigned_long_long_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_long_long_array_struct unsigned_long_long_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_long_long_array_member") ptr0 unsigned_long_long_array_member2

deriving via Marshal.EquivStorable Unsigned_long_long_array_struct instance RIP.Storable Unsigned_long_long_array_struct

instance HasCField.HasCField Unsigned_long_long_array_struct "unsigned_long_long_array_member" where

  type CFieldType Unsigned_long_long_array_struct "unsigned_long_long_array_member" =
    CA.ConstantArray 10 RIP.CULLong

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 RIP.CULLong
         ) => RIP.HasField "unsigned_long_long_array_member" (RIP.Ptr Unsigned_long_long_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_long_long_array_member")

{-| Structs: arrays of pointers

    NOTE: Here too @'Ordinary_signed_char_pointer_array_struct'@ was commented out in the original test suite, with no reason given.

    __C declaration:__ @struct ordinary_void_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 296:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_void_pointer_array_struct = Ordinary_void_pointer_array_struct
  { ordinary_void_pointer_array_member :: CA.ConstantArray 10 (RIP.Ptr RIP.Void)
    {- ^ __C declaration:__ @ordinary_void_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 296:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_void_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_void_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_void_pointer_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_void_pointer_array_member") ptr0

instance Marshal.WriteRaw Ordinary_void_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_void_pointer_array_struct ordinary_void_pointer_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_void_pointer_array_member") ptr0 ordinary_void_pointer_array_member2

deriving via Marshal.EquivStorable Ordinary_void_pointer_array_struct instance RIP.Storable Ordinary_void_pointer_array_struct

instance HasCField.HasCField Ordinary_void_pointer_array_struct "ordinary_void_pointer_array_member" where

  type CFieldType Ordinary_void_pointer_array_struct "ordinary_void_pointer_array_member" =
    CA.ConstantArray 10 (RIP.Ptr RIP.Void)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 (RIP.Ptr RIP.Void)
         ) => RIP.HasField "ordinary_void_pointer_array_member" (RIP.Ptr Ordinary_void_pointer_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_void_pointer_array_member")

{-| __C declaration:__ @struct ordinary_float_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 298:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_float_pointer_array_struct = Ordinary_float_pointer_array_struct
  { ordinary_float_pointer_array_member :: CA.ConstantArray 10 (RIP.Ptr RIP.CFloat)
    {- ^ __C declaration:__ @ordinary_float_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 298:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_float_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_float_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_float_pointer_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_float_pointer_array_member") ptr0

instance Marshal.WriteRaw Ordinary_float_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_float_pointer_array_struct ordinary_float_pointer_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_float_pointer_array_member") ptr0 ordinary_float_pointer_array_member2

deriving via Marshal.EquivStorable Ordinary_float_pointer_array_struct instance RIP.Storable Ordinary_float_pointer_array_struct

instance HasCField.HasCField Ordinary_float_pointer_array_struct "ordinary_float_pointer_array_member" where

  type CFieldType Ordinary_float_pointer_array_struct "ordinary_float_pointer_array_member" =
    CA.ConstantArray 10 (RIP.Ptr RIP.CFloat)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 (RIP.Ptr RIP.CFloat)
         ) => RIP.HasField "ordinary_float_pointer_array_member" (RIP.Ptr Ordinary_float_pointer_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_float_pointer_array_member")

{-| __C declaration:__ @struct ordinary_double_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 299:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_double_pointer_array_struct = Ordinary_double_pointer_array_struct
  { ordinary_double_pointer_array_member :: CA.ConstantArray 10 (RIP.Ptr RIP.CDouble)
    {- ^ __C declaration:__ @ordinary_double_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 299:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_double_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_double_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_double_pointer_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_double_pointer_array_member") ptr0

instance Marshal.WriteRaw Ordinary_double_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_double_pointer_array_struct ordinary_double_pointer_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"ordinary_double_pointer_array_member") ptr0 ordinary_double_pointer_array_member2

deriving via Marshal.EquivStorable Ordinary_double_pointer_array_struct instance RIP.Storable Ordinary_double_pointer_array_struct

instance HasCField.HasCField Ordinary_double_pointer_array_struct "ordinary_double_pointer_array_member" where

  type CFieldType Ordinary_double_pointer_array_struct "ordinary_double_pointer_array_member" =
    CA.ConstantArray 10 (RIP.Ptr RIP.CDouble)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 (RIP.Ptr RIP.CDouble)
         ) => RIP.HasField "ordinary_double_pointer_array_member" (RIP.Ptr Ordinary_double_pointer_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_double_pointer_array_member")

{-| __C declaration:__ @struct ordinary_signed_char_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 302:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_char_pointer_array_struct = Ordinary_signed_char_pointer_array_struct
  { ordinary_signed_char_pointer_array_member :: CA.ConstantArray 10 (RIP.Ptr RIP.CChar)
    {- ^ __C declaration:__ @ordinary_signed_char_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 302:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_char_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_char_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_char_pointer_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_char_pointer_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_char_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_char_pointer_array_struct
            ordinary_signed_char_pointer_array_member2 ->
              HasCField.writeRaw (RIP.Proxy @"ordinary_signed_char_pointer_array_member") ptr0 ordinary_signed_char_pointer_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_char_pointer_array_struct instance RIP.Storable Ordinary_signed_char_pointer_array_struct

instance HasCField.HasCField Ordinary_signed_char_pointer_array_struct "ordinary_signed_char_pointer_array_member" where

  type CFieldType Ordinary_signed_char_pointer_array_struct "ordinary_signed_char_pointer_array_member" =
    CA.ConstantArray 10 (RIP.Ptr RIP.CChar)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 (RIP.Ptr RIP.CChar)
         ) => RIP.HasField "ordinary_signed_char_pointer_array_member" (RIP.Ptr Ordinary_signed_char_pointer_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_char_pointer_array_member")

{-| __C declaration:__ @struct explicit_signed_char_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 303:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_char_pointer_array_struct = Explicit_signed_char_pointer_array_struct
  { explicit_signed_char_pointer_array_member :: CA.ConstantArray 10 (RIP.Ptr RIP.CSChar)
    {- ^ __C declaration:__ @explicit_signed_char_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 303:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_char_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_char_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_char_pointer_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_char_pointer_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_char_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_char_pointer_array_struct
            explicit_signed_char_pointer_array_member2 ->
              HasCField.writeRaw (RIP.Proxy @"explicit_signed_char_pointer_array_member") ptr0 explicit_signed_char_pointer_array_member2

deriving via Marshal.EquivStorable Explicit_signed_char_pointer_array_struct instance RIP.Storable Explicit_signed_char_pointer_array_struct

instance HasCField.HasCField Explicit_signed_char_pointer_array_struct "explicit_signed_char_pointer_array_member" where

  type CFieldType Explicit_signed_char_pointer_array_struct "explicit_signed_char_pointer_array_member" =
    CA.ConstantArray 10 (RIP.Ptr RIP.CSChar)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 (RIP.Ptr RIP.CSChar)
         ) => RIP.HasField "explicit_signed_char_pointer_array_member" (RIP.Ptr Explicit_signed_char_pointer_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_char_pointer_array_member")

{-| __C declaration:__ @struct unsigned_char_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 304:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_char_pointer_array_struct = Unsigned_char_pointer_array_struct
  { unsigned_char_pointer_array_member :: CA.ConstantArray 10 (RIP.Ptr RIP.CUChar)
    {- ^ __C declaration:__ @unsigned_char_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 304:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_char_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_char_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_char_pointer_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_char_pointer_array_member") ptr0

instance Marshal.WriteRaw Unsigned_char_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_char_pointer_array_struct unsigned_char_pointer_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_char_pointer_array_member") ptr0 unsigned_char_pointer_array_member2

deriving via Marshal.EquivStorable Unsigned_char_pointer_array_struct instance RIP.Storable Unsigned_char_pointer_array_struct

instance HasCField.HasCField Unsigned_char_pointer_array_struct "unsigned_char_pointer_array_member" where

  type CFieldType Unsigned_char_pointer_array_struct "unsigned_char_pointer_array_member" =
    CA.ConstantArray 10 (RIP.Ptr RIP.CUChar)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 (RIP.Ptr RIP.CUChar)
         ) => RIP.HasField "unsigned_char_pointer_array_member" (RIP.Ptr Unsigned_char_pointer_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_char_pointer_array_member")

{-| __C declaration:__ @struct ordinary_signed_short_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 306:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_short_pointer_array_struct = Ordinary_signed_short_pointer_array_struct
  { ordinary_signed_short_pointer_array_member :: CA.ConstantArray 10 (RIP.Ptr RIP.CShort)
    {- ^ __C declaration:__ @ordinary_signed_short_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 306:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_short_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_short_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_short_pointer_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_short_pointer_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_short_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_short_pointer_array_struct
            ordinary_signed_short_pointer_array_member2 ->
              HasCField.writeRaw (RIP.Proxy @"ordinary_signed_short_pointer_array_member") ptr0 ordinary_signed_short_pointer_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_short_pointer_array_struct instance RIP.Storable Ordinary_signed_short_pointer_array_struct

instance HasCField.HasCField Ordinary_signed_short_pointer_array_struct "ordinary_signed_short_pointer_array_member" where

  type CFieldType Ordinary_signed_short_pointer_array_struct "ordinary_signed_short_pointer_array_member" =
    CA.ConstantArray 10 (RIP.Ptr RIP.CShort)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 (RIP.Ptr RIP.CShort)
         ) => RIP.HasField "ordinary_signed_short_pointer_array_member" (RIP.Ptr Ordinary_signed_short_pointer_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_short_pointer_array_member")

{-| __C declaration:__ @struct explicit_signed_short_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 307:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_short_pointer_array_struct = Explicit_signed_short_pointer_array_struct
  { explicit_signed_short_pointer_array_member :: CA.ConstantArray 10 (RIP.Ptr RIP.CShort)
    {- ^ __C declaration:__ @explicit_signed_short_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 307:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_short_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_short_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_short_pointer_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_short_pointer_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_short_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_short_pointer_array_struct
            explicit_signed_short_pointer_array_member2 ->
              HasCField.writeRaw (RIP.Proxy @"explicit_signed_short_pointer_array_member") ptr0 explicit_signed_short_pointer_array_member2

deriving via Marshal.EquivStorable Explicit_signed_short_pointer_array_struct instance RIP.Storable Explicit_signed_short_pointer_array_struct

instance HasCField.HasCField Explicit_signed_short_pointer_array_struct "explicit_signed_short_pointer_array_member" where

  type CFieldType Explicit_signed_short_pointer_array_struct "explicit_signed_short_pointer_array_member" =
    CA.ConstantArray 10 (RIP.Ptr RIP.CShort)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 (RIP.Ptr RIP.CShort)
         ) => RIP.HasField "explicit_signed_short_pointer_array_member" (RIP.Ptr Explicit_signed_short_pointer_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_short_pointer_array_member")

{-| __C declaration:__ @struct unsigned_short_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 308:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_short_pointer_array_struct = Unsigned_short_pointer_array_struct
  { unsigned_short_pointer_array_member :: CA.ConstantArray 10 (RIP.Ptr RIP.CUShort)
    {- ^ __C declaration:__ @unsigned_short_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 308:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_short_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_short_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_short_pointer_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_short_pointer_array_member") ptr0

instance Marshal.WriteRaw Unsigned_short_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_short_pointer_array_struct unsigned_short_pointer_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_short_pointer_array_member") ptr0 unsigned_short_pointer_array_member2

deriving via Marshal.EquivStorable Unsigned_short_pointer_array_struct instance RIP.Storable Unsigned_short_pointer_array_struct

instance HasCField.HasCField Unsigned_short_pointer_array_struct "unsigned_short_pointer_array_member" where

  type CFieldType Unsigned_short_pointer_array_struct "unsigned_short_pointer_array_member" =
    CA.ConstantArray 10 (RIP.Ptr RIP.CUShort)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 (RIP.Ptr RIP.CUShort)
         ) => RIP.HasField "unsigned_short_pointer_array_member" (RIP.Ptr Unsigned_short_pointer_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_short_pointer_array_member")

{-| __C declaration:__ @struct ordinary_signed_int_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 310:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_int_pointer_array_struct = Ordinary_signed_int_pointer_array_struct
  { ordinary_signed_int_pointer_array_member :: CA.ConstantArray 10 (RIP.Ptr RIP.CInt)
    {- ^ __C declaration:__ @ordinary_signed_int_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 310:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_int_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_int_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_int_pointer_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_int_pointer_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_int_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_int_pointer_array_struct
            ordinary_signed_int_pointer_array_member2 ->
              HasCField.writeRaw (RIP.Proxy @"ordinary_signed_int_pointer_array_member") ptr0 ordinary_signed_int_pointer_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_int_pointer_array_struct instance RIP.Storable Ordinary_signed_int_pointer_array_struct

instance HasCField.HasCField Ordinary_signed_int_pointer_array_struct "ordinary_signed_int_pointer_array_member" where

  type CFieldType Ordinary_signed_int_pointer_array_struct "ordinary_signed_int_pointer_array_member" =
    CA.ConstantArray 10 (RIP.Ptr RIP.CInt)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 (RIP.Ptr RIP.CInt)
         ) => RIP.HasField "ordinary_signed_int_pointer_array_member" (RIP.Ptr Ordinary_signed_int_pointer_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_int_pointer_array_member")

{-| __C declaration:__ @struct explicit_signed_int_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 311:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_int_pointer_array_struct = Explicit_signed_int_pointer_array_struct
  { explicit_signed_int_pointer_array_member :: CA.ConstantArray 10 (RIP.Ptr RIP.CInt)
    {- ^ __C declaration:__ @explicit_signed_int_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 311:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_int_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_int_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_int_pointer_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_int_pointer_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_int_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_int_pointer_array_struct
            explicit_signed_int_pointer_array_member2 ->
              HasCField.writeRaw (RIP.Proxy @"explicit_signed_int_pointer_array_member") ptr0 explicit_signed_int_pointer_array_member2

deriving via Marshal.EquivStorable Explicit_signed_int_pointer_array_struct instance RIP.Storable Explicit_signed_int_pointer_array_struct

instance HasCField.HasCField Explicit_signed_int_pointer_array_struct "explicit_signed_int_pointer_array_member" where

  type CFieldType Explicit_signed_int_pointer_array_struct "explicit_signed_int_pointer_array_member" =
    CA.ConstantArray 10 (RIP.Ptr RIP.CInt)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 (RIP.Ptr RIP.CInt)
         ) => RIP.HasField "explicit_signed_int_pointer_array_member" (RIP.Ptr Explicit_signed_int_pointer_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_int_pointer_array_member")

{-| __C declaration:__ @struct unsigned_int_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 312:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_int_pointer_array_struct = Unsigned_int_pointer_array_struct
  { unsigned_int_pointer_array_member :: CA.ConstantArray 10 (RIP.Ptr RIP.CUInt)
    {- ^ __C declaration:__ @unsigned_int_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 312:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_int_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_int_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_int_pointer_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_int_pointer_array_member") ptr0

instance Marshal.WriteRaw Unsigned_int_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_int_pointer_array_struct unsigned_int_pointer_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_int_pointer_array_member") ptr0 unsigned_int_pointer_array_member2

deriving via Marshal.EquivStorable Unsigned_int_pointer_array_struct instance RIP.Storable Unsigned_int_pointer_array_struct

instance HasCField.HasCField Unsigned_int_pointer_array_struct "unsigned_int_pointer_array_member" where

  type CFieldType Unsigned_int_pointer_array_struct "unsigned_int_pointer_array_member" =
    CA.ConstantArray 10 (RIP.Ptr RIP.CUInt)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 (RIP.Ptr RIP.CUInt)
         ) => RIP.HasField "unsigned_int_pointer_array_member" (RIP.Ptr Unsigned_int_pointer_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_int_pointer_array_member")

{-| __C declaration:__ @struct ordinary_signed_long_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 314:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_long_pointer_array_struct = Ordinary_signed_long_pointer_array_struct
  { ordinary_signed_long_pointer_array_member :: CA.ConstantArray 10 (RIP.Ptr RIP.CLong)
    {- ^ __C declaration:__ @ordinary_signed_long_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 314:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_long_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_long_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_long_pointer_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_long_pointer_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_long_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_long_pointer_array_struct
            ordinary_signed_long_pointer_array_member2 ->
              HasCField.writeRaw (RIP.Proxy @"ordinary_signed_long_pointer_array_member") ptr0 ordinary_signed_long_pointer_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_long_pointer_array_struct instance RIP.Storable Ordinary_signed_long_pointer_array_struct

instance HasCField.HasCField Ordinary_signed_long_pointer_array_struct "ordinary_signed_long_pointer_array_member" where

  type CFieldType Ordinary_signed_long_pointer_array_struct "ordinary_signed_long_pointer_array_member" =
    CA.ConstantArray 10 (RIP.Ptr RIP.CLong)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 (RIP.Ptr RIP.CLong)
         ) => RIP.HasField "ordinary_signed_long_pointer_array_member" (RIP.Ptr Ordinary_signed_long_pointer_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_long_pointer_array_member")

{-| __C declaration:__ @struct explicit_signed_long_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 315:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_long_pointer_array_struct = Explicit_signed_long_pointer_array_struct
  { explicit_signed_long_pointer_array_member :: CA.ConstantArray 10 (RIP.Ptr RIP.CLong)
    {- ^ __C declaration:__ @explicit_signed_long_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 315:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_long_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_long_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_long_pointer_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_long_pointer_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_long_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_long_pointer_array_struct
            explicit_signed_long_pointer_array_member2 ->
              HasCField.writeRaw (RIP.Proxy @"explicit_signed_long_pointer_array_member") ptr0 explicit_signed_long_pointer_array_member2

deriving via Marshal.EquivStorable Explicit_signed_long_pointer_array_struct instance RIP.Storable Explicit_signed_long_pointer_array_struct

instance HasCField.HasCField Explicit_signed_long_pointer_array_struct "explicit_signed_long_pointer_array_member" where

  type CFieldType Explicit_signed_long_pointer_array_struct "explicit_signed_long_pointer_array_member" =
    CA.ConstantArray 10 (RIP.Ptr RIP.CLong)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 (RIP.Ptr RIP.CLong)
         ) => RIP.HasField "explicit_signed_long_pointer_array_member" (RIP.Ptr Explicit_signed_long_pointer_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_long_pointer_array_member")

{-| __C declaration:__ @struct unsigned_long_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 316:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_long_pointer_array_struct = Unsigned_long_pointer_array_struct
  { unsigned_long_pointer_array_member :: CA.ConstantArray 10 (RIP.Ptr RIP.CULong)
    {- ^ __C declaration:__ @unsigned_long_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 316:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_long_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_long_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_long_pointer_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_long_pointer_array_member") ptr0

instance Marshal.WriteRaw Unsigned_long_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_long_pointer_array_struct unsigned_long_pointer_array_member2 ->
            HasCField.writeRaw (RIP.Proxy @"unsigned_long_pointer_array_member") ptr0 unsigned_long_pointer_array_member2

deriving via Marshal.EquivStorable Unsigned_long_pointer_array_struct instance RIP.Storable Unsigned_long_pointer_array_struct

instance HasCField.HasCField Unsigned_long_pointer_array_struct "unsigned_long_pointer_array_member" where

  type CFieldType Unsigned_long_pointer_array_struct "unsigned_long_pointer_array_member" =
    CA.ConstantArray 10 (RIP.Ptr RIP.CULong)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 (RIP.Ptr RIP.CULong)
         ) => RIP.HasField "unsigned_long_pointer_array_member" (RIP.Ptr Unsigned_long_pointer_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_long_pointer_array_member")

{-| __C declaration:__ @struct ordinary_signed_long_long_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 318:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Ordinary_signed_long_long_pointer_array_struct = Ordinary_signed_long_long_pointer_array_struct
  { ordinary_signed_long_long_pointer_array_member :: CA.ConstantArray 10 (RIP.Ptr RIP.CLLong)
    {- ^ __C declaration:__ @ordinary_signed_long_long_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 318:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ordinary_signed_long_long_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ordinary_signed_long_long_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Ordinary_signed_long_long_pointer_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"ordinary_signed_long_long_pointer_array_member") ptr0

instance Marshal.WriteRaw Ordinary_signed_long_long_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordinary_signed_long_long_pointer_array_struct
            ordinary_signed_long_long_pointer_array_member2 ->
              HasCField.writeRaw (RIP.Proxy @"ordinary_signed_long_long_pointer_array_member") ptr0 ordinary_signed_long_long_pointer_array_member2

deriving via Marshal.EquivStorable Ordinary_signed_long_long_pointer_array_struct instance RIP.Storable Ordinary_signed_long_long_pointer_array_struct

instance HasCField.HasCField Ordinary_signed_long_long_pointer_array_struct "ordinary_signed_long_long_pointer_array_member" where

  type CFieldType Ordinary_signed_long_long_pointer_array_struct "ordinary_signed_long_long_pointer_array_member" =
    CA.ConstantArray 10 (RIP.Ptr RIP.CLLong)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 (RIP.Ptr RIP.CLLong)
         ) => RIP.HasField "ordinary_signed_long_long_pointer_array_member" (RIP.Ptr Ordinary_signed_long_long_pointer_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ordinary_signed_long_long_pointer_array_member")

{-| __C declaration:__ @struct explicit_signed_long_long_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 319:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Explicit_signed_long_long_pointer_array_struct = Explicit_signed_long_long_pointer_array_struct
  { explicit_signed_long_long_pointer_array_member :: CA.ConstantArray 10 (RIP.Ptr RIP.CLLong)
    {- ^ __C declaration:__ @explicit_signed_long_long_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 319:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Explicit_signed_long_long_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Explicit_signed_long_long_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Explicit_signed_long_long_pointer_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"explicit_signed_long_long_pointer_array_member") ptr0

instance Marshal.WriteRaw Explicit_signed_long_long_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Explicit_signed_long_long_pointer_array_struct
            explicit_signed_long_long_pointer_array_member2 ->
              HasCField.writeRaw (RIP.Proxy @"explicit_signed_long_long_pointer_array_member") ptr0 explicit_signed_long_long_pointer_array_member2

deriving via Marshal.EquivStorable Explicit_signed_long_long_pointer_array_struct instance RIP.Storable Explicit_signed_long_long_pointer_array_struct

instance HasCField.HasCField Explicit_signed_long_long_pointer_array_struct "explicit_signed_long_long_pointer_array_member" where

  type CFieldType Explicit_signed_long_long_pointer_array_struct "explicit_signed_long_long_pointer_array_member" =
    CA.ConstantArray 10 (RIP.Ptr RIP.CLLong)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 (RIP.Ptr RIP.CLLong)
         ) => RIP.HasField "explicit_signed_long_long_pointer_array_member" (RIP.Ptr Explicit_signed_long_long_pointer_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"explicit_signed_long_long_pointer_array_member")

{-| __C declaration:__ @struct unsigned_long_long_pointer_array_struct@

    __defined at:__ @comprehensive\/c2hsc.h 320:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Unsigned_long_long_pointer_array_struct = Unsigned_long_long_pointer_array_struct
  { unsigned_long_long_pointer_array_member :: CA.ConstantArray 10 (RIP.Ptr RIP.CULLong)
    {- ^ __C declaration:__ @unsigned_long_long_pointer_array_member@

         __defined at:__ @comprehensive\/c2hsc.h 320:77@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unsigned_long_long_pointer_array_struct where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Unsigned_long_long_pointer_array_struct where

  readRaw =
    \ptr0 ->
          pure Unsigned_long_long_pointer_array_struct
      <*> HasCField.readRaw (RIP.Proxy @"unsigned_long_long_pointer_array_member") ptr0

instance Marshal.WriteRaw Unsigned_long_long_pointer_array_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unsigned_long_long_pointer_array_struct
            unsigned_long_long_pointer_array_member2 ->
              HasCField.writeRaw (RIP.Proxy @"unsigned_long_long_pointer_array_member") ptr0 unsigned_long_long_pointer_array_member2

deriving via Marshal.EquivStorable Unsigned_long_long_pointer_array_struct instance RIP.Storable Unsigned_long_long_pointer_array_struct

instance HasCField.HasCField Unsigned_long_long_pointer_array_struct "unsigned_long_long_pointer_array_member" where

  type CFieldType Unsigned_long_long_pointer_array_struct "unsigned_long_long_pointer_array_member" =
    CA.ConstantArray 10 (RIP.Ptr RIP.CULLong)

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 10 (RIP.Ptr RIP.CULLong)
         ) => RIP.HasField "unsigned_long_long_pointer_array_member" (RIP.Ptr Unsigned_long_long_pointer_array_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unsigned_long_long_pointer_array_member")

{-| Sanity checks

    NOTE: The @smoke.h@ test is moved to a separate header.

    __C declaration:__ @an_int@

    __defined at:__ @comprehensive\/c2hsc.h 329:13@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype An_int = An_int
  { unwrap :: RIP.CInt
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
         ) => RIP.HasField "unwrap" (RIP.Ptr An_int) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField An_int "unwrap" where

  type CFieldType An_int "unwrap" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@cal_table_table@

    __defined at:__ @comprehensive\/c2hsc.h 341:5@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Cal_table_table = Cal_table_table
  { raw :: RIP.CInt
    {- ^ __C declaration:__ @raw@

         __defined at:__ @comprehensive\/c2hsc.h 342:13@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  , val :: RIP.CInt
    {- ^ __C declaration:__ @val@

         __defined at:__ @comprehensive\/c2hsc.h 342:22@

         __exported by:__ @comprehensive\/c2hsc.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Cal_table_table where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Cal_table_table where

  readRaw =
    \ptr0 ->
          pure Cal_table_table
      <*> HasCField.readRaw (RIP.Proxy @"raw") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"val") ptr0

instance Marshal.WriteRaw Cal_table_table where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Cal_table_table raw2 val3 ->
               HasCField.writeRaw (RIP.Proxy @"raw") ptr0 raw2
            >> HasCField.writeRaw (RIP.Proxy @"val") ptr0 val3

deriving via Marshal.EquivStorable Cal_table_table instance RIP.Storable Cal_table_table

instance HasCField.HasCField Cal_table_table "raw" where

  type CFieldType Cal_table_table "raw" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "raw" (RIP.Ptr Cal_table_table) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"raw")

instance HasCField.HasCField Cal_table_table "val" where

  type CFieldType Cal_table_table "val" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "val" (RIP.Ptr Cal_table_table) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"val")

{-| Issues without test cases in the original test suite

    These are examples from open issues on the c2hsc repository, that don't (yet) have corresponding test cases in the c2hsc test suite.

    __C declaration:__ @struct cal_table@

    __defined at:__ @comprehensive\/c2hsc.h 339:8@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Cal_table = Cal_table
  { size :: RIP.CInt
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
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Cal_table where

  staticSizeOf = \_ -> (260 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Cal_table where

  readRaw =
    \ptr0 ->
          pure Cal_table
      <*> HasCField.readRaw (RIP.Proxy @"size") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"table") ptr0

instance Marshal.WriteRaw Cal_table where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Cal_table size2 table3 ->
               HasCField.writeRaw (RIP.Proxy @"size") ptr0 size2
            >> HasCField.writeRaw (RIP.Proxy @"table") ptr0 table3

deriving via Marshal.EquivStorable Cal_table instance RIP.Storable Cal_table

instance HasCField.HasCField Cal_table "size" where

  type CFieldType Cal_table "size" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "size" (RIP.Ptr Cal_table) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"size")

instance HasCField.HasCField Cal_table "table" where

  type CFieldType Cal_table "table" =
    CA.ConstantArray 32 Cal_table_table

  offset# = \_ -> \_ -> 4

instance ( ty ~ CA.ConstantArray 32 Cal_table_table
         ) => RIP.HasField "table" (RIP.Ptr Cal_table) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"table")

{-| __C declaration:__ @union \@Elf32_External_Dyn_d_un@

    __defined at:__ @comprehensive\/c2hsc.h 349:3@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype Elf32_External_Dyn_d_un = Elf32_External_Dyn_d_un
  { unwrap :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 1 instance Marshal.StaticSize Elf32_External_Dyn_d_un

deriving via RIP.SizedByteArray 4 1 instance Marshal.ReadRaw Elf32_External_Dyn_d_un

deriving via RIP.SizedByteArray 4 1 instance Marshal.WriteRaw Elf32_External_Dyn_d_un

deriving via Marshal.EquivStorable Elf32_External_Dyn_d_un instance RIP.Storable Elf32_External_Dyn_d_un

{-|

    __See:__ 'set_elf32_External_Dyn_d_un_d_val'

    __C declaration:__ @d_val@

    __defined at:__ @comprehensive\/c2hsc.h 350:19@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
get_elf32_External_Dyn_d_un_d_val ::
     Elf32_External_Dyn_d_un
  -> CA.ConstantArray 4 RIP.CUChar
get_elf32_External_Dyn_d_un_d_val =
  RIP.getUnionPayload

{-|

    __See:__ 'get_elf32_External_Dyn_d_un_d_val'

-}
set_elf32_External_Dyn_d_un_d_val ::
     CA.ConstantArray 4 RIP.CUChar
  -> Elf32_External_Dyn_d_un
set_elf32_External_Dyn_d_un_d_val =
  RIP.setUnionPayload

{-|

    __See:__ 'set_elf32_External_Dyn_d_un_d_ptr'

    __C declaration:__ @d_ptr@

    __defined at:__ @comprehensive\/c2hsc.h 351:19@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
get_elf32_External_Dyn_d_un_d_ptr ::
     Elf32_External_Dyn_d_un
  -> CA.ConstantArray 4 RIP.CUChar
get_elf32_External_Dyn_d_un_d_ptr =
  RIP.getUnionPayload

{-|

    __See:__ 'get_elf32_External_Dyn_d_un_d_ptr'

-}
set_elf32_External_Dyn_d_un_d_ptr ::
     CA.ConstantArray 4 RIP.CUChar
  -> Elf32_External_Dyn_d_un
set_elf32_External_Dyn_d_un_d_ptr =
  RIP.setUnionPayload

instance HasCField.HasCField Elf32_External_Dyn_d_un "d_val" where

  type CFieldType Elf32_External_Dyn_d_un "d_val" =
    CA.ConstantArray 4 RIP.CUChar

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 4 RIP.CUChar
         ) => RIP.HasField "d_val" (RIP.Ptr Elf32_External_Dyn_d_un) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"d_val")

instance HasCField.HasCField Elf32_External_Dyn_d_un "d_ptr" where

  type CFieldType Elf32_External_Dyn_d_un "d_ptr" =
    CA.ConstantArray 4 RIP.CUChar

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 4 RIP.CUChar
         ) => RIP.HasField "d_ptr" (RIP.Ptr Elf32_External_Dyn_d_un) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"d_ptr")

{-| __C declaration:__ @struct Elf32_External_Dyn@

    __defined at:__ @comprehensive\/c2hsc.h 347:9@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
data Elf32_External_Dyn = Elf32_External_Dyn
  { d_tag :: CA.ConstantArray 4 RIP.CUChar
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
  deriving stock (RIP.Generic)

instance Marshal.StaticSize Elf32_External_Dyn where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Elf32_External_Dyn where

  readRaw =
    \ptr0 ->
          pure Elf32_External_Dyn
      <*> HasCField.readRaw (RIP.Proxy @"d_tag") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"d_un") ptr0

instance Marshal.WriteRaw Elf32_External_Dyn where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Elf32_External_Dyn d_tag2 d_un3 ->
               HasCField.writeRaw (RIP.Proxy @"d_tag") ptr0 d_tag2
            >> HasCField.writeRaw (RIP.Proxy @"d_un") ptr0 d_un3

deriving via Marshal.EquivStorable Elf32_External_Dyn instance RIP.Storable Elf32_External_Dyn

instance HasCField.HasCField Elf32_External_Dyn "d_tag" where

  type CFieldType Elf32_External_Dyn "d_tag" =
    CA.ConstantArray 4 RIP.CUChar

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 4 RIP.CUChar
         ) => RIP.HasField "d_tag" (RIP.Ptr Elf32_External_Dyn) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"d_tag")

instance HasCField.HasCField Elf32_External_Dyn "d_un" where

  type CFieldType Elf32_External_Dyn "d_un" =
    Elf32_External_Dyn_d_un

  offset# = \_ -> \_ -> 4

instance ( ty ~ Elf32_External_Dyn_d_un
         ) => RIP.HasField "d_un" (RIP.Ptr Elf32_External_Dyn) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"d_un")

{-| __C declaration:__ @bug_24@

    __defined at:__ @comprehensive\/c2hsc.h 358:15@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype Bug_24 = Bug_24
  { unwrap :: RIP.Ptr RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ RIP.Ptr RIP.CInt
         ) => RIP.HasField "unwrap" (RIP.Ptr Bug_24) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField Bug_24 "unwrap" where

  type CFieldType Bug_24 "unwrap" = RIP.Ptr RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @bug_24_2@

    __defined at:__ @comprehensive\/c2hsc.h 359:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype Bug_24_2 = Bug_24_2
  { unwrap :: PtrConst.PtrConst RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ PtrConst.PtrConst RIP.CInt
         ) => RIP.HasField "unwrap" (RIP.Ptr Bug_24_2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField Bug_24_2 "unwrap" where

  type CFieldType Bug_24_2 "unwrap" =
    PtrConst.PtrConst RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MyArray_27@

    __defined at:__ @comprehensive\/c2hsc.h 364:13@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
newtype MyArray_27 = MyArray_27
  { unwrap :: CA.ConstantArray 20 RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 20 RIP.CInt
         ) => RIP.HasField "unwrap" (RIP.Ptr MyArray_27) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField MyArray_27 "unwrap" where

  type CFieldType MyArray_27 "unwrap" =
    CA.ConstantArray 20 RIP.CInt

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
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize MyStruct_27 where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw MyStruct_27 where

  readRaw =
    \ptr0 ->
          pure MyStruct_27
      <*> HasCField.readRaw (RIP.Proxy @"x") ptr0

instance Marshal.WriteRaw MyStruct_27 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyStruct_27 x2 ->
            HasCField.writeRaw (RIP.Proxy @"x") ptr0 x2

deriving via Marshal.EquivStorable MyStruct_27 instance RIP.Storable MyStruct_27

instance HasCField.HasCField MyStruct_27 "x" where

  type CFieldType MyStruct_27 "x" = MyArray_27

  offset# = \_ -> \_ -> 0

instance ( ty ~ MyArray_27
         ) => RIP.HasField "x" (RIP.Ptr MyStruct_27) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"x")
