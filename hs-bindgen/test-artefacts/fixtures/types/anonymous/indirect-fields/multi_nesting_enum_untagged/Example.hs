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
    ( Example.S_anon'anon'x_anon'x_x(..)
    , pattern Example.A
    , Example.S_anon'anon'x_anon'x(..)
    , Example.S_anon'anon'x(..)
    , Example.S(..)
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @enum \@S_anon\'anon\'x_anon\'x_x@

    __defined at:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h@
-}
newtype S_anon'anon'x_anon'x_x = S_anon'anon'x_anon'x_x
  { unwrapS_anon'anon'x_anon'x_x :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize S_anon'anon'x_anon'x_x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S_anon'anon'x_anon'x_x where

  readRaw =
    \ptr0 ->
          pure S_anon'anon'x_anon'x_x
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw S_anon'anon'x_anon'x_x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S_anon'anon'x_anon'x_x unwrapS_anon'anon'x_anon'x_x2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapS_anon'anon'x_anon'x_x2

deriving via Marshal.EquivStorable S_anon'anon'x_anon'x_x instance BG.Storable S_anon'anon'x_anon'x_x

deriving via BG.CUInt instance BG.Prim S_anon'anon'x_anon'x_x

instance CEnum.CEnum S_anon'anon'x_anon'x_x where

  type CEnumZ S_anon'anon'x_anon'x_x = BG.CUInt

  toCEnum = S_anon'anon'x_anon'x_x

  fromCEnum =
    BG.getField @"unwrapS_anon'anon'x_anon'x_x"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "A")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "S_anon'anon'x_anon'x_x"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "S_anon'anon'x_anon'x_x"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum S_anon'anon'x_anon'x_x where

  minDeclaredValue = A

  maxDeclaredValue = A

instance Show S_anon'anon'x_anon'x_x where

  showsPrec = CEnum.shows

instance Read S_anon'anon'x_anon'x_x where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapS_anon'anon'x_anon'x_x" S_anon'anon'x_anon'x_x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S_anon'anon'x_anon'x_x {unwrapS_anon'anon'x_anon'x_x = y1}
      , BG.getField @"unwrapS_anon'anon'x_anon'x_x" x0
      )

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapS_anon'anon'x_anon'x_x" (BG.Ptr S_anon'anon'x_anon'x_x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapS_anon'anon'x_anon'x_x")

instance HasCField.HasCField S_anon'anon'x_anon'x_x "unwrapS_anon'anon'x_anon'x_x" where

  type CFieldType S_anon'anon'x_anon'x_x "unwrapS_anon'anon'x_anon'x_x" =
    BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @a@

    __defined at:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h@
-}
pattern A :: S_anon'anon'x_anon'x_x
pattern A = S_anon'anon'x_anon'x_x 0

{-| __C declaration:__ @struct \@S_anon\'anon\'x_anon\'x@

    __defined at:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h@
-}
data S_anon'anon'x_anon'x = S_anon'anon'x_anon'x
  { s_anon'anon'x_anon'x_x :: S_anon'anon'x_anon'x_x
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h 3:1@

         __exported by:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S_anon'anon'x_anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S_anon'anon'x_anon'x where

  readRaw =
    \ptr0 ->
          pure S_anon'anon'x_anon'x
      <*> HasCField.readRaw (BG.Proxy @"s_anon'anon'x_anon'x_x") ptr0

instance Marshal.WriteRaw S_anon'anon'x_anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S_anon'anon'x_anon'x s_anon'anon'x_anon'x_x2 ->
            HasCField.writeRaw (BG.Proxy @"s_anon'anon'x_anon'x_x") ptr0 s_anon'anon'x_anon'x_x2

deriving via Marshal.EquivStorable S_anon'anon'x_anon'x instance BG.Storable S_anon'anon'x_anon'x

deriving via Struct.IsStructViaReadRaw S_anon'anon'x_anon'x instance Struct.IsStruct S_anon'anon'x_anon'x

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h@
-}
instance ( ty ~ S_anon'anon'x_anon'x_x
         ) => BG.CompatHasField.HasField "s_anon'anon'x_anon'x_x" S_anon'anon'x_anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S_anon'anon'x_anon'x {s_anon'anon'x_anon'x_x = y1}
      , BG.getField @"s_anon'anon'x_anon'x_x" x0
      )

instance ( ty ~ S_anon'anon'x_anon'x_x
         ) => BG.HasField "s_anon'anon'x_anon'x_x" (BG.Ptr S_anon'anon'x_anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"s_anon'anon'x_anon'x_x")

instance HasCField.HasCField S_anon'anon'x_anon'x "s_anon'anon'x_anon'x_x" where

  type CFieldType S_anon'anon'x_anon'x "s_anon'anon'x_anon'x_x" =
    S_anon'anon'x_anon'x_x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@S_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h@
-}
data S_anon'anon'x = S_anon'anon'x
  { s_anon'anon'x_anon'x :: S_anon'anon'x_anon'x
    {- ^ __C declaration:__ @anon\'x@

         __defined at:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h 3:1@

         __exported by:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S_anon'anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S_anon'anon'x where

  readRaw =
    \ptr0 ->
          pure S_anon'anon'x
      <*> HasCField.readRaw (BG.Proxy @"s_anon'anon'x_anon'x") ptr0

instance Marshal.WriteRaw S_anon'anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S_anon'anon'x s_anon'anon'x_anon'x2 ->
            HasCField.writeRaw (BG.Proxy @"s_anon'anon'x_anon'x") ptr0 s_anon'anon'x_anon'x2

deriving via Marshal.EquivStorable S_anon'anon'x instance BG.Storable S_anon'anon'x

deriving via Struct.IsStructViaReadRaw S_anon'anon'x instance Struct.IsStruct S_anon'anon'x

{-| __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h@
-}
instance ( ty ~ S_anon'anon'x_anon'x
         ) => BG.CompatHasField.HasField "s_anon'anon'x_anon'x" S_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 -> S_anon'anon'x {s_anon'anon'x_anon'x = y1}
      , BG.getField @"s_anon'anon'x_anon'x" x0
      )

instance ( ty ~ S_anon'anon'x_anon'x
         ) => BG.HasField "s_anon'anon'x_anon'x" (BG.Ptr S_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"s_anon'anon'x_anon'x")

instance HasCField.HasCField S_anon'anon'x "s_anon'anon'x_anon'x" where

  type CFieldType S_anon'anon'x "s_anon'anon'x_anon'x" =
    S_anon'anon'x_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h@
-}
instance ( ty ~ S_anon'anon'x_anon'x_x
         ) => BG.HasField "s_anon'anon'x_x" S_anon'anon'x ty where

  getField =
    \x0 ->
      BG.getField @"s_anon'anon'x_anon'x_x" (BG.getField @"s_anon'anon'x_anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h@
-}
instance ( ty ~ S_anon'anon'x_anon'x_x
         ) => BG.CompatHasField.HasField "s_anon'anon'x_x" S_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"s_anon'anon'x_anon'x" x0 (\z2 ->
                                                                      BG.CompatHasField.setField @"s_anon'anon'x_anon'x_x" z2 y1)
      , BG.getField @"s_anon'anon'x_x" x0
      )

instance ( ty ~ S_anon'anon'x_anon'x_x
         ) => BG.HasField "s_anon'anon'x_x" (BG.Ptr S_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"s_anon'anon'x_x")

instance HasCField.HasCField S_anon'anon'x "s_anon'anon'x_x" where

  type CFieldType S_anon'anon'x "s_anon'anon'x_x" =
    S_anon'anon'x_anon'x_x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct S@

    __defined at:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h@
-}
data S = S
  { s_anon'anon'x :: S_anon'anon'x
    {- ^ __C declaration:__ @anon\'anon\'x@

         __defined at:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h 3:1@

         __exported by:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HasCField.readRaw (BG.Proxy @"s_anon'anon'x") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_anon'anon'x2 ->
            HasCField.writeRaw (BG.Proxy @"s_anon'anon'x") ptr0 s_anon'anon'x2

deriving via Marshal.EquivStorable S instance BG.Storable S

deriving via Struct.IsStructViaReadRaw S instance Struct.IsStruct S

{-| __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h@
-}
instance ( ty ~ S_anon'anon'x
         ) => BG.CompatHasField.HasField "s_anon'anon'x" S ty where

  hasField =
    \x0 ->
      (\y1 ->
         S {s_anon'anon'x = y1}, BG.getField @"s_anon'anon'x" x0)

instance ( ty ~ S_anon'anon'x
         ) => BG.HasField "s_anon'anon'x" (BG.Ptr S) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"s_anon'anon'x")

instance HasCField.HasCField S "s_anon'anon'x" where

  type CFieldType S "s_anon'anon'x" = S_anon'anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h@
-}
instance (ty ~ S_anon'anon'x_anon'x_x) => BG.HasField "s_x" S ty where

  getField =
    \x0 ->
      BG.getField @"s_anon'anon'x_x" (BG.getField @"s_anon'anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/multi_nesting_enum_untagged.h@
-}
instance ( ty ~ S_anon'anon'x_anon'x_x
         ) => BG.CompatHasField.HasField "s_x" S ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"s_anon'anon'x" x0 (\z2 ->
                                                               BG.CompatHasField.setField @"s_anon'anon'x_x" z2 y1)
      , BG.getField @"s_x" x0
      )

instance ( ty ~ S_anon'anon'x_anon'x_x
         ) => BG.HasField "s_x" (BG.Ptr S) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s_x")

instance HasCField.HasCField S "s_x" where

  type CFieldType S "s_x" = S_anon'anon'x_anon'x_x

  offset# = \_ -> \_ -> 0
