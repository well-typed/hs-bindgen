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
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module BootFiles.A
    ( BootFiles.A.a_VALUE
    , BootFiles.A.a_FUNCTION
    , BootFiles.A.A_enum(..)
    , pattern BootFiles.A.A_ONE
    , pattern BootFiles.A.A_TWO
    , BootFiles.A.A(..)
    )
  where

import qualified C.Expr.HostPlatform
import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified BootFiles.B
import Prelude ((<*>), (>>), Eq, Int, Ord, Read, Show, pure, type (~))

{-| __C declaration:__ @macro A_VALUE@

    __defined at:__ @a.h 7:9@

    __exported by:__ @boot_files.h@
-}
a_VALUE :: BG.CInt
a_VALUE = (1024 :: BG.CInt)

{-| __C declaration:__ @macro A_FUNCTION@

    __defined at:__ @a.h 8:9@

    __exported by:__ @boot_files.h@
-}
a_FUNCTION :: forall a0. C.Expr.HostPlatform.Mult a0 BG.CInt => a0 -> C.Expr.HostPlatform.MultRes a0 BG.CInt
a_FUNCTION =
  \n0 -> (C.Expr.HostPlatform.*) n0 (2 :: BG.CInt)

{-| __C declaration:__ @enum a_enum@

    __defined at:__ @a.h 10:6@

    __exported by:__ @boot_files.h@
-}
newtype A_enum = A_enum
  { unwrapA_enum :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize A_enum where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw A_enum where

  readRaw =
    \ptr0 ->
          pure A_enum
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw A_enum where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A_enum unwrapA_enum2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapA_enum2

deriving via Marshal.EquivStorable A_enum instance BG.Storable A_enum

deriving via BG.CUInt instance BG.Prim A_enum

instance CEnum.CEnum A_enum where

  type CEnumZ A_enum = BG.CUInt

  toCEnum = A_enum

  fromCEnum = BG.getField @"unwrapA_enum"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "A_ONE"), (1, BG.singleton "A_TWO")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "A_enum"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "A_enum"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum A_enum where

  minDeclaredValue = A_ONE

  maxDeclaredValue = A_TWO

instance Show A_enum where

  showsPrec = CEnum.shows

instance Read A_enum where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapA_enum" A_enum ty where

  hasField =
    \x0 ->
      (\y1 ->
         A_enum {unwrapA_enum = y1}, BG.getField @"unwrapA_enum" x0)

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapA_enum" (BG.Ptr A_enum) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapA_enum")

instance HasCField.HasCField A_enum "unwrapA_enum" where

  type CFieldType A_enum "unwrapA_enum" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A_ONE@

    __defined at:__ @a.h 10:15@

    __exported by:__ @boot_files.h@
-}
pattern A_ONE :: A_enum
pattern A_ONE = A_enum 0

{-| __C declaration:__ @A_TWO@

    __defined at:__ @a.h 10:22@

    __exported by:__ @boot_files.h@
-}
pattern A_TWO :: A_enum
pattern A_TWO = A_enum 1

{-| __C declaration:__ @struct a@

    __defined at:__ @a.h 14:8@

    __exported by:__ @boot_files.h@
-}
data A = A
  { a_b :: BG.Ptr BootFiles.B.B
    {- ^ __C declaration:__ @b@

         __defined at:__ @a.h 15:13@

         __exported by:__ @boot_files.h@
    -}
  , a_e :: A_enum
    {- ^ __C declaration:__ @e@

         __defined at:__ @a.h 16:15@

         __exported by:__ @boot_files.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize A where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw A where

  readRaw =
    \ptr0 ->
          pure A
      <*> HasCField.readRaw (BG.Proxy @"a_b") ptr0
      <*> HasCField.readRaw (BG.Proxy @"a_e") ptr0

instance Marshal.WriteRaw A where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A a_b2 a_e3 ->
               HasCField.writeRaw (BG.Proxy @"a_b") ptr0 a_b2
            >> HasCField.writeRaw (BG.Proxy @"a_e") ptr0 a_e3

deriving via Marshal.EquivStorable A instance BG.Storable A

deriving via Struct.IsStructViaReadRaw A instance Struct.IsStruct A

{-| __C declaration:__ @b@

    __defined at:__ @a.h 15:13@

    __exported by:__ @boot_files.h@
-}
instance ( ty ~ BG.Ptr BootFiles.B.B
         ) => BG.CompatHasField.HasField "a_b" A ty where

  hasField =
    \x0 ->
      (\y1 ->
         A {a_b = y1, a_e = BG.getField @"a_e" x0}, BG.getField @"a_b" x0)

instance ( ty ~ BG.Ptr BootFiles.B.B
         ) => BG.HasField "a_b" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a_b")

instance HasCField.HasCField A "a_b" where

  type CFieldType A "a_b" = BG.Ptr BootFiles.B.B

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @e@

    __defined at:__ @a.h 16:15@

    __exported by:__ @boot_files.h@
-}
instance (ty ~ A_enum) => BG.CompatHasField.HasField "a_e" A ty where

  hasField =
    \x0 ->
      (\y1 ->
         A {a_e = y1, a_b = BG.getField @"a_b" x0}, BG.getField @"a_e" x0)

instance (ty ~ A_enum) => BG.HasField "a_e" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a_e")

instance HasCField.HasCField A "a_e" where

  type CFieldType A "a_e" = A_enum

  offset# = \_ -> \_ -> 8
