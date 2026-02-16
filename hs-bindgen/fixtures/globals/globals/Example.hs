{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct config@

    __defined at:__ @globals\/globals.h 12:8@

    __exported by:__ @globals\/globals.h@
-}
data Config = Config
  { config_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals\/globals.h 13:7@

         __exported by:__ @globals\/globals.h@
    -}
  , config_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals\/globals.h 14:7@

         __exported by:__ @globals\/globals.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Config where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Config where

  readRaw =
    \ptr0 ->
          pure Config
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"config_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"config_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Config where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Config config_x2 config_y3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"config_x") ptr0 config_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"config_y") ptr0 config_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Config instance F.Storable Config

instance HsBindgen.Runtime.HasCField.HasCField Config "config_x" where

  type CFieldType Config "config_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "config_x" (Ptr.Ptr Config) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"config_x")

instance HsBindgen.Runtime.HasCField.HasCField Config "config_y" where

  type CFieldType Config "config_y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "config_y" (Ptr.Ptr Config) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"config_y")

{-| __C declaration:__ @struct inline_struct@

    __defined at:__ @globals\/globals.h 19:15@

    __exported by:__ @globals\/globals.h@
-}
data Inline_struct = Inline_struct
  { inline_struct_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals\/globals.h 19:35@

         __exported by:__ @globals\/globals.h@
    -}
  , inline_struct_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals\/globals.h 19:42@

         __exported by:__ @globals\/globals.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Inline_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Inline_struct where

  readRaw =
    \ptr0 ->
          pure Inline_struct
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"inline_struct_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"inline_struct_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Inline_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Inline_struct inline_struct_x2 inline_struct_y3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"inline_struct_x") ptr0 inline_struct_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"inline_struct_y") ptr0 inline_struct_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Inline_struct instance F.Storable Inline_struct

instance HsBindgen.Runtime.HasCField.HasCField Inline_struct "inline_struct_x" where

  type CFieldType Inline_struct "inline_struct_x" =
    FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "inline_struct_x" (Ptr.Ptr Inline_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"inline_struct_x")

instance HsBindgen.Runtime.HasCField.HasCField Inline_struct "inline_struct_y" where

  type CFieldType Inline_struct "inline_struct_y" =
    FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "inline_struct_y" (Ptr.Ptr Inline_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"inline_struct_y")

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
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Version_t where

  staticSizeOf = \_ -> (6 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Version_t where

  readRaw =
    \ptr0 ->
          pure Version_t
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"version_t_major") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"version_t_minor") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"version_t_patch") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Version_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Version_t version_t_major2 version_t_minor3 version_t_patch4 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"version_t_major") ptr0 version_t_major2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"version_t_minor") ptr0 version_t_minor3
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"version_t_patch") ptr0 version_t_patch4

deriving via HsBindgen.Runtime.Marshal.EquivStorable Version_t instance F.Storable Version_t

instance HsBindgen.Runtime.HasCField.HasCField Version_t "version_t_major" where

  type CFieldType Version_t "version_t_major" =
    HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 0

instance ( TyEq ty HsBindgen.Runtime.LibC.Word8
         ) => GHC.Records.HasField "version_t_major" (Ptr.Ptr Version_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"version_t_major")

instance HsBindgen.Runtime.HasCField.HasCField Version_t "version_t_minor" where

  type CFieldType Version_t "version_t_minor" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 2

instance ( TyEq ty HsBindgen.Runtime.LibC.Word16
         ) => GHC.Records.HasField "version_t_minor" (Ptr.Ptr Version_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"version_t_minor")

instance HsBindgen.Runtime.HasCField.HasCField Version_t "version_t_patch" where

  type CFieldType Version_t "version_t_patch" =
    HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 4

instance ( TyEq ty HsBindgen.Runtime.LibC.Word8
         ) => GHC.Records.HasField "version_t_patch" (Ptr.Ptr Version_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"version_t_patch")

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
  , struct1_t_y :: FC.CBool
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
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Struct1_t where

  staticSizeOf = \_ -> (10 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Struct1_t where

  readRaw =
    \ptr0 ->
          pure Struct1_t
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"struct1_t_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"struct1_t_y") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"struct1_t_version") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Struct1_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct1_t struct1_t_x2 struct1_t_y3 struct1_t_version4 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"struct1_t_x") ptr0 struct1_t_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"struct1_t_y") ptr0 struct1_t_y3
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"struct1_t_version") ptr0 struct1_t_version4

deriving via HsBindgen.Runtime.Marshal.EquivStorable Struct1_t instance F.Storable Struct1_t

instance HsBindgen.Runtime.HasCField.HasCField Struct1_t "struct1_t_x" where

  type CFieldType Struct1_t "struct1_t_x" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 0

instance ( TyEq ty HsBindgen.Runtime.LibC.Word16
         ) => GHC.Records.HasField "struct1_t_x" (Ptr.Ptr Struct1_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"struct1_t_x")

instance HsBindgen.Runtime.HasCField.HasCField Struct1_t "struct1_t_y" where

  type CFieldType Struct1_t "struct1_t_y" = FC.CBool

  offset# = \_ -> \_ -> 2

instance ( TyEq ty FC.CBool
         ) => GHC.Records.HasField "struct1_t_y" (Ptr.Ptr Struct1_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"struct1_t_y")

instance HsBindgen.Runtime.HasCField.HasCField Struct1_t "struct1_t_version" where

  type CFieldType Struct1_t "struct1_t_version" =
    Version_t

  offset# = \_ -> \_ -> 4

instance ( TyEq ty Version_t
         ) => GHC.Records.HasField "struct1_t_version" (Ptr.Ptr Struct1_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"struct1_t_version")

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
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Struct2_t where

  staticSizeOf = \_ -> (10 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Struct2_t where

  readRaw =
    \ptr0 ->
          pure Struct2_t
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"struct2_t_field1") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Struct2_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2_t struct2_t_field12 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"struct2_t_field1") ptr0 struct2_t_field12

deriving via HsBindgen.Runtime.Marshal.EquivStorable Struct2_t instance F.Storable Struct2_t

instance HsBindgen.Runtime.HasCField.HasCField Struct2_t "struct2_t_field1" where

  type CFieldType Struct2_t "struct2_t_field1" =
    Struct1_t

  offset# = \_ -> \_ -> 0

instance ( TyEq ty Struct1_t
         ) => GHC.Records.HasField "struct2_t_field1" (Ptr.Ptr Struct2_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"struct2_t_field1")
