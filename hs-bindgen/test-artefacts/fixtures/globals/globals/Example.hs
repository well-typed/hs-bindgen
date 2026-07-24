{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Config(..)
    , Example.Inline_struct(..)
    , Example.Version_t(..)
    , Example.Struct1_t(..)
    , Example.Struct2_t(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct config@

    __defined at:__ @globals\/globals.h 13:8@

    __exported by:__ @globals\/globals.h@
-}
data Config = Config
  { config_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals\/globals.h 14:7@

         __exported by:__ @globals\/globals.h@
    -}
  , config_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals\/globals.h 15:7@

         __exported by:__ @globals\/globals.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Config where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Config where

  readRaw =
    \ptr0 ->
          pure Config
      <*> HasCField.readRaw (BG.Proxy @"config_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"config_y") ptr0

instance Marshal.WriteRaw Config where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Config config_x2 config_y3 ->
               HasCField.writeRaw (BG.Proxy @"config_x") ptr0 config_x2
            >> HasCField.writeRaw (BG.Proxy @"config_y") ptr0 config_y3

deriving via Marshal.EquivStorable Config instance BG.Storable Config

deriving via Struct.IsStructViaStorable Config instance Struct.IsStruct Config

{-| __C declaration:__ @x@

    __defined at:__ @globals\/globals.h 14:7@

    __exported by:__ @globals\/globals.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "config_x" Config ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Config {config_x = y1, config_y = BG.getField @"config_y" x0}
      , BG.getField @"config_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "config_x" (BG.Ptr Config) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"config_x")

instance HasCField.HasCField Config "config_x" where

  type CFieldType Config "config_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @globals\/globals.h 15:7@

    __exported by:__ @globals\/globals.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "config_y" Config ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Config {config_y = y1, config_x = BG.getField @"config_x" x0}
      , BG.getField @"config_y" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "config_y" (BG.Ptr Config) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"config_y")

instance HasCField.HasCField Config "config_y" where

  type CFieldType Config "config_y" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct inline_struct@

    __defined at:__ @globals\/globals.h 20:15@

    __exported by:__ @globals\/globals.h@
-}
data Inline_struct = Inline_struct
  { inline_struct_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals\/globals.h 20:35@

         __exported by:__ @globals\/globals.h@
    -}
  , inline_struct_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals\/globals.h 20:42@

         __exported by:__ @globals\/globals.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Inline_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Inline_struct where

  readRaw =
    \ptr0 ->
          pure Inline_struct
      <*> HasCField.readRaw (BG.Proxy @"inline_struct_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"inline_struct_y") ptr0

instance Marshal.WriteRaw Inline_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Inline_struct inline_struct_x2 inline_struct_y3 ->
               HasCField.writeRaw (BG.Proxy @"inline_struct_x") ptr0 inline_struct_x2
            >> HasCField.writeRaw (BG.Proxy @"inline_struct_y") ptr0 inline_struct_y3

deriving via Marshal.EquivStorable Inline_struct instance BG.Storable Inline_struct

deriving via Struct.IsStructViaStorable Inline_struct instance Struct.IsStruct Inline_struct

{-| __C declaration:__ @x@

    __defined at:__ @globals\/globals.h 20:35@

    __exported by:__ @globals\/globals.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "inline_struct_x" Inline_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Inline_struct {inline_struct_x = y1, inline_struct_y = BG.getField @"inline_struct_y" x0}
      , BG.getField @"inline_struct_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "inline_struct_x" (BG.Ptr Inline_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"inline_struct_x")

instance HasCField.HasCField Inline_struct "inline_struct_x" where

  type CFieldType Inline_struct "inline_struct_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @globals\/globals.h 20:42@

    __exported by:__ @globals\/globals.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "inline_struct_y" Inline_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Inline_struct {inline_struct_y = y1, inline_struct_x = BG.getField @"inline_struct_x" x0}
      , BG.getField @"inline_struct_y" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "inline_struct_y" (BG.Ptr Inline_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"inline_struct_y")

instance HasCField.HasCField Inline_struct "inline_struct_y" where

  type CFieldType Inline_struct "inline_struct_y" =
    BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct version_t@

    __defined at:__ @globals\/globals.h 407:9@

    __exported by:__ @globals\/globals.h@
-}
data Version_t = Version_t
  { version_t_major :: HsBindgen.Runtime.LibC.Word8
    {- ^ __C declaration:__ @major@

         __defined at:__ @globals\/globals.h 409:12@

         __exported by:__ @globals\/globals.h@
    -}
  , version_t_minor :: HsBindgen.Runtime.LibC.Word16
    {- ^ __C declaration:__ @minor@

         __defined at:__ @globals\/globals.h 410:12@

         __exported by:__ @globals\/globals.h@
    -}
  , version_t_patch :: HsBindgen.Runtime.LibC.Word8
    {- ^ __C declaration:__ @patch@

         __defined at:__ @globals\/globals.h 411:12@

         __exported by:__ @globals\/globals.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Version_t where

  staticSizeOf = \_ -> (6 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Version_t where

  readRaw =
    \ptr0 ->
          pure Version_t
      <*> HasCField.readRaw (BG.Proxy @"version_t_major") ptr0
      <*> HasCField.readRaw (BG.Proxy @"version_t_minor") ptr0
      <*> HasCField.readRaw (BG.Proxy @"version_t_patch") ptr0

instance Marshal.WriteRaw Version_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Version_t version_t_major2 version_t_minor3 version_t_patch4 ->
               HasCField.writeRaw (BG.Proxy @"version_t_major") ptr0 version_t_major2
            >> HasCField.writeRaw (BG.Proxy @"version_t_minor") ptr0 version_t_minor3
            >> HasCField.writeRaw (BG.Proxy @"version_t_patch") ptr0 version_t_patch4

deriving via Marshal.EquivStorable Version_t instance BG.Storable Version_t

deriving via Struct.IsStructViaStorable Version_t instance Struct.IsStruct Version_t

{-| __C declaration:__ @major@

    __defined at:__ @globals\/globals.h 409:12@

    __exported by:__ @globals\/globals.h@
-}
instance ( ty ~ HsBindgen.Runtime.LibC.Word8
         ) => BG.CompatHasField.HasField "version_t_major" Version_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Version_t { version_t_major = y1
                    , version_t_minor = BG.getField @"version_t_minor" x0
                    , version_t_patch = BG.getField @"version_t_patch" x0
                    }
      , BG.getField @"version_t_major" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word8
         ) => BG.HasField "version_t_major" (BG.Ptr Version_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"version_t_major")

instance HasCField.HasCField Version_t "version_t_major" where

  type CFieldType Version_t "version_t_major" =
    HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @minor@

    __defined at:__ @globals\/globals.h 410:12@

    __exported by:__ @globals\/globals.h@
-}
instance ( ty ~ HsBindgen.Runtime.LibC.Word16
         ) => BG.CompatHasField.HasField "version_t_minor" Version_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Version_t { version_t_minor = y1
                    , version_t_major = BG.getField @"version_t_major" x0
                    , version_t_patch = BG.getField @"version_t_patch" x0
                    }
      , BG.getField @"version_t_minor" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word16
         ) => BG.HasField "version_t_minor" (BG.Ptr Version_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"version_t_minor")

instance HasCField.HasCField Version_t "version_t_minor" where

  type CFieldType Version_t "version_t_minor" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 2

{-| __C declaration:__ @patch@

    __defined at:__ @globals\/globals.h 411:12@

    __exported by:__ @globals\/globals.h@
-}
instance ( ty ~ HsBindgen.Runtime.LibC.Word8
         ) => BG.CompatHasField.HasField "version_t_patch" Version_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Version_t { version_t_patch = y1
                    , version_t_major = BG.getField @"version_t_major" x0
                    , version_t_minor = BG.getField @"version_t_minor" x0
                    }
      , BG.getField @"version_t_patch" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word8
         ) => BG.HasField "version_t_patch" (BG.Ptr Version_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"version_t_patch")

instance HasCField.HasCField Version_t "version_t_patch" where

  type CFieldType Version_t "version_t_patch" =
    HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct struct1_t@

    __defined at:__ @globals\/globals.h 414:9@

    __exported by:__ @globals\/globals.h@
-}
data Struct1_t = Struct1_t
  { struct1_t_x :: HsBindgen.Runtime.LibC.Word16
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals\/globals.h 416:13@

         __exported by:__ @globals\/globals.h@
    -}
  , struct1_t_y :: BG.CBool
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals\/globals.h 417:13@

         __exported by:__ @globals\/globals.h@
    -}
  , struct1_t_version :: Version_t
    {- ^ __C declaration:__ @version@

         __defined at:__ @globals\/globals.h 418:13@

         __exported by:__ @globals\/globals.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Struct1_t where

  staticSizeOf = \_ -> (10 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Struct1_t where

  readRaw =
    \ptr0 ->
          pure Struct1_t
      <*> HasCField.readRaw (BG.Proxy @"struct1_t_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"struct1_t_y") ptr0
      <*> HasCField.readRaw (BG.Proxy @"struct1_t_version") ptr0

instance Marshal.WriteRaw Struct1_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct1_t struct1_t_x2 struct1_t_y3 struct1_t_version4 ->
               HasCField.writeRaw (BG.Proxy @"struct1_t_x") ptr0 struct1_t_x2
            >> HasCField.writeRaw (BG.Proxy @"struct1_t_y") ptr0 struct1_t_y3
            >> HasCField.writeRaw (BG.Proxy @"struct1_t_version") ptr0 struct1_t_version4

deriving via Marshal.EquivStorable Struct1_t instance BG.Storable Struct1_t

deriving via Struct.IsStructViaStorable Struct1_t instance Struct.IsStruct Struct1_t

{-| __C declaration:__ @x@

    __defined at:__ @globals\/globals.h 416:13@

    __exported by:__ @globals\/globals.h@
-}
instance ( ty ~ HsBindgen.Runtime.LibC.Word16
         ) => BG.CompatHasField.HasField "struct1_t_x" Struct1_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Struct1_t { struct1_t_x = y1
                    , struct1_t_y = BG.getField @"struct1_t_y" x0
                    , struct1_t_version = BG.getField @"struct1_t_version" x0
                    }
      , BG.getField @"struct1_t_x" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word16
         ) => BG.HasField "struct1_t_x" (BG.Ptr Struct1_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"struct1_t_x")

instance HasCField.HasCField Struct1_t "struct1_t_x" where

  type CFieldType Struct1_t "struct1_t_x" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @globals\/globals.h 417:13@

    __exported by:__ @globals\/globals.h@
-}
instance ( ty ~ BG.CBool
         ) => BG.CompatHasField.HasField "struct1_t_y" Struct1_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Struct1_t { struct1_t_y = y1
                    , struct1_t_x = BG.getField @"struct1_t_x" x0
                    , struct1_t_version = BG.getField @"struct1_t_version" x0
                    }
      , BG.getField @"struct1_t_y" x0
      )

instance ( ty ~ BG.CBool
         ) => BG.HasField "struct1_t_y" (BG.Ptr Struct1_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"struct1_t_y")

instance HasCField.HasCField Struct1_t "struct1_t_y" where

  type CFieldType Struct1_t "struct1_t_y" = BG.CBool

  offset# = \_ -> \_ -> 2

{-| __C declaration:__ @version@

    __defined at:__ @globals\/globals.h 418:13@

    __exported by:__ @globals\/globals.h@
-}
instance ( ty ~ Version_t
         ) => BG.CompatHasField.HasField "struct1_t_version" Struct1_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Struct1_t { struct1_t_version = y1
                    , struct1_t_x = BG.getField @"struct1_t_x" x0
                    , struct1_t_y = BG.getField @"struct1_t_y" x0
                    }
      , BG.getField @"struct1_t_version" x0
      )

instance ( ty ~ Version_t
         ) => BG.HasField "struct1_t_version" (BG.Ptr Struct1_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"struct1_t_version")

instance HasCField.HasCField Struct1_t "struct1_t_version" where

  type CFieldType Struct1_t "struct1_t_version" =
    Version_t

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct struct2_t@

    __defined at:__ @globals\/globals.h 421:9@

    __exported by:__ @globals\/globals.h@
-}
data Struct2_t = Struct2_t
  { struct2_t_field1 :: Struct1_t
    {- ^ __C declaration:__ @field1@

         __defined at:__ @globals\/globals.h 423:13@

         __exported by:__ @globals\/globals.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Struct2_t where

  staticSizeOf = \_ -> (10 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Struct2_t where

  readRaw =
    \ptr0 ->
          pure Struct2_t
      <*> HasCField.readRaw (BG.Proxy @"struct2_t_field1") ptr0

instance Marshal.WriteRaw Struct2_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2_t struct2_t_field12 ->
            HasCField.writeRaw (BG.Proxy @"struct2_t_field1") ptr0 struct2_t_field12

deriving via Marshal.EquivStorable Struct2_t instance BG.Storable Struct2_t

deriving via Struct.IsStructViaStorable Struct2_t instance Struct.IsStruct Struct2_t

{-| __C declaration:__ @field1@

    __defined at:__ @globals\/globals.h 423:13@

    __exported by:__ @globals\/globals.h@
-}
instance ( ty ~ Struct1_t
         ) => BG.CompatHasField.HasField "struct2_t_field1" Struct2_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Struct2_t {struct2_t_field1 = y1}
      , BG.getField @"struct2_t_field1" x0
      )

instance ( ty ~ Struct1_t
         ) => BG.HasField "struct2_t_field1" (BG.Ptr Struct2_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"struct2_t_field1")

instance HasCField.HasCField Struct2_t "struct2_t_field1" where

  type CFieldType Struct2_t "struct2_t_field1" =
    Struct1_t

  offset# = \_ -> \_ -> 0
