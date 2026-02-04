{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal
import GHC.Exts ((*#), (+#))
import HsBindgen.Runtime.TypeEquality (TyEq)
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
  deriving stock (Eq, Show)

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

instance Data.Primitive.Types.Prim Config where

  sizeOf# = \_ -> (8#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Config (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Config v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Config config_x4 config_y5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) config_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) config_y5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Config (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Config v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Config config_x4 config_y5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) config_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) config_y5 s6

instance HsBindgen.Runtime.HasCField.HasCField Config "config_x" where

  type CFieldType Config "config_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Config) "config_x")
         ) => GHC.Records.HasField "config_x" (Ptr.Ptr Config) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"config_x")

instance HsBindgen.Runtime.HasCField.HasCField Config "config_y" where

  type CFieldType Config "config_y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Config) "config_y")
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
  deriving stock (Eq, Show)

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

instance Data.Primitive.Types.Prim Inline_struct where

  sizeOf# = \_ -> (8#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Inline_struct (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Inline_struct v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Inline_struct inline_struct_x4 inline_struct_y5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) inline_struct_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) inline_struct_y5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Inline_struct (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Inline_struct v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Inline_struct inline_struct_x4 inline_struct_y5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) inline_struct_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) inline_struct_y5 s6

instance HsBindgen.Runtime.HasCField.HasCField Inline_struct "inline_struct_x" where

  type CFieldType Inline_struct "inline_struct_x" =
    FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Inline_struct) "inline_struct_x")
         ) => GHC.Records.HasField "inline_struct_x" (Ptr.Ptr Inline_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"inline_struct_x")

instance HsBindgen.Runtime.HasCField.HasCField Inline_struct "inline_struct_y" where

  type CFieldType Inline_struct "inline_struct_y" =
    FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Inline_struct) "inline_struct_y")
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
  deriving stock (Eq, Show)

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

instance Data.Primitive.Types.Prim Version_t where

  sizeOf# = \_ -> (6#)

  alignment# = \_ -> (2#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Version_t (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (3#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (3#) i1) (1#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (3#) i1) (2#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (3#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (3#) i1) (1#)) s3 of
                (# s5, v6 #) ->
                  case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (3#) i1) (2#)) s5 of
                    (# s7, v8 #) -> (# s7, Version_t v4 v6 v8 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Version_t version_t_major4 version_t_minor5 version_t_patch6 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (3#) i1) (0#)) version_t_major4 s3 of
                  s7 ->
                    case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (3#) i1) (1#)) version_t_minor5 s7 of
                      s8 ->
                        Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (3#) i1) (2#)) version_t_patch6 s8

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Version_t (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (3#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (3#) i1) (1#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (3#) i1) (2#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (3#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (3#) i1) (1#)) s3 of
                (# s5, v6 #) ->
                  case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (3#) i1) (2#)) s5 of
                    (# s7, v8 #) -> (# s7, Version_t v4 v6 v8 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Version_t version_t_major4 version_t_minor5 version_t_patch6 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (3#) i1) (0#)) version_t_major4 s3 of
                  s7 ->
                    case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (3#) i1) (1#)) version_t_minor5 s7 of
                      s8 ->
                        Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (3#) i1) (2#)) version_t_patch6 s8

instance HsBindgen.Runtime.HasCField.HasCField Version_t "version_t_major" where

  type CFieldType Version_t "version_t_major" =
    HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Version_t) "version_t_major")
         ) => GHC.Records.HasField "version_t_major" (Ptr.Ptr Version_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"version_t_major")

instance HsBindgen.Runtime.HasCField.HasCField Version_t "version_t_minor" where

  type CFieldType Version_t "version_t_minor" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 2

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Version_t) "version_t_minor")
         ) => GHC.Records.HasField "version_t_minor" (Ptr.Ptr Version_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"version_t_minor")

instance HsBindgen.Runtime.HasCField.HasCField Version_t "version_t_patch" where

  type CFieldType Version_t "version_t_patch" =
    HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Version_t) "version_t_patch")
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
  deriving stock (Eq, Show)

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

instance Data.Primitive.Types.Prim Struct1_t where

  sizeOf# = \_ -> (10#)

  alignment# = \_ -> (2#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Struct1_t (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (3#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (3#) i1) (1#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (3#) i1) (2#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (3#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (3#) i1) (1#)) s3 of
                (# s5, v6 #) ->
                  case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (3#) i1) (2#)) s5 of
                    (# s7, v8 #) -> (# s7, Struct1_t v4 v6 v8 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct1_t struct1_t_x4 struct1_t_y5 struct1_t_version6 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (3#) i1) (0#)) struct1_t_x4 s3 of
                  s7 ->
                    case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (3#) i1) (1#)) struct1_t_y5 s7 of
                      s8 ->
                        Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (3#) i1) (2#)) struct1_t_version6 s8

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Struct1_t (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (3#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (3#) i1) (1#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (3#) i1) (2#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (3#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (3#) i1) (1#)) s3 of
                (# s5, v6 #) ->
                  case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (3#) i1) (2#)) s5 of
                    (# s7, v8 #) -> (# s7, Struct1_t v4 v6 v8 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct1_t struct1_t_x4 struct1_t_y5 struct1_t_version6 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (3#) i1) (0#)) struct1_t_x4 s3 of
                  s7 ->
                    case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (3#) i1) (1#)) struct1_t_y5 s7 of
                      s8 ->
                        Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (3#) i1) (2#)) struct1_t_version6 s8

instance HsBindgen.Runtime.HasCField.HasCField Struct1_t "struct1_t_x" where

  type CFieldType Struct1_t "struct1_t_x" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct1_t) "struct1_t_x")
         ) => GHC.Records.HasField "struct1_t_x" (Ptr.Ptr Struct1_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"struct1_t_x")

instance HsBindgen.Runtime.HasCField.HasCField Struct1_t "struct1_t_y" where

  type CFieldType Struct1_t "struct1_t_y" = FC.CBool

  offset# = \_ -> \_ -> 2

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct1_t) "struct1_t_y")
         ) => GHC.Records.HasField "struct1_t_y" (Ptr.Ptr Struct1_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"struct1_t_y")

instance HsBindgen.Runtime.HasCField.HasCField Struct1_t "struct1_t_version" where

  type CFieldType Struct1_t "struct1_t_version" =
    Version_t

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct1_t) "struct1_t_version")
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
  deriving stock (Eq, Show)

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

instance Data.Primitive.Types.Prim Struct2_t where

  sizeOf# = \_ -> (10#)

  alignment# = \_ -> (2#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Struct2_t (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Struct2_t v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct2_t struct2_t_field14 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 struct2_t_field14 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Struct2_t (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Struct2_t v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct2_t struct2_t_field14 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 struct2_t_field14 s3

instance HsBindgen.Runtime.HasCField.HasCField Struct2_t "struct2_t_field1" where

  type CFieldType Struct2_t "struct2_t_field1" =
    Struct1_t

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct2_t) "struct2_t_field1")
         ) => GHC.Records.HasField "struct2_t_field1" (Ptr.Ptr Struct2_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"struct2_t_field1")
