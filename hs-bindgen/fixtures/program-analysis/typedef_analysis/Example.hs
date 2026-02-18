{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| Examples for the various cases in by `HsBindgen.Frontend.Analysis.Typedefs`

__C declaration:__ @struct struct1@

__defined at:__ @program-analysis\/typedef_analysis.h 7:8@

__exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct1_t = Struct1_t
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct1_t where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Struct1_t where

  readRaw = \ptr0 -> pure Struct1_t

instance Marshal.WriteRaw Struct1_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct1_t -> return ()

deriving via Marshal.EquivStorable Struct1_t instance RIP.Storable Struct1_t

{-| __C declaration:__ @struct struct2@

    __defined at:__ @program-analysis\/typedef_analysis.h 11:16@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct2_t = Struct2_t
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct2_t where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Struct2_t where

  readRaw = \ptr0 -> pure Struct2_t

instance Marshal.WriteRaw Struct2_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2_t -> return ()

deriving via Marshal.EquivStorable Struct2_t instance RIP.Storable Struct2_t

{-| __C declaration:__ @struct struct3@

    __defined at:__ @program-analysis\/typedef_analysis.h 14:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct3_t

{-| __C declaration:__ @struct struct4@

    __defined at:__ @program-analysis\/typedef_analysis.h 18:16@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct4_t

{-| __C declaration:__ @struct struct5@

    __defined at:__ @program-analysis\/typedef_analysis.h 21:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct5 = Struct5
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct5 where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Struct5 where

  readRaw = \ptr0 -> pure Struct5

instance Marshal.WriteRaw Struct5 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct5 -> return ()

deriving via Marshal.EquivStorable Struct5 instance RIP.Storable Struct5

{-| __C declaration:__ @struct5_t@

    __defined at:__ @program-analysis\/typedef_analysis.h 22:25@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Struct5_t = Struct5_t
  { unwrapStruct5_t :: RIP.Ptr Struct5
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr Struct5)
         ) => RIP.HasField "unwrapStruct5_t" (RIP.Ptr Struct5_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStruct5_t")

instance HasCField.HasCField Struct5_t "unwrapStruct5_t" where

  type CFieldType Struct5_t "unwrapStruct5_t" =
    RIP.Ptr Struct5

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct6@

    __defined at:__ @program-analysis\/typedef_analysis.h 25:16@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct6_Aux = Struct6_Aux
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct6_Aux where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Struct6_Aux where

  readRaw = \ptr0 -> pure Struct6_Aux

instance Marshal.WriteRaw Struct6_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct6_Aux -> return ()

deriving via Marshal.EquivStorable Struct6_Aux instance RIP.Storable Struct6_Aux

{-| __C declaration:__ @struct6@

    __defined at:__ @program-analysis\/typedef_analysis.h 25:28@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Struct6 = Struct6
  { unwrapStruct6 :: RIP.Ptr Struct6_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr Struct6_Aux)
         ) => RIP.HasField "unwrapStruct6" (RIP.Ptr Struct6) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStruct6")

instance HasCField.HasCField Struct6 "unwrapStruct6" where

  type CFieldType Struct6 "unwrapStruct6" =
    RIP.Ptr Struct6_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct7@

    __defined at:__ @program-analysis\/typedef_analysis.h 28:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct7 = Struct7
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct7 where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Struct7 where

  readRaw = \ptr0 -> pure Struct7

instance Marshal.WriteRaw Struct7 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct7 -> return ()

deriving via Marshal.EquivStorable Struct7 instance RIP.Storable Struct7

{-| __C declaration:__ @struct7a@

    __defined at:__ @program-analysis\/typedef_analysis.h 29:24@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Struct7a = Struct7a
  { unwrapStruct7a :: Struct7
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) Struct7
         ) => RIP.HasField "unwrapStruct7a" (RIP.Ptr Struct7a) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStruct7a")

instance HasCField.HasCField Struct7a "unwrapStruct7a" where

  type CFieldType Struct7a "unwrapStruct7a" = Struct7

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct7b@

    __defined at:__ @program-analysis\/typedef_analysis.h 30:24@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Struct7b = Struct7b
  { unwrapStruct7b :: Struct7
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) Struct7
         ) => RIP.HasField "unwrapStruct7b" (RIP.Ptr Struct7b) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStruct7b")

instance HasCField.HasCField Struct7b "unwrapStruct7b" where

  type CFieldType Struct7b "unwrapStruct7b" = Struct7

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct8@

    __defined at:__ @program-analysis\/typedef_analysis.h 33:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct8 = Struct8
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct8 where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Struct8 where

  readRaw = \ptr0 -> pure Struct8

instance Marshal.WriteRaw Struct8 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct8 -> return ()

deriving via Marshal.EquivStorable Struct8 instance RIP.Storable Struct8

{-| __C declaration:__ @struct8b@

    __defined at:__ @program-analysis\/typedef_analysis.h 35:24@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Struct8b = Struct8b
  { unwrapStruct8b :: Struct8
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) Struct8
         ) => RIP.HasField "unwrapStruct8b" (RIP.Ptr Struct8b) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStruct8b")

instance HasCField.HasCField Struct8b "unwrapStruct8b" where

  type CFieldType Struct8b "unwrapStruct8b" = Struct8

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct9@

    __defined at:__ @program-analysis\/typedef_analysis.h 38:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct9 = Struct9
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct9 where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Struct9 where

  readRaw = \ptr0 -> pure Struct9

instance Marshal.WriteRaw Struct9 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct9 -> return ()

deriving via Marshal.EquivStorable Struct9 instance RIP.Storable Struct9

{-| __C declaration:__ @struct9_t@

    __defined at:__ @program-analysis\/typedef_analysis.h 40:17@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Struct9_t = Struct9_t
  { unwrapStruct9_t :: Struct9
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) Struct9
         ) => RIP.HasField "unwrapStruct9_t" (RIP.Ptr Struct9_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStruct9_t")

instance HasCField.HasCField Struct9_t "unwrapStruct9_t" where

  type CFieldType Struct9_t "unwrapStruct9_t" = Struct9

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct10@

    __defined at:__ @program-analysis\/typedef_analysis.h 46:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct10_t = Struct10_t
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct10_t where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Struct10_t where

  readRaw = \ptr0 -> pure Struct10_t

instance Marshal.WriteRaw Struct10_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct10_t -> return ()

deriving via Marshal.EquivStorable Struct10_t instance RIP.Storable Struct10_t

{-| __C declaration:__ @struct10_t_t@

    __defined at:__ @program-analysis\/typedef_analysis.h 48:20@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Struct10_t_t = Struct10_t_t
  { unwrapStruct10_t_t :: Struct10_t
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) Struct10_t
         ) => RIP.HasField "unwrapStruct10_t_t" (RIP.Ptr Struct10_t_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStruct10_t_t")

instance HasCField.HasCField Struct10_t_t "unwrapStruct10_t_t" where

  type CFieldType Struct10_t_t "unwrapStruct10_t_t" =
    Struct10_t

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct11@

    __defined at:__ @program-analysis\/typedef_analysis.h 51:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct11_t = Struct11_t
  { struct11_t_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/typedef_analysis.h 52:7@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , struct11_t_self :: RIP.Ptr Struct11_t
    {- ^ __C declaration:__ @self@

         __defined at:__ @program-analysis\/typedef_analysis.h 53:20@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct11_t where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Struct11_t where

  readRaw =
    \ptr0 ->
          pure Struct11_t
      <*> HasCField.readRaw (RIP.Proxy @"struct11_t_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"struct11_t_self") ptr0

instance Marshal.WriteRaw Struct11_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct11_t struct11_t_x2 struct11_t_self3 ->
               HasCField.writeRaw (RIP.Proxy @"struct11_t_x") ptr0 struct11_t_x2
            >> HasCField.writeRaw (RIP.Proxy @"struct11_t_self") ptr0 struct11_t_self3

deriving via Marshal.EquivStorable Struct11_t instance RIP.Storable Struct11_t

instance HasCField.HasCField Struct11_t "struct11_t_x" where

  type CFieldType Struct11_t "struct11_t_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "struct11_t_x" (RIP.Ptr Struct11_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"struct11_t_x")

instance HasCField.HasCField Struct11_t "struct11_t_self" where

  type CFieldType Struct11_t "struct11_t_self" =
    RIP.Ptr Struct11_t

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) (RIP.Ptr Struct11_t)
         ) => RIP.HasField "struct11_t_self" (RIP.Ptr Struct11_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"struct11_t_self")

{-| __C declaration:__ @struct struct12@

    __defined at:__ @program-analysis\/typedef_analysis.h 60:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct12_t = Struct12_t
  { struct12_t_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/typedef_analysis.h 61:7@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , struct12_t_self :: RIP.Ptr Struct12_t
    {- ^ __C declaration:__ @self@

         __defined at:__ @program-analysis\/typedef_analysis.h 62:15@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct12_t where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Struct12_t where

  readRaw =
    \ptr0 ->
          pure Struct12_t
      <*> HasCField.readRaw (RIP.Proxy @"struct12_t_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"struct12_t_self") ptr0

instance Marshal.WriteRaw Struct12_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct12_t struct12_t_x2 struct12_t_self3 ->
               HasCField.writeRaw (RIP.Proxy @"struct12_t_x") ptr0 struct12_t_x2
            >> HasCField.writeRaw (RIP.Proxy @"struct12_t_self") ptr0 struct12_t_self3

deriving via Marshal.EquivStorable Struct12_t instance RIP.Storable Struct12_t

instance HasCField.HasCField Struct12_t "struct12_t_x" where

  type CFieldType Struct12_t "struct12_t_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "struct12_t_x" (RIP.Ptr Struct12_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"struct12_t_x")

instance HasCField.HasCField Struct12_t "struct12_t_self" where

  type CFieldType Struct12_t "struct12_t_self" =
    RIP.Ptr Struct12_t

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) (RIP.Ptr Struct12_t)
         ) => RIP.HasField "struct12_t_self" (RIP.Ptr Struct12_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"struct12_t_self")

{-| __C declaration:__ @struct use_sites@

    __defined at:__ @program-analysis\/typedef_analysis.h 66:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Use_sites = Use_sites
  { use_sites_useTypedef_struct1_t :: Struct1_t
    {- ^ __C declaration:__ @useTypedef_struct1_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 68:13@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct2_t :: Struct2_t
    {- ^ __C declaration:__ @useTypedef_struct2_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 71:13@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct3_t :: RIP.Ptr Struct3_t
    {- ^ __C declaration:__ @useTypedef_struct3_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 74:14@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct4_t :: RIP.Ptr Struct4_t
    {- ^ __C declaration:__ @useTypedef_struct4_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 75:14@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useStruct_struct5 :: Struct5
    {- ^ __C declaration:__ @useStruct_struct5@

         __defined at:__ @program-analysis\/typedef_analysis.h 78:18@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct5_t :: Struct5_t
    {- ^ __C declaration:__ @useTypedef_struct5_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 79:13@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useStruct_struct6 :: Struct6_Aux
    {- ^ __C declaration:__ @useStruct_struct6@

         __defined at:__ @program-analysis\/typedef_analysis.h 82:18@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct6 :: Struct6
    {- ^ __C declaration:__ @useTypedef_struct6@

         __defined at:__ @program-analysis\/typedef_analysis.h 83:11@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct7a :: Struct7a
    {- ^ __C declaration:__ @useTypedef_struct7a@

         __defined at:__ @program-analysis\/typedef_analysis.h 86:12@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct7b :: Struct7b
    {- ^ __C declaration:__ @useTypedef_struct7b@

         __defined at:__ @program-analysis\/typedef_analysis.h 87:12@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct8 :: Struct8
    {- ^ __C declaration:__ @useTypedef_struct8@

         __defined at:__ @program-analysis\/typedef_analysis.h 91:11@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct8b :: Struct8b
    {- ^ __C declaration:__ @useTypedef_struct8b@

         __defined at:__ @program-analysis\/typedef_analysis.h 92:12@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct9 :: Struct9
    {- ^ __C declaration:__ @useTypedef_struct9@

         __defined at:__ @program-analysis\/typedef_analysis.h 96:11@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct9_t :: Struct9_t
    {- ^ __C declaration:__ @useTypedef_struct9_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 97:13@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct10_t :: Struct10_t
    {- ^ __C declaration:__ @useTypedef_struct10_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 98:14@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct10_t_t :: Struct10_t_t
    {- ^ __C declaration:__ @useTypedef_struct10_t_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 99:16@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct11_t :: Struct11_t
    {- ^ __C declaration:__ @useTypedef_struct11_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 102:14@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct12_t :: Struct12_t
    {- ^ __C declaration:__ @useTypedef_struct12_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 103:14@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Use_sites where

  staticSizeOf = \_ -> (64 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Use_sites where

  readRaw =
    \ptr0 ->
          pure Use_sites
      <*> HasCField.readRaw (RIP.Proxy @"use_sites_useTypedef_struct1_t") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"use_sites_useTypedef_struct2_t") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"use_sites_useTypedef_struct3_t") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"use_sites_useTypedef_struct4_t") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"use_sites_useStruct_struct5") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"use_sites_useTypedef_struct5_t") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"use_sites_useStruct_struct6") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"use_sites_useTypedef_struct6") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"use_sites_useTypedef_struct7a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"use_sites_useTypedef_struct7b") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"use_sites_useTypedef_struct8") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"use_sites_useTypedef_struct8b") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"use_sites_useTypedef_struct9") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"use_sites_useTypedef_struct9_t") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"use_sites_useTypedef_struct10_t") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"use_sites_useTypedef_struct10_t_t") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"use_sites_useTypedef_struct11_t") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"use_sites_useTypedef_struct12_t") ptr0

instance Marshal.WriteRaw Use_sites where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Use_sites
            use_sites_useTypedef_struct1_t2
            use_sites_useTypedef_struct2_t3
            use_sites_useTypedef_struct3_t4
            use_sites_useTypedef_struct4_t5
            use_sites_useStruct_struct56
            use_sites_useTypedef_struct5_t7
            use_sites_useStruct_struct68
            use_sites_useTypedef_struct69
            use_sites_useTypedef_struct7a10
            use_sites_useTypedef_struct7b11
            use_sites_useTypedef_struct812
            use_sites_useTypedef_struct8b13
            use_sites_useTypedef_struct914
            use_sites_useTypedef_struct9_t15
            use_sites_useTypedef_struct10_t16
            use_sites_useTypedef_struct10_t_t17
            use_sites_useTypedef_struct11_t18
            use_sites_useTypedef_struct12_t19 ->
                 HasCField.writeRaw (RIP.Proxy @"use_sites_useTypedef_struct1_t") ptr0 use_sites_useTypedef_struct1_t2
              >> HasCField.writeRaw (RIP.Proxy @"use_sites_useTypedef_struct2_t") ptr0 use_sites_useTypedef_struct2_t3
              >> HasCField.writeRaw (RIP.Proxy @"use_sites_useTypedef_struct3_t") ptr0 use_sites_useTypedef_struct3_t4
              >> HasCField.writeRaw (RIP.Proxy @"use_sites_useTypedef_struct4_t") ptr0 use_sites_useTypedef_struct4_t5
              >> HasCField.writeRaw (RIP.Proxy @"use_sites_useStruct_struct5") ptr0 use_sites_useStruct_struct56
              >> HasCField.writeRaw (RIP.Proxy @"use_sites_useTypedef_struct5_t") ptr0 use_sites_useTypedef_struct5_t7
              >> HasCField.writeRaw (RIP.Proxy @"use_sites_useStruct_struct6") ptr0 use_sites_useStruct_struct68
              >> HasCField.writeRaw (RIP.Proxy @"use_sites_useTypedef_struct6") ptr0 use_sites_useTypedef_struct69
              >> HasCField.writeRaw (RIP.Proxy @"use_sites_useTypedef_struct7a") ptr0 use_sites_useTypedef_struct7a10
              >> HasCField.writeRaw (RIP.Proxy @"use_sites_useTypedef_struct7b") ptr0 use_sites_useTypedef_struct7b11
              >> HasCField.writeRaw (RIP.Proxy @"use_sites_useTypedef_struct8") ptr0 use_sites_useTypedef_struct812
              >> HasCField.writeRaw (RIP.Proxy @"use_sites_useTypedef_struct8b") ptr0 use_sites_useTypedef_struct8b13
              >> HasCField.writeRaw (RIP.Proxy @"use_sites_useTypedef_struct9") ptr0 use_sites_useTypedef_struct914
              >> HasCField.writeRaw (RIP.Proxy @"use_sites_useTypedef_struct9_t") ptr0 use_sites_useTypedef_struct9_t15
              >> HasCField.writeRaw (RIP.Proxy @"use_sites_useTypedef_struct10_t") ptr0 use_sites_useTypedef_struct10_t16
              >> HasCField.writeRaw (RIP.Proxy @"use_sites_useTypedef_struct10_t_t") ptr0 use_sites_useTypedef_struct10_t_t17
              >> HasCField.writeRaw (RIP.Proxy @"use_sites_useTypedef_struct11_t") ptr0 use_sites_useTypedef_struct11_t18
              >> HasCField.writeRaw (RIP.Proxy @"use_sites_useTypedef_struct12_t") ptr0 use_sites_useTypedef_struct12_t19

deriving via Marshal.EquivStorable Use_sites instance RIP.Storable Use_sites

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct1_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct1_t" =
    Struct1_t

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Struct1_t
         ) => RIP.HasField "use_sites_useTypedef_struct1_t" (RIP.Ptr Use_sites) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"use_sites_useTypedef_struct1_t")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct2_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct2_t" =
    Struct2_t

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Struct2_t
         ) => RIP.HasField "use_sites_useTypedef_struct2_t" (RIP.Ptr Use_sites) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"use_sites_useTypedef_struct2_t")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct3_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct3_t" =
    RIP.Ptr Struct3_t

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.Ptr Struct3_t)
         ) => RIP.HasField "use_sites_useTypedef_struct3_t" (RIP.Ptr Use_sites) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"use_sites_useTypedef_struct3_t")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct4_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct4_t" =
    RIP.Ptr Struct4_t

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) (RIP.Ptr Struct4_t)
         ) => RIP.HasField "use_sites_useTypedef_struct4_t" (RIP.Ptr Use_sites) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"use_sites_useTypedef_struct4_t")

instance HasCField.HasCField Use_sites "use_sites_useStruct_struct5" where

  type CFieldType Use_sites "use_sites_useStruct_struct5" =
    Struct5

  offset# = \_ -> \_ -> 16

instance ( ((~) ty) Struct5
         ) => RIP.HasField "use_sites_useStruct_struct5" (RIP.Ptr Use_sites) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"use_sites_useStruct_struct5")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct5_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct5_t" =
    Struct5_t

  offset# = \_ -> \_ -> 16

instance ( ((~) ty) Struct5_t
         ) => RIP.HasField "use_sites_useTypedef_struct5_t" (RIP.Ptr Use_sites) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"use_sites_useTypedef_struct5_t")

instance HasCField.HasCField Use_sites "use_sites_useStruct_struct6" where

  type CFieldType Use_sites "use_sites_useStruct_struct6" =
    Struct6_Aux

  offset# = \_ -> \_ -> 24

instance ( ((~) ty) Struct6_Aux
         ) => RIP.HasField "use_sites_useStruct_struct6" (RIP.Ptr Use_sites) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"use_sites_useStruct_struct6")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct6" where

  type CFieldType Use_sites "use_sites_useTypedef_struct6" =
    Struct6

  offset# = \_ -> \_ -> 24

instance ( ((~) ty) Struct6
         ) => RIP.HasField "use_sites_useTypedef_struct6" (RIP.Ptr Use_sites) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"use_sites_useTypedef_struct6")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct7a" where

  type CFieldType Use_sites "use_sites_useTypedef_struct7a" =
    Struct7a

  offset# = \_ -> \_ -> 32

instance ( ((~) ty) Struct7a
         ) => RIP.HasField "use_sites_useTypedef_struct7a" (RIP.Ptr Use_sites) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"use_sites_useTypedef_struct7a")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct7b" where

  type CFieldType Use_sites "use_sites_useTypedef_struct7b" =
    Struct7b

  offset# = \_ -> \_ -> 32

instance ( ((~) ty) Struct7b
         ) => RIP.HasField "use_sites_useTypedef_struct7b" (RIP.Ptr Use_sites) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"use_sites_useTypedef_struct7b")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct8" where

  type CFieldType Use_sites "use_sites_useTypedef_struct8" =
    Struct8

  offset# = \_ -> \_ -> 32

instance ( ((~) ty) Struct8
         ) => RIP.HasField "use_sites_useTypedef_struct8" (RIP.Ptr Use_sites) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"use_sites_useTypedef_struct8")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct8b" where

  type CFieldType Use_sites "use_sites_useTypedef_struct8b" =
    Struct8b

  offset# = \_ -> \_ -> 32

instance ( ((~) ty) Struct8b
         ) => RIP.HasField "use_sites_useTypedef_struct8b" (RIP.Ptr Use_sites) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"use_sites_useTypedef_struct8b")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct9" where

  type CFieldType Use_sites "use_sites_useTypedef_struct9" =
    Struct9

  offset# = \_ -> \_ -> 32

instance ( ((~) ty) Struct9
         ) => RIP.HasField "use_sites_useTypedef_struct9" (RIP.Ptr Use_sites) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"use_sites_useTypedef_struct9")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct9_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct9_t" =
    Struct9_t

  offset# = \_ -> \_ -> 32

instance ( ((~) ty) Struct9_t
         ) => RIP.HasField "use_sites_useTypedef_struct9_t" (RIP.Ptr Use_sites) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"use_sites_useTypedef_struct9_t")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct10_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct10_t" =
    Struct10_t

  offset# = \_ -> \_ -> 32

instance ( ((~) ty) Struct10_t
         ) => RIP.HasField "use_sites_useTypedef_struct10_t" (RIP.Ptr Use_sites) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"use_sites_useTypedef_struct10_t")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct10_t_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct10_t_t" =
    Struct10_t_t

  offset# = \_ -> \_ -> 32

instance ( ((~) ty) Struct10_t_t
         ) => RIP.HasField "use_sites_useTypedef_struct10_t_t" (RIP.Ptr Use_sites) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"use_sites_useTypedef_struct10_t_t")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct11_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct11_t" =
    Struct11_t

  offset# = \_ -> \_ -> 32

instance ( ((~) ty) Struct11_t
         ) => RIP.HasField "use_sites_useTypedef_struct11_t" (RIP.Ptr Use_sites) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"use_sites_useTypedef_struct11_t")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct12_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct12_t" =
    Struct12_t

  offset# = \_ -> \_ -> 48

instance ( ((~) ty) Struct12_t
         ) => RIP.HasField "use_sites_useTypedef_struct12_t" (RIP.Ptr Use_sites) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"use_sites_useTypedef_struct12_t")
