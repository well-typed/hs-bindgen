{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Struct1_t(..)
    , Example.Struct2_t(..)
    , Example.Struct3_t
    , Example.Struct4_t
    , Example.Struct5(..)
    , Example.Struct5_t(..)
    , Example.Struct6a_struct(..)
    , Example.Struct6a(..)
    , Example.Struct6b_struct(..)
    , Example.Struct6b(..)
    , Example.Struct7(..)
    , Example.Struct7a(..)
    , Example.Struct7b(..)
    , Example.Struct8(..)
    , Example.Struct8b(..)
    , Example.Struct9(..)
    , Example.Struct9_t(..)
    , Example.Struct10_t(..)
    , Example.Struct10_t_t(..)
    , Example.Struct11_t(..)
    , Example.Struct12_t(..)
    , Example.Use_sites(..)
    , Example.Foo_struct(..)
    , Example.Foo_Aux(..)
    , Example.Foo(..)
    , Example.Bar_struct(..)
    , Example.Bar_Aux(..)
    , Example.Bar(..)
    , Example.Struct15(..)
    , Example.Struct16_struct(..)
    , Example.Struct16(..)
    , Example.Use_sites_qual(..)
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct struct1@

    __defined at:__ @program-analysis\/typedef_analysis.h 41:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct1_t = Struct1_t
  {}
  deriving stock (Eq, BG.Generic, Show)

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

deriving via Marshal.EquivStorable Struct1_t instance BG.Storable Struct1_t

{-| __C declaration:__ @struct struct2@

    __defined at:__ @program-analysis\/typedef_analysis.h 45:16@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct2_t = Struct2_t
  {}
  deriving stock (Eq, BG.Generic, Show)

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

deriving via Marshal.EquivStorable Struct2_t instance BG.Storable Struct2_t

{-| __C declaration:__ @struct struct3@

    __defined at:__ @program-analysis\/typedef_analysis.h 49:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct3_t

{-| __C declaration:__ @struct struct4@

    __defined at:__ @program-analysis\/typedef_analysis.h 53:16@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct4_t

{-| __C declaration:__ @struct struct5@

    __defined at:__ @program-analysis\/typedef_analysis.h 56:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct5 = Struct5
  {}
  deriving stock (Eq, BG.Generic, Show)

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

deriving via Marshal.EquivStorable Struct5 instance BG.Storable Struct5

{-| __C declaration:__ @struct5_t@

    __defined at:__ @program-analysis\/typedef_analysis.h 57:25@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Struct5_t = Struct5_t
  { unwrapStruct5_t :: BG.Ptr Struct5
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr Struct5
         ) => BG.CompatHasField.HasField "unwrapStruct5_t" Struct5_t ty where

  hasField =
    \x0 ->
      (\y1 ->
         Struct5_t {unwrapStruct5_t = y1}, BG.getField @"unwrapStruct5_t" x0)

instance ( ty ~ BG.Ptr Struct5
         ) => BG.HasField "unwrapStruct5_t" (BG.Ptr Struct5_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapStruct5_t")

instance HasCField.HasCField Struct5_t "unwrapStruct5_t" where

  type CFieldType Struct5_t "unwrapStruct5_t" =
    BG.Ptr Struct5

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct6a@

    __defined at:__ @program-analysis\/typedef_analysis.h 60:16@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct6a_struct = Struct6a_struct
  {}
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Struct6a_struct where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Struct6a_struct where

  readRaw = \ptr0 -> pure Struct6a_struct

instance Marshal.WriteRaw Struct6a_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct6a_struct -> return ()

deriving via Marshal.EquivStorable Struct6a_struct instance BG.Storable Struct6a_struct

{-| __C declaration:__ @struct6a@

    __defined at:__ @program-analysis\/typedef_analysis.h 61:4@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Struct6a = Struct6a
  { unwrapStruct6a :: BG.Ptr Struct6a_struct
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr Struct6a_struct
         ) => BG.CompatHasField.HasField "unwrapStruct6a" Struct6a ty where

  hasField =
    \x0 ->
      (\y1 ->
         Struct6a {unwrapStruct6a = y1}, BG.getField @"unwrapStruct6a" x0)

instance ( ty ~ BG.Ptr Struct6a_struct
         ) => BG.HasField "unwrapStruct6a" (BG.Ptr Struct6a) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapStruct6a")

instance HasCField.HasCField Struct6a "unwrapStruct6a" where

  type CFieldType Struct6a "unwrapStruct6a" =
    BG.Ptr Struct6a_struct

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct6b@

    __defined at:__ @program-analysis\/typedef_analysis.h 68:16@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct6b_struct = Struct6b_struct
  {}
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Struct6b_struct where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Struct6b_struct where

  readRaw = \ptr0 -> pure Struct6b_struct

instance Marshal.WriteRaw Struct6b_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct6b_struct -> return ()

deriving via Marshal.EquivStorable Struct6b_struct instance BG.Storable Struct6b_struct

{-| __C declaration:__ @struct6b@

    __defined at:__ @program-analysis\/typedef_analysis.h 69:3@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Struct6b = Struct6b
  { unwrapStruct6b :: CA.ConstantArray 50 Struct6b_struct
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 50 Struct6b_struct
         ) => BG.CompatHasField.HasField "unwrapStruct6b" Struct6b ty where

  hasField =
    \x0 ->
      (\y1 ->
         Struct6b {unwrapStruct6b = y1}, BG.getField @"unwrapStruct6b" x0)

instance ( ty ~ CA.ConstantArray 50 Struct6b_struct
         ) => BG.HasField "unwrapStruct6b" (BG.Ptr Struct6b) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapStruct6b")

instance HasCField.HasCField Struct6b "unwrapStruct6b" where

  type CFieldType Struct6b "unwrapStruct6b" =
    CA.ConstantArray 50 Struct6b_struct

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct7@

    __defined at:__ @program-analysis\/typedef_analysis.h 72:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct7 = Struct7
  {}
  deriving stock (Eq, BG.Generic, Show)

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

deriving via Marshal.EquivStorable Struct7 instance BG.Storable Struct7

{-| __C declaration:__ @struct7a@

    __defined at:__ @program-analysis\/typedef_analysis.h 73:24@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Struct7a = Struct7a
  { unwrapStruct7a :: Struct7
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ Struct7
         ) => BG.CompatHasField.HasField "unwrapStruct7a" Struct7a ty where

  hasField =
    \x0 ->
      (\y1 ->
         Struct7a {unwrapStruct7a = y1}, BG.getField @"unwrapStruct7a" x0)

instance ( ty ~ Struct7
         ) => BG.HasField "unwrapStruct7a" (BG.Ptr Struct7a) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapStruct7a")

instance HasCField.HasCField Struct7a "unwrapStruct7a" where

  type CFieldType Struct7a "unwrapStruct7a" = Struct7

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct7b@

    __defined at:__ @program-analysis\/typedef_analysis.h 74:24@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Struct7b = Struct7b
  { unwrapStruct7b :: Struct7
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ Struct7
         ) => BG.CompatHasField.HasField "unwrapStruct7b" Struct7b ty where

  hasField =
    \x0 ->
      (\y1 ->
         Struct7b {unwrapStruct7b = y1}, BG.getField @"unwrapStruct7b" x0)

instance ( ty ~ Struct7
         ) => BG.HasField "unwrapStruct7b" (BG.Ptr Struct7b) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapStruct7b")

instance HasCField.HasCField Struct7b "unwrapStruct7b" where

  type CFieldType Struct7b "unwrapStruct7b" = Struct7

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct8@

    __defined at:__ @program-analysis\/typedef_analysis.h 77:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct8 = Struct8
  {}
  deriving stock (Eq, BG.Generic, Show)

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

deriving via Marshal.EquivStorable Struct8 instance BG.Storable Struct8

{-| __C declaration:__ @struct8b@

    __defined at:__ @program-analysis\/typedef_analysis.h 79:24@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Struct8b = Struct8b
  { unwrapStruct8b :: Struct8
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ Struct8
         ) => BG.CompatHasField.HasField "unwrapStruct8b" Struct8b ty where

  hasField =
    \x0 ->
      (\y1 ->
         Struct8b {unwrapStruct8b = y1}, BG.getField @"unwrapStruct8b" x0)

instance ( ty ~ Struct8
         ) => BG.HasField "unwrapStruct8b" (BG.Ptr Struct8b) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapStruct8b")

instance HasCField.HasCField Struct8b "unwrapStruct8b" where

  type CFieldType Struct8b "unwrapStruct8b" = Struct8

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct9@

    __defined at:__ @program-analysis\/typedef_analysis.h 82:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct9 = Struct9
  {}
  deriving stock (Eq, BG.Generic, Show)

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

deriving via Marshal.EquivStorable Struct9 instance BG.Storable Struct9

{-| __C declaration:__ @struct9_t@

    __defined at:__ @program-analysis\/typedef_analysis.h 84:17@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Struct9_t = Struct9_t
  { unwrapStruct9_t :: Struct9
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ Struct9
         ) => BG.CompatHasField.HasField "unwrapStruct9_t" Struct9_t ty where

  hasField =
    \x0 ->
      (\y1 ->
         Struct9_t {unwrapStruct9_t = y1}, BG.getField @"unwrapStruct9_t" x0)

instance ( ty ~ Struct9
         ) => BG.HasField "unwrapStruct9_t" (BG.Ptr Struct9_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapStruct9_t")

instance HasCField.HasCField Struct9_t "unwrapStruct9_t" where

  type CFieldType Struct9_t "unwrapStruct9_t" = Struct9

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct10@

    __defined at:__ @program-analysis\/typedef_analysis.h 90:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct10_t = Struct10_t
  {}
  deriving stock (Eq, BG.Generic, Show)

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

deriving via Marshal.EquivStorable Struct10_t instance BG.Storable Struct10_t

{-| __C declaration:__ @struct10_t_t@

    __defined at:__ @program-analysis\/typedef_analysis.h 92:20@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Struct10_t_t = Struct10_t_t
  { unwrapStruct10_t_t :: Struct10_t
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ Struct10_t
         ) => BG.CompatHasField.HasField "unwrapStruct10_t_t" Struct10_t_t ty where

  hasField =
    \x0 ->
      ( \y1 -> Struct10_t_t {unwrapStruct10_t_t = y1}
      , BG.getField @"unwrapStruct10_t_t" x0
      )

instance ( ty ~ Struct10_t
         ) => BG.HasField "unwrapStruct10_t_t" (BG.Ptr Struct10_t_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapStruct10_t_t")

instance HasCField.HasCField Struct10_t_t "unwrapStruct10_t_t" where

  type CFieldType Struct10_t_t "unwrapStruct10_t_t" =
    Struct10_t

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct11@

    __defined at:__ @program-analysis\/typedef_analysis.h 95:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct11_t = Struct11_t
  { struct11_t_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/typedef_analysis.h 96:7@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , struct11_t_self :: BG.Ptr Struct11_t
    {- ^ __C declaration:__ @self@

         __defined at:__ @program-analysis\/typedef_analysis.h 97:20@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Struct11_t where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Struct11_t where

  readRaw =
    \ptr0 ->
          pure Struct11_t
      <*> HasCField.readRaw (BG.Proxy @"struct11_t_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"struct11_t_self") ptr0

instance Marshal.WriteRaw Struct11_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct11_t struct11_t_x2 struct11_t_self3 ->
               HasCField.writeRaw (BG.Proxy @"struct11_t_x") ptr0 struct11_t_x2
            >> HasCField.writeRaw (BG.Proxy @"struct11_t_self") ptr0 struct11_t_self3

deriving via Marshal.EquivStorable Struct11_t instance BG.Storable Struct11_t

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "struct11_t_x" Struct11_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Struct11_t {struct11_t_x = y1, struct11_t_self = BG.getField @"struct11_t_self" x0}
      , BG.getField @"struct11_t_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "struct11_t_x" (BG.Ptr Struct11_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"struct11_t_x")

instance HasCField.HasCField Struct11_t "struct11_t_x" where

  type CFieldType Struct11_t "struct11_t_x" = BG.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ BG.Ptr Struct11_t
         ) => BG.CompatHasField.HasField "struct11_t_self" Struct11_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Struct11_t {struct11_t_self = y1, struct11_t_x = BG.getField @"struct11_t_x" x0}
      , BG.getField @"struct11_t_self" x0
      )

instance ( ty ~ BG.Ptr Struct11_t
         ) => BG.HasField "struct11_t_self" (BG.Ptr Struct11_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"struct11_t_self")

instance HasCField.HasCField Struct11_t "struct11_t_self" where

  type CFieldType Struct11_t "struct11_t_self" =
    BG.Ptr Struct11_t

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @struct struct12@

    __defined at:__ @program-analysis\/typedef_analysis.h 104:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct12_t = Struct12_t
  { struct12_t_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/typedef_analysis.h 105:7@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , struct12_t_self :: BG.Ptr Struct12_t
    {- ^ __C declaration:__ @self@

         __defined at:__ @program-analysis\/typedef_analysis.h 106:15@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Struct12_t where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Struct12_t where

  readRaw =
    \ptr0 ->
          pure Struct12_t
      <*> HasCField.readRaw (BG.Proxy @"struct12_t_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"struct12_t_self") ptr0

instance Marshal.WriteRaw Struct12_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct12_t struct12_t_x2 struct12_t_self3 ->
               HasCField.writeRaw (BG.Proxy @"struct12_t_x") ptr0 struct12_t_x2
            >> HasCField.writeRaw (BG.Proxy @"struct12_t_self") ptr0 struct12_t_self3

deriving via Marshal.EquivStorable Struct12_t instance BG.Storable Struct12_t

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "struct12_t_x" Struct12_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Struct12_t {struct12_t_x = y1, struct12_t_self = BG.getField @"struct12_t_self" x0}
      , BG.getField @"struct12_t_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "struct12_t_x" (BG.Ptr Struct12_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"struct12_t_x")

instance HasCField.HasCField Struct12_t "struct12_t_x" where

  type CFieldType Struct12_t "struct12_t_x" = BG.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ BG.Ptr Struct12_t
         ) => BG.CompatHasField.HasField "struct12_t_self" Struct12_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Struct12_t {struct12_t_self = y1, struct12_t_x = BG.getField @"struct12_t_x" x0}
      , BG.getField @"struct12_t_self" x0
      )

instance ( ty ~ BG.Ptr Struct12_t
         ) => BG.HasField "struct12_t_self" (BG.Ptr Struct12_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"struct12_t_self")

instance HasCField.HasCField Struct12_t "struct12_t_self" where

  type CFieldType Struct12_t "struct12_t_self" =
    BG.Ptr Struct12_t

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @struct use_sites@

    __defined at:__ @program-analysis\/typedef_analysis.h 110:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Use_sites = Use_sites
  { use_sites_useTypedef_struct1_t :: Struct1_t
    {- ^ __C declaration:__ @useTypedef_struct1_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 112:13@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct2_t :: Struct2_t
    {- ^ __C declaration:__ @useTypedef_struct2_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 115:13@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct3_t :: BG.Ptr Struct3_t
    {- ^ __C declaration:__ @useTypedef_struct3_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 118:14@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct4_t :: BG.Ptr Struct4_t
    {- ^ __C declaration:__ @useTypedef_struct4_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 119:14@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useStruct_struct5 :: Struct5
    {- ^ __C declaration:__ @useStruct_struct5@

         __defined at:__ @program-analysis\/typedef_analysis.h 122:18@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct5_t :: Struct5_t
    {- ^ __C declaration:__ @useTypedef_struct5_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 123:13@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useStruct_struct6a :: Struct6a_struct
    {- ^ __C declaration:__ @useStruct_struct6a@

         __defined at:__ @program-analysis\/typedef_analysis.h 126:19@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct6a :: Struct6a
    {- ^ __C declaration:__ @useTypedef_struct6a@

         __defined at:__ @program-analysis\/typedef_analysis.h 127:12@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useStruct_struct6b :: Struct6b_struct
    {- ^ __C declaration:__ @useStruct_struct6b@

         __defined at:__ @program-analysis\/typedef_analysis.h 130:19@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct6b :: Struct6b
    {- ^ __C declaration:__ @useTypedef_struct6b@

         __defined at:__ @program-analysis\/typedef_analysis.h 131:12@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct7a :: Struct7a
    {- ^ __C declaration:__ @useTypedef_struct7a@

         __defined at:__ @program-analysis\/typedef_analysis.h 134:12@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct7b :: Struct7b
    {- ^ __C declaration:__ @useTypedef_struct7b@

         __defined at:__ @program-analysis\/typedef_analysis.h 135:12@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct8 :: Struct8
    {- ^ __C declaration:__ @useTypedef_struct8@

         __defined at:__ @program-analysis\/typedef_analysis.h 139:11@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct8b :: Struct8b
    {- ^ __C declaration:__ @useTypedef_struct8b@

         __defined at:__ @program-analysis\/typedef_analysis.h 140:12@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct9 :: Struct9
    {- ^ __C declaration:__ @useTypedef_struct9@

         __defined at:__ @program-analysis\/typedef_analysis.h 144:11@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct9_t :: Struct9_t
    {- ^ __C declaration:__ @useTypedef_struct9_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 145:13@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct10_t :: Struct10_t
    {- ^ __C declaration:__ @useTypedef_struct10_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 146:14@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct10_t_t :: Struct10_t_t
    {- ^ __C declaration:__ @useTypedef_struct10_t_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 147:16@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct11_t :: Struct11_t
    {- ^ __C declaration:__ @useTypedef_struct11_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 150:14@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct12_t :: Struct12_t
    {- ^ __C declaration:__ @useTypedef_struct12_t@

         __defined at:__ @program-analysis\/typedef_analysis.h 151:14@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Use_sites where

  staticSizeOf = \_ -> (64 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Use_sites where

  readRaw =
    \ptr0 ->
          pure Use_sites
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useTypedef_struct1_t") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useTypedef_struct2_t") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useTypedef_struct3_t") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useTypedef_struct4_t") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useStruct_struct5") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useTypedef_struct5_t") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useStruct_struct6a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useTypedef_struct6a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useStruct_struct6b") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useTypedef_struct6b") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useTypedef_struct7a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useTypedef_struct7b") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useTypedef_struct8") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useTypedef_struct8b") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useTypedef_struct9") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useTypedef_struct9_t") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useTypedef_struct10_t") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useTypedef_struct10_t_t") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useTypedef_struct11_t") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_useTypedef_struct12_t") ptr0

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
            use_sites_useStruct_struct6a8
            use_sites_useTypedef_struct6a9
            use_sites_useStruct_struct6b10
            use_sites_useTypedef_struct6b11
            use_sites_useTypedef_struct7a12
            use_sites_useTypedef_struct7b13
            use_sites_useTypedef_struct814
            use_sites_useTypedef_struct8b15
            use_sites_useTypedef_struct916
            use_sites_useTypedef_struct9_t17
            use_sites_useTypedef_struct10_t18
            use_sites_useTypedef_struct10_t_t19
            use_sites_useTypedef_struct11_t20
            use_sites_useTypedef_struct12_t21 ->
                 HasCField.writeRaw (BG.Proxy @"use_sites_useTypedef_struct1_t") ptr0 use_sites_useTypedef_struct1_t2
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useTypedef_struct2_t") ptr0 use_sites_useTypedef_struct2_t3
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useTypedef_struct3_t") ptr0 use_sites_useTypedef_struct3_t4
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useTypedef_struct4_t") ptr0 use_sites_useTypedef_struct4_t5
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useStruct_struct5") ptr0 use_sites_useStruct_struct56
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useTypedef_struct5_t") ptr0 use_sites_useTypedef_struct5_t7
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useStruct_struct6a") ptr0 use_sites_useStruct_struct6a8
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useTypedef_struct6a") ptr0 use_sites_useTypedef_struct6a9
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useStruct_struct6b") ptr0 use_sites_useStruct_struct6b10
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useTypedef_struct6b") ptr0 use_sites_useTypedef_struct6b11
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useTypedef_struct7a") ptr0 use_sites_useTypedef_struct7a12
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useTypedef_struct7b") ptr0 use_sites_useTypedef_struct7b13
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useTypedef_struct8") ptr0 use_sites_useTypedef_struct814
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useTypedef_struct8b") ptr0 use_sites_useTypedef_struct8b15
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useTypedef_struct9") ptr0 use_sites_useTypedef_struct916
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useTypedef_struct9_t") ptr0 use_sites_useTypedef_struct9_t17
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useTypedef_struct10_t") ptr0 use_sites_useTypedef_struct10_t18
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useTypedef_struct10_t_t") ptr0 use_sites_useTypedef_struct10_t_t19
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useTypedef_struct11_t") ptr0 use_sites_useTypedef_struct11_t20
              >> HasCField.writeRaw (BG.Proxy @"use_sites_useTypedef_struct12_t") ptr0 use_sites_useTypedef_struct12_t21

deriving via Marshal.EquivStorable Use_sites instance BG.Storable Use_sites

instance ( ty ~ Struct1_t
         ) => BG.CompatHasField.HasField "use_sites_useTypedef_struct1_t" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useTypedef_struct1_t = y1
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useTypedef_struct1_t" x0
      )

instance ( ty ~ Struct1_t
         ) => BG.HasField "use_sites_useTypedef_struct1_t" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useTypedef_struct1_t")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct1_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct1_t" =
    Struct1_t

  offset# = \_ -> \_ -> 0

instance ( ty ~ Struct2_t
         ) => BG.CompatHasField.HasField "use_sites_useTypedef_struct2_t" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useTypedef_struct2_t = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useTypedef_struct2_t" x0
      )

instance ( ty ~ Struct2_t
         ) => BG.HasField "use_sites_useTypedef_struct2_t" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useTypedef_struct2_t")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct2_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct2_t" =
    Struct2_t

  offset# = \_ -> \_ -> 0

instance ( ty ~ BG.Ptr Struct3_t
         ) => BG.CompatHasField.HasField "use_sites_useTypedef_struct3_t" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useTypedef_struct3_t = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useTypedef_struct3_t" x0
      )

instance ( ty ~ BG.Ptr Struct3_t
         ) => BG.HasField "use_sites_useTypedef_struct3_t" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useTypedef_struct3_t")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct3_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct3_t" =
    BG.Ptr Struct3_t

  offset# = \_ -> \_ -> 0

instance ( ty ~ BG.Ptr Struct4_t
         ) => BG.CompatHasField.HasField "use_sites_useTypedef_struct4_t" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useTypedef_struct4_t = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useTypedef_struct4_t" x0
      )

instance ( ty ~ BG.Ptr Struct4_t
         ) => BG.HasField "use_sites_useTypedef_struct4_t" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useTypedef_struct4_t")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct4_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct4_t" =
    BG.Ptr Struct4_t

  offset# = \_ -> \_ -> 8

instance ( ty ~ Struct5
         ) => BG.CompatHasField.HasField "use_sites_useStruct_struct5" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useStruct_struct5 = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useStruct_struct5" x0
      )

instance ( ty ~ Struct5
         ) => BG.HasField "use_sites_useStruct_struct5" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useStruct_struct5")

instance HasCField.HasCField Use_sites "use_sites_useStruct_struct5" where

  type CFieldType Use_sites "use_sites_useStruct_struct5" =
    Struct5

  offset# = \_ -> \_ -> 16

instance ( ty ~ Struct5_t
         ) => BG.CompatHasField.HasField "use_sites_useTypedef_struct5_t" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useTypedef_struct5_t = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useTypedef_struct5_t" x0
      )

instance ( ty ~ Struct5_t
         ) => BG.HasField "use_sites_useTypedef_struct5_t" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useTypedef_struct5_t")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct5_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct5_t" =
    Struct5_t

  offset# = \_ -> \_ -> 16

instance ( ty ~ Struct6a_struct
         ) => BG.CompatHasField.HasField "use_sites_useStruct_struct6a" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useStruct_struct6a = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useStruct_struct6a" x0
      )

instance ( ty ~ Struct6a_struct
         ) => BG.HasField "use_sites_useStruct_struct6a" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useStruct_struct6a")

instance HasCField.HasCField Use_sites "use_sites_useStruct_struct6a" where

  type CFieldType Use_sites "use_sites_useStruct_struct6a" =
    Struct6a_struct

  offset# = \_ -> \_ -> 24

instance ( ty ~ Struct6a
         ) => BG.CompatHasField.HasField "use_sites_useTypedef_struct6a" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useTypedef_struct6a = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useTypedef_struct6a" x0
      )

instance ( ty ~ Struct6a
         ) => BG.HasField "use_sites_useTypedef_struct6a" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useTypedef_struct6a")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct6a" where

  type CFieldType Use_sites "use_sites_useTypedef_struct6a" =
    Struct6a

  offset# = \_ -> \_ -> 24

instance ( ty ~ Struct6b_struct
         ) => BG.CompatHasField.HasField "use_sites_useStruct_struct6b" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useStruct_struct6b = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useStruct_struct6b" x0
      )

instance ( ty ~ Struct6b_struct
         ) => BG.HasField "use_sites_useStruct_struct6b" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useStruct_struct6b")

instance HasCField.HasCField Use_sites "use_sites_useStruct_struct6b" where

  type CFieldType Use_sites "use_sites_useStruct_struct6b" =
    Struct6b_struct

  offset# = \_ -> \_ -> 32

instance ( ty ~ Struct6b
         ) => BG.CompatHasField.HasField "use_sites_useTypedef_struct6b" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useTypedef_struct6b = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useTypedef_struct6b" x0
      )

instance ( ty ~ Struct6b
         ) => BG.HasField "use_sites_useTypedef_struct6b" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useTypedef_struct6b")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct6b" where

  type CFieldType Use_sites "use_sites_useTypedef_struct6b" =
    Struct6b

  offset# = \_ -> \_ -> 32

instance ( ty ~ Struct7a
         ) => BG.CompatHasField.HasField "use_sites_useTypedef_struct7a" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useTypedef_struct7a = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useTypedef_struct7a" x0
      )

instance ( ty ~ Struct7a
         ) => BG.HasField "use_sites_useTypedef_struct7a" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useTypedef_struct7a")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct7a" where

  type CFieldType Use_sites "use_sites_useTypedef_struct7a" =
    Struct7a

  offset# = \_ -> \_ -> 32

instance ( ty ~ Struct7b
         ) => BG.CompatHasField.HasField "use_sites_useTypedef_struct7b" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useTypedef_struct7b = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useTypedef_struct7b" x0
      )

instance ( ty ~ Struct7b
         ) => BG.HasField "use_sites_useTypedef_struct7b" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useTypedef_struct7b")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct7b" where

  type CFieldType Use_sites "use_sites_useTypedef_struct7b" =
    Struct7b

  offset# = \_ -> \_ -> 32

instance ( ty ~ Struct8
         ) => BG.CompatHasField.HasField "use_sites_useTypedef_struct8" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useTypedef_struct8 = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useTypedef_struct8" x0
      )

instance ( ty ~ Struct8
         ) => BG.HasField "use_sites_useTypedef_struct8" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useTypedef_struct8")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct8" where

  type CFieldType Use_sites "use_sites_useTypedef_struct8" =
    Struct8

  offset# = \_ -> \_ -> 32

instance ( ty ~ Struct8b
         ) => BG.CompatHasField.HasField "use_sites_useTypedef_struct8b" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useTypedef_struct8b = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useTypedef_struct8b" x0
      )

instance ( ty ~ Struct8b
         ) => BG.HasField "use_sites_useTypedef_struct8b" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useTypedef_struct8b")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct8b" where

  type CFieldType Use_sites "use_sites_useTypedef_struct8b" =
    Struct8b

  offset# = \_ -> \_ -> 32

instance ( ty ~ Struct9
         ) => BG.CompatHasField.HasField "use_sites_useTypedef_struct9" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useTypedef_struct9 = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useTypedef_struct9" x0
      )

instance ( ty ~ Struct9
         ) => BG.HasField "use_sites_useTypedef_struct9" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useTypedef_struct9")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct9" where

  type CFieldType Use_sites "use_sites_useTypedef_struct9" =
    Struct9

  offset# = \_ -> \_ -> 32

instance ( ty ~ Struct9_t
         ) => BG.CompatHasField.HasField "use_sites_useTypedef_struct9_t" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useTypedef_struct9_t = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useTypedef_struct9_t" x0
      )

instance ( ty ~ Struct9_t
         ) => BG.HasField "use_sites_useTypedef_struct9_t" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useTypedef_struct9_t")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct9_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct9_t" =
    Struct9_t

  offset# = \_ -> \_ -> 32

instance ( ty ~ Struct10_t
         ) => BG.CompatHasField.HasField "use_sites_useTypedef_struct10_t" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useTypedef_struct10_t = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useTypedef_struct10_t" x0
      )

instance ( ty ~ Struct10_t
         ) => BG.HasField "use_sites_useTypedef_struct10_t" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useTypedef_struct10_t")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct10_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct10_t" =
    Struct10_t

  offset# = \_ -> \_ -> 32

instance ( ty ~ Struct10_t_t
         ) => BG.CompatHasField.HasField "use_sites_useTypedef_struct10_t_t" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useTypedef_struct10_t_t = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useTypedef_struct10_t_t" x0
      )

instance ( ty ~ Struct10_t_t
         ) => BG.HasField "use_sites_useTypedef_struct10_t_t" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useTypedef_struct10_t_t")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct10_t_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct10_t_t" =
    Struct10_t_t

  offset# = \_ -> \_ -> 32

instance ( ty ~ Struct11_t
         ) => BG.CompatHasField.HasField "use_sites_useTypedef_struct11_t" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useTypedef_struct11_t = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct12_t = BG.getField @"use_sites_useTypedef_struct12_t" x0
                    }
      , BG.getField @"use_sites_useTypedef_struct11_t" x0
      )

instance ( ty ~ Struct11_t
         ) => BG.HasField "use_sites_useTypedef_struct11_t" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useTypedef_struct11_t")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct11_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct11_t" =
    Struct11_t

  offset# = \_ -> \_ -> 32

instance ( ty ~ Struct12_t
         ) => BG.CompatHasField.HasField "use_sites_useTypedef_struct12_t" Use_sites ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites { use_sites_useTypedef_struct12_t = y1
                    , use_sites_useTypedef_struct1_t = BG.getField @"use_sites_useTypedef_struct1_t" x0
                    , use_sites_useTypedef_struct2_t = BG.getField @"use_sites_useTypedef_struct2_t" x0
                    , use_sites_useTypedef_struct3_t = BG.getField @"use_sites_useTypedef_struct3_t" x0
                    , use_sites_useTypedef_struct4_t = BG.getField @"use_sites_useTypedef_struct4_t" x0
                    , use_sites_useStruct_struct5 = BG.getField @"use_sites_useStruct_struct5" x0
                    , use_sites_useTypedef_struct5_t = BG.getField @"use_sites_useTypedef_struct5_t" x0
                    , use_sites_useStruct_struct6a = BG.getField @"use_sites_useStruct_struct6a" x0
                    , use_sites_useTypedef_struct6a = BG.getField @"use_sites_useTypedef_struct6a" x0
                    , use_sites_useStruct_struct6b = BG.getField @"use_sites_useStruct_struct6b" x0
                    , use_sites_useTypedef_struct6b = BG.getField @"use_sites_useTypedef_struct6b" x0
                    , use_sites_useTypedef_struct7a = BG.getField @"use_sites_useTypedef_struct7a" x0
                    , use_sites_useTypedef_struct7b = BG.getField @"use_sites_useTypedef_struct7b" x0
                    , use_sites_useTypedef_struct8 = BG.getField @"use_sites_useTypedef_struct8" x0
                    , use_sites_useTypedef_struct8b = BG.getField @"use_sites_useTypedef_struct8b" x0
                    , use_sites_useTypedef_struct9 = BG.getField @"use_sites_useTypedef_struct9" x0
                    , use_sites_useTypedef_struct9_t = BG.getField @"use_sites_useTypedef_struct9_t" x0
                    , use_sites_useTypedef_struct10_t = BG.getField @"use_sites_useTypedef_struct10_t" x0
                    , use_sites_useTypedef_struct10_t_t = BG.getField @"use_sites_useTypedef_struct10_t_t" x0
                    , use_sites_useTypedef_struct11_t = BG.getField @"use_sites_useTypedef_struct11_t" x0
                    }
      , BG.getField @"use_sites_useTypedef_struct12_t" x0
      )

instance ( ty ~ Struct12_t
         ) => BG.HasField "use_sites_useTypedef_struct12_t" (BG.Ptr Use_sites) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_useTypedef_struct12_t")

instance HasCField.HasCField Use_sites "use_sites_useTypedef_struct12_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct12_t" =
    Struct12_t

  offset# = \_ -> \_ -> 48

{-| __C declaration:__ @struct foo@

    __defined at:__ @program-analysis\/typedef_analysis.h 163:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Foo_struct = Foo_struct
  { foo_struct_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/typedef_analysis.h 164:7@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , foo_struct_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @program-analysis\/typedef_analysis.h 165:7@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Foo_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Foo_struct where

  readRaw =
    \ptr0 ->
          pure Foo_struct
      <*> HasCField.readRaw (BG.Proxy @"foo_struct_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"foo_struct_y") ptr0

instance Marshal.WriteRaw Foo_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo_struct foo_struct_x2 foo_struct_y3 ->
               HasCField.writeRaw (BG.Proxy @"foo_struct_x") ptr0 foo_struct_x2
            >> HasCField.writeRaw (BG.Proxy @"foo_struct_y") ptr0 foo_struct_y3

deriving via Marshal.EquivStorable Foo_struct instance BG.Storable Foo_struct

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "foo_struct_x" Foo_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Foo_struct {foo_struct_x = y1, foo_struct_y = BG.getField @"foo_struct_y" x0}
      , BG.getField @"foo_struct_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "foo_struct_x" (BG.Ptr Foo_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"foo_struct_x")

instance HasCField.HasCField Foo_struct "foo_struct_x" where

  type CFieldType Foo_struct "foo_struct_x" = BG.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "foo_struct_y" Foo_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Foo_struct {foo_struct_y = y1, foo_struct_x = BG.getField @"foo_struct_x" x0}
      , BG.getField @"foo_struct_y" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "foo_struct_y" (BG.Ptr Foo_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"foo_struct_y")

instance HasCField.HasCField Foo_struct "foo_struct_y" where

  type CFieldType Foo_struct "foo_struct_y" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| Auxiliary type used by 'Foo'

    __C declaration:__ @foo@

    __defined at:__ @program-analysis\/typedef_analysis.h 167:22@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Foo_Aux = Foo_Aux
  { unwrapFoo_Aux :: IO Foo_struct
  }
  deriving stock (BG.Generic)

instance ( ty ~ IO Foo_struct
         ) => BG.CompatHasField.HasField "unwrapFoo_Aux" Foo_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         Foo_Aux {unwrapFoo_Aux = y1}, BG.getField @"unwrapFoo_Aux" x0)

instance ( ty ~ IO Foo_struct
         ) => BG.HasField "unwrapFoo_Aux" (BG.Ptr Foo_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFoo_Aux")

instance HasCField.HasCField Foo_Aux "unwrapFoo_Aux" where

  type CFieldType Foo_Aux "unwrapFoo_Aux" =
    IO Foo_struct

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @foo@

    __defined at:__ @program-analysis\/typedef_analysis.h 167:22@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Foo = Foo
  { unwrapFoo :: BG.FunPtr Foo_Aux
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
         ) => BG.CompatHasField.HasField "unwrapFoo" Foo ty where

  hasField =
    \x0 ->
      (\y1 ->
         Foo {unwrapFoo = y1}, BG.getField @"unwrapFoo" x0)

instance ( ty ~ BG.FunPtr Foo_Aux
         ) => BG.HasField "unwrapFoo" (BG.Ptr Foo) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapFoo")

instance HasCField.HasCField Foo "unwrapFoo" where

  type CFieldType Foo "unwrapFoo" = BG.FunPtr Foo_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct bar@

    __defined at:__ @program-analysis\/typedef_analysis.h 171:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Bar_struct = Bar_struct
  { bar_struct_a :: BG.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @program-analysis\/typedef_analysis.h 172:7@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , bar_struct_b :: BG.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @program-analysis\/typedef_analysis.h 173:7@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Bar_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Bar_struct where

  readRaw =
    \ptr0 ->
          pure Bar_struct
      <*> HasCField.readRaw (BG.Proxy @"bar_struct_a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"bar_struct_b") ptr0

instance Marshal.WriteRaw Bar_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar_struct bar_struct_a2 bar_struct_b3 ->
               HasCField.writeRaw (BG.Proxy @"bar_struct_a") ptr0 bar_struct_a2
            >> HasCField.writeRaw (BG.Proxy @"bar_struct_b") ptr0 bar_struct_b3

deriving via Marshal.EquivStorable Bar_struct instance BG.Storable Bar_struct

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "bar_struct_a" Bar_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar_struct {bar_struct_a = y1, bar_struct_b = BG.getField @"bar_struct_b" x0}
      , BG.getField @"bar_struct_a" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "bar_struct_a" (BG.Ptr Bar_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"bar_struct_a")

instance HasCField.HasCField Bar_struct "bar_struct_a" where

  type CFieldType Bar_struct "bar_struct_a" = BG.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "bar_struct_b" Bar_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar_struct {bar_struct_b = y1, bar_struct_a = BG.getField @"bar_struct_a" x0}
      , BG.getField @"bar_struct_b" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "bar_struct_b" (BG.Ptr Bar_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"bar_struct_b")

instance HasCField.HasCField Bar_struct "bar_struct_b" where

  type CFieldType Bar_struct "bar_struct_b" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| Auxiliary type used by 'Bar'

    __C declaration:__ @bar@

    __defined at:__ @program-analysis\/typedef_analysis.h 175:22@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Bar_Aux = Bar_Aux
  { unwrapBar_Aux :: Bar_struct -> IO Foo_struct
  }
  deriving stock (BG.Generic)

instance ( ty ~ (Bar_struct -> IO Foo_struct)
         ) => BG.CompatHasField.HasField "unwrapBar_Aux" Bar_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         Bar_Aux {unwrapBar_Aux = y1}, BG.getField @"unwrapBar_Aux" x0)

instance ( ty ~ (Bar_struct -> IO Foo_struct)
         ) => BG.HasField "unwrapBar_Aux" (BG.Ptr Bar_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapBar_Aux")

instance HasCField.HasCField Bar_Aux "unwrapBar_Aux" where

  type CFieldType Bar_Aux "unwrapBar_Aux" =
    Bar_struct -> IO Foo_struct

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @bar@

    __defined at:__ @program-analysis\/typedef_analysis.h 175:22@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Bar = Bar
  { unwrapBar :: BG.FunPtr Bar_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Bar_Aux
         ) => BG.CompatHasField.HasField "unwrapBar" Bar ty where

  hasField =
    \x0 ->
      (\y1 ->
         Bar {unwrapBar = y1}, BG.getField @"unwrapBar" x0)

instance ( ty ~ BG.FunPtr Bar_Aux
         ) => BG.HasField "unwrapBar" (BG.Ptr Bar) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapBar")

instance HasCField.HasCField Bar "unwrapBar" where

  type CFieldType Bar "unwrapBar" = BG.FunPtr Bar_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct15@

    __defined at:__ @program-analysis\/typedef_analysis.h 182:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct15 = Struct15
  {}
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Struct15 where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Struct15 where

  readRaw = \ptr0 -> pure Struct15

instance Marshal.WriteRaw Struct15 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct15 -> return ()

deriving via Marshal.EquivStorable Struct15 instance BG.Storable Struct15

{-| __C declaration:__ @struct struct16@

    __defined at:__ @program-analysis\/typedef_analysis.h 191:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Struct16_struct = Struct16_struct
  {}
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Struct16_struct where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Struct16_struct where

  readRaw = \ptr0 -> pure Struct16_struct

instance Marshal.WriteRaw Struct16_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct16_struct -> return ()

deriving via Marshal.EquivStorable Struct16_struct instance BG.Storable Struct16_struct

{-| __C declaration:__ @struct16@

    __defined at:__ @program-analysis\/typedef_analysis.h 192:32@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
newtype Struct16 = Struct16
  { unwrapStruct16 :: BG.Ptr Struct16_struct
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr Struct16_struct
         ) => BG.CompatHasField.HasField "unwrapStruct16" Struct16 ty where

  hasField =
    \x0 ->
      (\y1 ->
         Struct16 {unwrapStruct16 = y1}, BG.getField @"unwrapStruct16" x0)

instance ( ty ~ BG.Ptr Struct16_struct
         ) => BG.HasField "unwrapStruct16" (BG.Ptr Struct16) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapStruct16")

instance HasCField.HasCField Struct16 "unwrapStruct16" where

  type CFieldType Struct16 "unwrapStruct16" =
    BG.Ptr Struct16_struct

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct use_sites_qual@

    __defined at:__ @program-analysis\/typedef_analysis.h 195:8@

    __exported by:__ @program-analysis\/typedef_analysis.h@
-}
data Use_sites_qual = Use_sites_qual
  { use_sites_qual_useTypedef_struct15 :: Struct15
    {- ^ __C declaration:__ @useTypedef_struct15@

         __defined at:__ @program-analysis\/typedef_analysis.h 197:12@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_qual_useStruct_struct16 :: Struct16_struct
    {- ^ __C declaration:__ @useStruct_struct16@

         __defined at:__ @program-analysis\/typedef_analysis.h 200:19@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  , use_sites_qual_useTypedef_struct16 :: Struct16
    {- ^ __C declaration:__ @useTypedef_struct16@

         __defined at:__ @program-analysis\/typedef_analysis.h 201:12@

         __exported by:__ @program-analysis\/typedef_analysis.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Use_sites_qual where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Use_sites_qual where

  readRaw =
    \ptr0 ->
          pure Use_sites_qual
      <*> HasCField.readRaw (BG.Proxy @"use_sites_qual_useTypedef_struct15") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_qual_useStruct_struct16") ptr0
      <*> HasCField.readRaw (BG.Proxy @"use_sites_qual_useTypedef_struct16") ptr0

instance Marshal.WriteRaw Use_sites_qual where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Use_sites_qual
            use_sites_qual_useTypedef_struct152
            use_sites_qual_useStruct_struct163
            use_sites_qual_useTypedef_struct164 ->
                 HasCField.writeRaw (BG.Proxy @"use_sites_qual_useTypedef_struct15") ptr0 use_sites_qual_useTypedef_struct152
              >> HasCField.writeRaw (BG.Proxy @"use_sites_qual_useStruct_struct16") ptr0 use_sites_qual_useStruct_struct163
              >> HasCField.writeRaw (BG.Proxy @"use_sites_qual_useTypedef_struct16") ptr0 use_sites_qual_useTypedef_struct164

deriving via Marshal.EquivStorable Use_sites_qual instance BG.Storable Use_sites_qual

instance ( ty ~ Struct15
         ) => BG.CompatHasField.HasField "use_sites_qual_useTypedef_struct15" Use_sites_qual ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites_qual { use_sites_qual_useTypedef_struct15 = y1
                         , use_sites_qual_useStruct_struct16 = BG.getField @"use_sites_qual_useStruct_struct16" x0
                         , use_sites_qual_useTypedef_struct16 = BG.getField @"use_sites_qual_useTypedef_struct16" x0
                         }
      , BG.getField @"use_sites_qual_useTypedef_struct15" x0
      )

instance ( ty ~ Struct15
         ) => BG.HasField "use_sites_qual_useTypedef_struct15" (BG.Ptr Use_sites_qual) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_qual_useTypedef_struct15")

instance HasCField.HasCField Use_sites_qual "use_sites_qual_useTypedef_struct15" where

  type CFieldType Use_sites_qual "use_sites_qual_useTypedef_struct15" =
    Struct15

  offset# = \_ -> \_ -> 0

instance ( ty ~ Struct16_struct
         ) => BG.CompatHasField.HasField "use_sites_qual_useStruct_struct16" Use_sites_qual ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites_qual { use_sites_qual_useStruct_struct16 = y1
                         , use_sites_qual_useTypedef_struct15 = BG.getField @"use_sites_qual_useTypedef_struct15" x0
                         , use_sites_qual_useTypedef_struct16 = BG.getField @"use_sites_qual_useTypedef_struct16" x0
                         }
      , BG.getField @"use_sites_qual_useStruct_struct16" x0
      )

instance ( ty ~ Struct16_struct
         ) => BG.HasField "use_sites_qual_useStruct_struct16" (BG.Ptr Use_sites_qual) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_qual_useStruct_struct16")

instance HasCField.HasCField Use_sites_qual "use_sites_qual_useStruct_struct16" where

  type CFieldType Use_sites_qual "use_sites_qual_useStruct_struct16" =
    Struct16_struct

  offset# = \_ -> \_ -> 0

instance ( ty ~ Struct16
         ) => BG.CompatHasField.HasField "use_sites_qual_useTypedef_struct16" Use_sites_qual ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Use_sites_qual { use_sites_qual_useTypedef_struct16 = y1
                         , use_sites_qual_useTypedef_struct15 = BG.getField @"use_sites_qual_useTypedef_struct15" x0
                         , use_sites_qual_useStruct_struct16 = BG.getField @"use_sites_qual_useStruct_struct16" x0
                         }
      , BG.getField @"use_sites_qual_useTypedef_struct16" x0
      )

instance ( ty ~ Struct16
         ) => BG.HasField "use_sites_qual_useTypedef_struct16" (BG.Ptr Use_sites_qual) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"use_sites_qual_useTypedef_struct16")

instance HasCField.HasCField Use_sites_qual "use_sites_qual_useTypedef_struct16" where

  type CFieldType Use_sites_qual "use_sites_qual_useTypedef_struct16" =
    Struct16

  offset# = \_ -> \_ -> 0
