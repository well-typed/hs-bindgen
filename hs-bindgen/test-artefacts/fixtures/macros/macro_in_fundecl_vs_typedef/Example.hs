{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.MC(..)
    , Example.TC(..)
    , Example.Struct1(..)
    , Example.Struct2(..)
    , Example.Struct3(..)
    , Example.Struct3_t(..)
    , Example.Struct4(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro MC@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 4:9@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
newtype MC = MC
  { unwrapMC :: BG.CChar
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

instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "unwrapMC" MC ty where

  hasField =
    \x0 ->
      (\y1 ->
         MC {unwrapMC = y1}, BG.getField @"unwrapMC" x0)

instance (ty ~ BG.CChar) => BG.HasField "unwrapMC" (BG.Ptr MC) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapMC")

instance HasCField.HasCField MC "unwrapMC" where

  type CFieldType MC "unwrapMC" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TC@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 5:14@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
newtype TC = TC
  { unwrapTC :: BG.CChar
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

instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "unwrapTC" TC ty where

  hasField =
    \x0 ->
      (\y1 ->
         TC {unwrapTC = y1}, BG.getField @"unwrapTC" x0)

instance (ty ~ BG.CChar) => BG.HasField "unwrapTC" (BG.Ptr TC) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapTC")

instance HasCField.HasCField TC "unwrapTC" where

  type CFieldType TC "unwrapTC" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 18:16@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
data Struct1 = Struct1
  { struct1_a :: BG.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 18:30@

         __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Struct1 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Struct1 where

  readRaw =
    \ptr0 ->
          pure Struct1
      <*> HasCField.readRaw (BG.Proxy @"struct1_a") ptr0

instance Marshal.WriteRaw Struct1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct1 struct1_a2 ->
            HasCField.writeRaw (BG.Proxy @"struct1_a") ptr0 struct1_a2

deriving via Marshal.EquivStorable Struct1 instance BG.Storable Struct1

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "struct1_a" Struct1 ty where

  hasField =
    \x0 ->
      (\y1 ->
         Struct1 {struct1_a = y1}, BG.getField @"struct1_a" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "struct1_a" (BG.Ptr Struct1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"struct1_a")

instance HasCField.HasCField Struct1 "struct1_a" where

  type CFieldType Struct1 "struct1_a" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 19:9@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
data Struct2 = Struct2
  { struct2_a :: BG.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 19:30@

         __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Struct2 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Struct2 where

  readRaw =
    \ptr0 ->
          pure Struct2
      <*> HasCField.readRaw (BG.Proxy @"struct2_a") ptr0

instance Marshal.WriteRaw Struct2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2 struct2_a2 ->
            HasCField.writeRaw (BG.Proxy @"struct2_a") ptr0 struct2_a2

deriving via Marshal.EquivStorable Struct2 instance BG.Storable Struct2

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "struct2_a" Struct2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         Struct2 {struct2_a = y1}, BG.getField @"struct2_a" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "struct2_a" (BG.Ptr Struct2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"struct2_a")

instance HasCField.HasCField Struct2 "struct2_a" where

  type CFieldType Struct2 "struct2_a" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 20:16@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
data Struct3 = Struct3
  { struct3_a :: BG.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 20:30@

         __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Struct3 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Struct3 where

  readRaw =
    \ptr0 ->
          pure Struct3
      <*> HasCField.readRaw (BG.Proxy @"struct3_a") ptr0

instance Marshal.WriteRaw Struct3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct3 struct3_a2 ->
            HasCField.writeRaw (BG.Proxy @"struct3_a") ptr0 struct3_a2

deriving via Marshal.EquivStorable Struct3 instance BG.Storable Struct3

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "struct3_a" Struct3 ty where

  hasField =
    \x0 ->
      (\y1 ->
         Struct3 {struct3_a = y1}, BG.getField @"struct3_a" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "struct3_a" (BG.Ptr Struct3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"struct3_a")

instance HasCField.HasCField Struct3 "struct3_a" where

  type CFieldType Struct3 "struct3_a" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct3_t@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 20:35@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
newtype Struct3_t = Struct3_t
  { unwrapStruct3_t :: Struct3
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ Struct3
         ) => BG.CompatHasField.HasField "unwrapStruct3_t" Struct3_t ty where

  hasField =
    \x0 ->
      (\y1 ->
         Struct3_t {unwrapStruct3_t = y1}, BG.getField @"unwrapStruct3_t" x0)

instance ( ty ~ Struct3
         ) => BG.HasField "unwrapStruct3_t" (BG.Ptr Struct3_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapStruct3_t")

instance HasCField.HasCField Struct3_t "unwrapStruct3_t" where

  type CFieldType Struct3_t "unwrapStruct3_t" = Struct3

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct4@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 21:16@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
data Struct4 = Struct4
  { struct4_a :: BG.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 21:30@

         __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Struct4 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Struct4 where

  readRaw =
    \ptr0 ->
          pure Struct4
      <*> HasCField.readRaw (BG.Proxy @"struct4_a") ptr0

instance Marshal.WriteRaw Struct4 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct4 struct4_a2 ->
            HasCField.writeRaw (BG.Proxy @"struct4_a") ptr0 struct4_a2

deriving via Marshal.EquivStorable Struct4 instance BG.Storable Struct4

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "struct4_a" Struct4 ty where

  hasField =
    \x0 ->
      (\y1 ->
         Struct4 {struct4_a = y1}, BG.getField @"struct4_a" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "struct4_a" (BG.Ptr Struct4) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"struct4_a")

instance HasCField.HasCField Struct4 "struct4_a" where

  type CFieldType Struct4 "struct4_a" = BG.CInt

  offset# = \_ -> \_ -> 0
