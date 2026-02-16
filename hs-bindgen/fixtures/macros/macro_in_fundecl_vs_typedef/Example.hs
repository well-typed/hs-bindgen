{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @MC@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 4:9@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
newtype MC = MC
  { unwrapMC :: RIP.CChar
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

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "unwrapMC" (RIP.Ptr MC) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapMC")

instance HasCField.HasCField MC "unwrapMC" where

  type CFieldType MC "unwrapMC" = RIP.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TC@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 5:14@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
newtype TC = TC
  { unwrapTC :: RIP.CChar
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

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "unwrapTC" (RIP.Ptr TC) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapTC")

instance HasCField.HasCField TC "unwrapTC" where

  type CFieldType TC "unwrapTC" = RIP.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 18:16@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
data Struct1 = Struct1
  { struct1_a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 18:30@

         __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct1 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Struct1 where

  readRaw =
    \ptr0 ->
          pure Struct1
      <*> HasCField.readRaw (RIP.Proxy @"struct1_a") ptr0

instance Marshal.WriteRaw Struct1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct1 struct1_a2 ->
            HasCField.writeRaw (RIP.Proxy @"struct1_a") ptr0 struct1_a2

deriving via Marshal.EquivStorable Struct1 instance RIP.Storable Struct1

instance HasCField.HasCField Struct1 "struct1_a" where

  type CFieldType Struct1 "struct1_a" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "struct1_a" (RIP.Ptr Struct1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"struct1_a")

{-| __C declaration:__ @struct struct2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 19:9@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
data Struct2 = Struct2
  { struct2_a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 19:30@

         __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct2 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Struct2 where

  readRaw =
    \ptr0 ->
          pure Struct2
      <*> HasCField.readRaw (RIP.Proxy @"struct2_a") ptr0

instance Marshal.WriteRaw Struct2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2 struct2_a2 ->
            HasCField.writeRaw (RIP.Proxy @"struct2_a") ptr0 struct2_a2

deriving via Marshal.EquivStorable Struct2 instance RIP.Storable Struct2

instance HasCField.HasCField Struct2 "struct2_a" where

  type CFieldType Struct2 "struct2_a" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "struct2_a" (RIP.Ptr Struct2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"struct2_a")

{-| __C declaration:__ @struct struct3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 20:16@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
data Struct3 = Struct3
  { struct3_a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 20:30@

         __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct3 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Struct3 where

  readRaw =
    \ptr0 ->
          pure Struct3
      <*> HasCField.readRaw (RIP.Proxy @"struct3_a") ptr0

instance Marshal.WriteRaw Struct3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct3 struct3_a2 ->
            HasCField.writeRaw (RIP.Proxy @"struct3_a") ptr0 struct3_a2

deriving via Marshal.EquivStorable Struct3 instance RIP.Storable Struct3

instance HasCField.HasCField Struct3 "struct3_a" where

  type CFieldType Struct3 "struct3_a" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "struct3_a" (RIP.Ptr Struct3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"struct3_a")

{-| __C declaration:__ @struct3_t@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 20:35@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
newtype Struct3_t = Struct3_t
  { unwrapStruct3_t :: Struct3
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) Struct3
         ) => RIP.HasField "unwrapStruct3_t" (RIP.Ptr Struct3_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStruct3_t")

instance HasCField.HasCField Struct3_t "unwrapStruct3_t" where

  type CFieldType Struct3_t "unwrapStruct3_t" = Struct3

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct4@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 21:16@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
data Struct4 = Struct4
  { struct4_a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 21:30@

         __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct4 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Struct4 where

  readRaw =
    \ptr0 ->
          pure Struct4
      <*> HasCField.readRaw (RIP.Proxy @"struct4_a") ptr0

instance Marshal.WriteRaw Struct4 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct4 struct4_a2 ->
            HasCField.writeRaw (RIP.Proxy @"struct4_a") ptr0 struct4_a2

deriving via Marshal.EquivStorable Struct4 instance RIP.Storable Struct4

instance HasCField.HasCField Struct4 "struct4_a" where

  type CFieldType Struct4 "struct4_a" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "struct4_a" (RIP.Ptr Struct4) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"struct4_a")
