{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @MC@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:4:9@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
newtype MC = MC
  { un_MC :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @TC@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:5:14@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
newtype TC = TC
  { un_TC :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType TC) "un_TC")
         ) => GHC.Records.HasField "un_TC" (Ptr.Ptr TC) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_TC")

instance HsBindgen.Runtime.HasCField.HasCField TC "un_TC" where

  type CFieldType TC "un_TC" = FC.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:18:16@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
data Struct1 = Struct1
  { struct1_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:18:30@

         __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct1 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct1
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"struct1_a") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct1 struct1_a2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"struct1_a") ptr0 struct1_a2

instance HsBindgen.Runtime.HasCField.HasCField Struct1 "struct1_a" where

  type CFieldType Struct1 "struct1_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct1) "struct1_a")
         ) => GHC.Records.HasField "struct1_a" (Ptr.Ptr Struct1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"struct1_a")

{-| __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:19:9@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
data Struct2 = Struct2
  { struct2_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:19:30@

         __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct2 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct2
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"struct2_a") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2 struct2_a2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"struct2_a") ptr0 struct2_a2

instance HsBindgen.Runtime.HasCField.HasCField Struct2 "struct2_a" where

  type CFieldType Struct2 "struct2_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct2) "struct2_a")
         ) => GHC.Records.HasField "struct2_a" (Ptr.Ptr Struct2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"struct2_a")

{-| __C declaration:__ @struct3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:20:16@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
data Struct3 = Struct3
  { struct3_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:20:30@

         __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct3 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct3
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"struct3_a") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct3 struct3_a2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"struct3_a") ptr0 struct3_a2

instance HsBindgen.Runtime.HasCField.HasCField Struct3 "struct3_a" where

  type CFieldType Struct3 "struct3_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct3) "struct3_a")
         ) => GHC.Records.HasField "struct3_a" (Ptr.Ptr Struct3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"struct3_a")

{-| __C declaration:__ @struct3_t@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:20:35@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
newtype Struct3_t = Struct3_t
  { un_Struct3_t :: Struct3
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct3_t) "un_Struct3_t")
         ) => GHC.Records.HasField "un_Struct3_t" (Ptr.Ptr Struct3_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Struct3_t")

instance HsBindgen.Runtime.HasCField.HasCField Struct3_t "un_Struct3_t" where

  type CFieldType Struct3_t "un_Struct3_t" = Struct3

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct4@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:21:16@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
data Struct4 = Struct4
  { struct4_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:21:30@

         __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct4 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct4
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"struct4_a") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct4 struct4_a2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"struct4_a") ptr0 struct4_a2

instance HsBindgen.Runtime.HasCField.HasCField Struct4 "struct4_a" where

  type CFieldType Struct4 "struct4_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct4) "struct4_a")
         ) => GHC.Records.HasField "struct4_a" (Ptr.Ptr Struct4) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"struct4_a")
