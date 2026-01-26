{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
import qualified HsBindgen.Runtime.HasFFIType
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Ord, Show, pure, return)

{-| Examples for the various cases in by `HsBindgen.Frontend.Analysis.Typedefs`

__C declaration:__ @struct struct1@

__defined at:__ @types\/typedefs\/typedef_analysis.h 7:8@

__exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
data Struct1_t = Struct1_t
  {}
  deriving stock (Eq, Show)

instance F.Storable Struct1_t where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Struct1_t

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct1_t -> return ()

instance Data.Primitive.Types.Prim Struct1_t where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

  indexByteArray# = \arr0 -> \i1 -> Struct1_t

  readByteArray# =
    \arr0 -> \i1 -> \s2 -> (# s2, Struct1_t #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct1_t -> s3

  indexOffAddr# = \addr0 -> \i1 -> Struct1_t

  readOffAddr# =
    \addr0 -> \i1 -> \s2 -> (# s2, Struct1_t #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct1_t -> s3

{-| __C declaration:__ @struct struct2@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 11:16@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
data Struct2_t = Struct2_t
  {}
  deriving stock (Eq, Show)

instance F.Storable Struct2_t where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Struct2_t

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2_t -> return ()

instance Data.Primitive.Types.Prim Struct2_t where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

  indexByteArray# = \arr0 -> \i1 -> Struct2_t

  readByteArray# =
    \arr0 -> \i1 -> \s2 -> (# s2, Struct2_t #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct2_t -> s3

  indexOffAddr# = \addr0 -> \i1 -> Struct2_t

  readOffAddr# =
    \addr0 -> \i1 -> \s2 -> (# s2, Struct2_t #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct2_t -> s3

{-| __C declaration:__ @struct struct3@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 14:8@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
data Struct3_t

{-| __C declaration:__ @struct struct4@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 18:16@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
data Struct4_t

{-| __C declaration:__ @struct struct5@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 21:8@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
data Struct5 = Struct5
  {}
  deriving stock (Eq, Show)

instance F.Storable Struct5 where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Struct5

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct5 -> return ()

instance Data.Primitive.Types.Prim Struct5 where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

  indexByteArray# = \arr0 -> \i1 -> Struct5

  readByteArray# =
    \arr0 -> \i1 -> \s2 -> (# s2, Struct5 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct5 -> s3

  indexOffAddr# = \addr0 -> \i1 -> Struct5

  readOffAddr# =
    \addr0 -> \i1 -> \s2 -> (# s2, Struct5 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct5 -> s3

{-| __C declaration:__ @struct5_t@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 22:25@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
newtype Struct5_t = Struct5_t
  { un_Struct5_t :: Ptr.Ptr Struct5
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct5_t) "un_Struct5_t")
         ) => GHC.Records.HasField "un_Struct5_t" (Ptr.Ptr Struct5_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Struct5_t")

instance HsBindgen.Runtime.HasCField.HasCField Struct5_t "un_Struct5_t" where

  type CFieldType Struct5_t "un_Struct5_t" =
    Ptr.Ptr Struct5

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct6@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 25:16@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
data Struct6_Aux = Struct6_Aux
  {}
  deriving stock (Eq, Show)

instance F.Storable Struct6_Aux where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Struct6_Aux

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct6_Aux -> return ()

instance Data.Primitive.Types.Prim Struct6_Aux where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

  indexByteArray# = \arr0 -> \i1 -> Struct6_Aux

  readByteArray# =
    \arr0 -> \i1 -> \s2 -> (# s2, Struct6_Aux #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct6_Aux -> s3

  indexOffAddr# = \addr0 -> \i1 -> Struct6_Aux

  readOffAddr# =
    \addr0 -> \i1 -> \s2 -> (# s2, Struct6_Aux #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct6_Aux -> s3

{-| __C declaration:__ @struct6@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 25:28@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
newtype Struct6 = Struct6
  { un_Struct6 :: Ptr.Ptr Struct6_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct6) "un_Struct6")
         ) => GHC.Records.HasField "un_Struct6" (Ptr.Ptr Struct6) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Struct6")

instance HsBindgen.Runtime.HasCField.HasCField Struct6 "un_Struct6" where

  type CFieldType Struct6 "un_Struct6" =
    Ptr.Ptr Struct6_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct7@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 28:8@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
data Struct7 = Struct7
  {}
  deriving stock (Eq, Show)

instance F.Storable Struct7 where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Struct7

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct7 -> return ()

instance Data.Primitive.Types.Prim Struct7 where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

  indexByteArray# = \arr0 -> \i1 -> Struct7

  readByteArray# =
    \arr0 -> \i1 -> \s2 -> (# s2, Struct7 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct7 -> s3

  indexOffAddr# = \addr0 -> \i1 -> Struct7

  readOffAddr# =
    \addr0 -> \i1 -> \s2 -> (# s2, Struct7 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct7 -> s3

{-| __C declaration:__ @struct7a@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 29:24@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
newtype Struct7a = Struct7a
  { un_Struct7a :: Struct7
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable, Data.Primitive.Types.Prim)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct7a) "un_Struct7a")
         ) => GHC.Records.HasField "un_Struct7a" (Ptr.Ptr Struct7a) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Struct7a")

instance HsBindgen.Runtime.HasCField.HasCField Struct7a "un_Struct7a" where

  type CFieldType Struct7a "un_Struct7a" = Struct7

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct7b@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 30:24@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
newtype Struct7b = Struct7b
  { un_Struct7b :: Struct7
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable, Data.Primitive.Types.Prim)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct7b) "un_Struct7b")
         ) => GHC.Records.HasField "un_Struct7b" (Ptr.Ptr Struct7b) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Struct7b")

instance HsBindgen.Runtime.HasCField.HasCField Struct7b "un_Struct7b" where

  type CFieldType Struct7b "un_Struct7b" = Struct7

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct8@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 33:8@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
data Struct8 = Struct8
  {}
  deriving stock (Eq, Show)

instance F.Storable Struct8 where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Struct8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct8 -> return ()

instance Data.Primitive.Types.Prim Struct8 where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

  indexByteArray# = \arr0 -> \i1 -> Struct8

  readByteArray# =
    \arr0 -> \i1 -> \s2 -> (# s2, Struct8 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct8 -> s3

  indexOffAddr# = \addr0 -> \i1 -> Struct8

  readOffAddr# =
    \addr0 -> \i1 -> \s2 -> (# s2, Struct8 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct8 -> s3

{-| __C declaration:__ @struct8b@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 35:24@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
newtype Struct8b = Struct8b
  { un_Struct8b :: Struct8
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable, Data.Primitive.Types.Prim)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct8b) "un_Struct8b")
         ) => GHC.Records.HasField "un_Struct8b" (Ptr.Ptr Struct8b) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Struct8b")

instance HsBindgen.Runtime.HasCField.HasCField Struct8b "un_Struct8b" where

  type CFieldType Struct8b "un_Struct8b" = Struct8

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct9@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 38:8@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
data Struct9 = Struct9
  {}
  deriving stock (Eq, Show)

instance F.Storable Struct9 where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Struct9

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct9 -> return ()

instance Data.Primitive.Types.Prim Struct9 where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

  indexByteArray# = \arr0 -> \i1 -> Struct9

  readByteArray# =
    \arr0 -> \i1 -> \s2 -> (# s2, Struct9 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct9 -> s3

  indexOffAddr# = \addr0 -> \i1 -> Struct9

  readOffAddr# =
    \addr0 -> \i1 -> \s2 -> (# s2, Struct9 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct9 -> s3

{-| __C declaration:__ @struct9_t@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 40:17@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
newtype Struct9_t = Struct9_t
  { un_Struct9_t :: Struct9
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable, Data.Primitive.Types.Prim)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct9_t) "un_Struct9_t")
         ) => GHC.Records.HasField "un_Struct9_t" (Ptr.Ptr Struct9_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Struct9_t")

instance HsBindgen.Runtime.HasCField.HasCField Struct9_t "un_Struct9_t" where

  type CFieldType Struct9_t "un_Struct9_t" = Struct9

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct10@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 46:8@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
data Struct10_t = Struct10_t
  {}
  deriving stock (Eq, Show)

instance F.Storable Struct10_t where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Struct10_t

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct10_t -> return ()

instance Data.Primitive.Types.Prim Struct10_t where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

  indexByteArray# = \arr0 -> \i1 -> Struct10_t

  readByteArray# =
    \arr0 -> \i1 -> \s2 -> (# s2, Struct10_t #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct10_t -> s3

  indexOffAddr# = \addr0 -> \i1 -> Struct10_t

  readOffAddr# =
    \addr0 -> \i1 -> \s2 -> (# s2, Struct10_t #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct10_t -> s3

{-| __C declaration:__ @struct10_t_t@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 48:20@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
newtype Struct10_t_t = Struct10_t_t
  { un_Struct10_t_t :: Struct10_t
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable, Data.Primitive.Types.Prim)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct10_t_t) "un_Struct10_t_t")
         ) => GHC.Records.HasField "un_Struct10_t_t" (Ptr.Ptr Struct10_t_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Struct10_t_t")

instance HsBindgen.Runtime.HasCField.HasCField Struct10_t_t "un_Struct10_t_t" where

  type CFieldType Struct10_t_t "un_Struct10_t_t" =
    Struct10_t

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct11@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 51:8@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
data Struct11_t = Struct11_t
  { struct11_t_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 52:7@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , struct11_t_self :: Ptr.Ptr Struct11_t
    {- ^ __C declaration:__ @self@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 53:20@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct11_t where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Struct11_t
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"struct11_t_x") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"struct11_t_self") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct11_t struct11_t_x2 struct11_t_self3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"struct11_t_x") ptr0 struct11_t_x2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"struct11_t_self") ptr0 struct11_t_self3

instance HsBindgen.Runtime.HasCField.HasCField Struct11_t "struct11_t_x" where

  type CFieldType Struct11_t "struct11_t_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct11_t) "struct11_t_x")
         ) => GHC.Records.HasField "struct11_t_x" (Ptr.Ptr Struct11_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"struct11_t_x")

instance HsBindgen.Runtime.HasCField.HasCField Struct11_t "struct11_t_self" where

  type CFieldType Struct11_t "struct11_t_self" =
    Ptr.Ptr Struct11_t

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct11_t) "struct11_t_self")
         ) => GHC.Records.HasField "struct11_t_self" (Ptr.Ptr Struct11_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"struct11_t_self")

{-| __C declaration:__ @struct struct12@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 60:8@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
data Struct12_t = Struct12_t
  { struct12_t_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 61:7@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , struct12_t_self :: Ptr.Ptr Struct12_t
    {- ^ __C declaration:__ @self@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 62:15@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct12_t where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Struct12_t
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"struct12_t_x") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"struct12_t_self") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct12_t struct12_t_x2 struct12_t_self3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"struct12_t_x") ptr0 struct12_t_x2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"struct12_t_self") ptr0 struct12_t_self3

instance HsBindgen.Runtime.HasCField.HasCField Struct12_t "struct12_t_x" where

  type CFieldType Struct12_t "struct12_t_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct12_t) "struct12_t_x")
         ) => GHC.Records.HasField "struct12_t_x" (Ptr.Ptr Struct12_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"struct12_t_x")

instance HsBindgen.Runtime.HasCField.HasCField Struct12_t "struct12_t_self" where

  type CFieldType Struct12_t "struct12_t_self" =
    Ptr.Ptr Struct12_t

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct12_t) "struct12_t_self")
         ) => GHC.Records.HasField "struct12_t_self" (Ptr.Ptr Struct12_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"struct12_t_self")

{-| __C declaration:__ @struct use_sites@

    __defined at:__ @types\/typedefs\/typedef_analysis.h 66:8@

    __exported by:__ @types\/typedefs\/typedef_analysis.h@
-}
data Use_sites = Use_sites
  { use_sites_useTypedef_struct1_t :: Struct1_t
    {- ^ __C declaration:__ @useTypedef_struct1_t@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 68:13@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct2_t :: Struct2_t
    {- ^ __C declaration:__ @useTypedef_struct2_t@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 71:13@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct3_t :: Ptr.Ptr Struct3_t
    {- ^ __C declaration:__ @useTypedef_struct3_t@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 74:14@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct4_t :: Ptr.Ptr Struct4_t
    {- ^ __C declaration:__ @useTypedef_struct4_t@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 75:14@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , use_sites_useStruct_struct5 :: Struct5
    {- ^ __C declaration:__ @useStruct_struct5@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 78:18@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct5_t :: Struct5_t
    {- ^ __C declaration:__ @useTypedef_struct5_t@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 79:13@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , use_sites_useStruct_struct6 :: Struct6_Aux
    {- ^ __C declaration:__ @useStruct_struct6@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 82:18@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct6 :: Struct6
    {- ^ __C declaration:__ @useTypedef_struct6@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 83:11@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct7a :: Struct7a
    {- ^ __C declaration:__ @useTypedef_struct7a@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 86:12@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct7b :: Struct7b
    {- ^ __C declaration:__ @useTypedef_struct7b@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 87:12@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct8 :: Struct8
    {- ^ __C declaration:__ @useTypedef_struct8@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 91:11@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct8b :: Struct8b
    {- ^ __C declaration:__ @useTypedef_struct8b@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 92:12@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct9 :: Struct9
    {- ^ __C declaration:__ @useTypedef_struct9@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 96:11@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct9_t :: Struct9_t
    {- ^ __C declaration:__ @useTypedef_struct9_t@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 97:13@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct10_t :: Struct10_t
    {- ^ __C declaration:__ @useTypedef_struct10_t@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 98:14@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct10_t_t :: Struct10_t_t
    {- ^ __C declaration:__ @useTypedef_struct10_t_t@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 99:16@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct11_t :: Struct11_t
    {- ^ __C declaration:__ @useTypedef_struct11_t@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 102:14@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct12_t :: Struct12_t
    {- ^ __C declaration:__ @useTypedef_struct12_t@

         __defined at:__ @types\/typedefs\/typedef_analysis.h 103:14@

         __exported by:__ @types\/typedefs\/typedef_analysis.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Use_sites where

  sizeOf = \_ -> (64 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Use_sites
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct1_t") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct2_t") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct3_t") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct4_t") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"use_sites_useStruct_struct5") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct5_t") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"use_sites_useStruct_struct6") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct6") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct7a") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct7b") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct8") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct8b") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct9") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct9_t") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct10_t") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct10_t_t") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct11_t") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct12_t") ptr0

  poke =
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
                 HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct1_t") ptr0 use_sites_useTypedef_struct1_t2
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct2_t") ptr0 use_sites_useTypedef_struct2_t3
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct3_t") ptr0 use_sites_useTypedef_struct3_t4
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct4_t") ptr0 use_sites_useTypedef_struct4_t5
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"use_sites_useStruct_struct5") ptr0 use_sites_useStruct_struct56
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct5_t") ptr0 use_sites_useTypedef_struct5_t7
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"use_sites_useStruct_struct6") ptr0 use_sites_useStruct_struct68
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct6") ptr0 use_sites_useTypedef_struct69
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct7a") ptr0 use_sites_useTypedef_struct7a10
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct7b") ptr0 use_sites_useTypedef_struct7b11
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct8") ptr0 use_sites_useTypedef_struct812
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct8b") ptr0 use_sites_useTypedef_struct8b13
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct9") ptr0 use_sites_useTypedef_struct914
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct9_t") ptr0 use_sites_useTypedef_struct9_t15
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct10_t") ptr0 use_sites_useTypedef_struct10_t16
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct10_t_t") ptr0 use_sites_useTypedef_struct10_t_t17
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct11_t") ptr0 use_sites_useTypedef_struct11_t18
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct12_t") ptr0 use_sites_useTypedef_struct12_t19

instance HsBindgen.Runtime.HasCField.HasCField Use_sites "use_sites_useTypedef_struct1_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct1_t" =
    Struct1_t

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Use_sites) "use_sites_useTypedef_struct1_t")
         ) => GHC.Records.HasField "use_sites_useTypedef_struct1_t" (Ptr.Ptr Use_sites) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct1_t")

instance HsBindgen.Runtime.HasCField.HasCField Use_sites "use_sites_useTypedef_struct2_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct2_t" =
    Struct2_t

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Use_sites) "use_sites_useTypedef_struct2_t")
         ) => GHC.Records.HasField "use_sites_useTypedef_struct2_t" (Ptr.Ptr Use_sites) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct2_t")

instance HsBindgen.Runtime.HasCField.HasCField Use_sites "use_sites_useTypedef_struct3_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct3_t" =
    Ptr.Ptr Struct3_t

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Use_sites) "use_sites_useTypedef_struct3_t")
         ) => GHC.Records.HasField "use_sites_useTypedef_struct3_t" (Ptr.Ptr Use_sites) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct3_t")

instance HsBindgen.Runtime.HasCField.HasCField Use_sites "use_sites_useTypedef_struct4_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct4_t" =
    Ptr.Ptr Struct4_t

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Use_sites) "use_sites_useTypedef_struct4_t")
         ) => GHC.Records.HasField "use_sites_useTypedef_struct4_t" (Ptr.Ptr Use_sites) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct4_t")

instance HsBindgen.Runtime.HasCField.HasCField Use_sites "use_sites_useStruct_struct5" where

  type CFieldType Use_sites "use_sites_useStruct_struct5" =
    Struct5

  offset# = \_ -> \_ -> 16

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Use_sites) "use_sites_useStruct_struct5")
         ) => GHC.Records.HasField "use_sites_useStruct_struct5" (Ptr.Ptr Use_sites) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"use_sites_useStruct_struct5")

instance HsBindgen.Runtime.HasCField.HasCField Use_sites "use_sites_useTypedef_struct5_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct5_t" =
    Struct5_t

  offset# = \_ -> \_ -> 16

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Use_sites) "use_sites_useTypedef_struct5_t")
         ) => GHC.Records.HasField "use_sites_useTypedef_struct5_t" (Ptr.Ptr Use_sites) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct5_t")

instance HsBindgen.Runtime.HasCField.HasCField Use_sites "use_sites_useStruct_struct6" where

  type CFieldType Use_sites "use_sites_useStruct_struct6" =
    Struct6_Aux

  offset# = \_ -> \_ -> 24

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Use_sites) "use_sites_useStruct_struct6")
         ) => GHC.Records.HasField "use_sites_useStruct_struct6" (Ptr.Ptr Use_sites) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"use_sites_useStruct_struct6")

instance HsBindgen.Runtime.HasCField.HasCField Use_sites "use_sites_useTypedef_struct6" where

  type CFieldType Use_sites "use_sites_useTypedef_struct6" =
    Struct6

  offset# = \_ -> \_ -> 24

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Use_sites) "use_sites_useTypedef_struct6")
         ) => GHC.Records.HasField "use_sites_useTypedef_struct6" (Ptr.Ptr Use_sites) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct6")

instance HsBindgen.Runtime.HasCField.HasCField Use_sites "use_sites_useTypedef_struct7a" where

  type CFieldType Use_sites "use_sites_useTypedef_struct7a" =
    Struct7a

  offset# = \_ -> \_ -> 32

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Use_sites) "use_sites_useTypedef_struct7a")
         ) => GHC.Records.HasField "use_sites_useTypedef_struct7a" (Ptr.Ptr Use_sites) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct7a")

instance HsBindgen.Runtime.HasCField.HasCField Use_sites "use_sites_useTypedef_struct7b" where

  type CFieldType Use_sites "use_sites_useTypedef_struct7b" =
    Struct7b

  offset# = \_ -> \_ -> 32

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Use_sites) "use_sites_useTypedef_struct7b")
         ) => GHC.Records.HasField "use_sites_useTypedef_struct7b" (Ptr.Ptr Use_sites) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct7b")

instance HsBindgen.Runtime.HasCField.HasCField Use_sites "use_sites_useTypedef_struct8" where

  type CFieldType Use_sites "use_sites_useTypedef_struct8" =
    Struct8

  offset# = \_ -> \_ -> 32

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Use_sites) "use_sites_useTypedef_struct8")
         ) => GHC.Records.HasField "use_sites_useTypedef_struct8" (Ptr.Ptr Use_sites) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct8")

instance HsBindgen.Runtime.HasCField.HasCField Use_sites "use_sites_useTypedef_struct8b" where

  type CFieldType Use_sites "use_sites_useTypedef_struct8b" =
    Struct8b

  offset# = \_ -> \_ -> 32

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Use_sites) "use_sites_useTypedef_struct8b")
         ) => GHC.Records.HasField "use_sites_useTypedef_struct8b" (Ptr.Ptr Use_sites) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct8b")

instance HsBindgen.Runtime.HasCField.HasCField Use_sites "use_sites_useTypedef_struct9" where

  type CFieldType Use_sites "use_sites_useTypedef_struct9" =
    Struct9

  offset# = \_ -> \_ -> 32

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Use_sites) "use_sites_useTypedef_struct9")
         ) => GHC.Records.HasField "use_sites_useTypedef_struct9" (Ptr.Ptr Use_sites) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct9")

instance HsBindgen.Runtime.HasCField.HasCField Use_sites "use_sites_useTypedef_struct9_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct9_t" =
    Struct9_t

  offset# = \_ -> \_ -> 32

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Use_sites) "use_sites_useTypedef_struct9_t")
         ) => GHC.Records.HasField "use_sites_useTypedef_struct9_t" (Ptr.Ptr Use_sites) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct9_t")

instance HsBindgen.Runtime.HasCField.HasCField Use_sites "use_sites_useTypedef_struct10_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct10_t" =
    Struct10_t

  offset# = \_ -> \_ -> 32

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Use_sites) "use_sites_useTypedef_struct10_t")
         ) => GHC.Records.HasField "use_sites_useTypedef_struct10_t" (Ptr.Ptr Use_sites) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct10_t")

instance HsBindgen.Runtime.HasCField.HasCField Use_sites "use_sites_useTypedef_struct10_t_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct10_t_t" =
    Struct10_t_t

  offset# = \_ -> \_ -> 32

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Use_sites) "use_sites_useTypedef_struct10_t_t")
         ) => GHC.Records.HasField "use_sites_useTypedef_struct10_t_t" (Ptr.Ptr Use_sites) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct10_t_t")

instance HsBindgen.Runtime.HasCField.HasCField Use_sites "use_sites_useTypedef_struct11_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct11_t" =
    Struct11_t

  offset# = \_ -> \_ -> 32

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Use_sites) "use_sites_useTypedef_struct11_t")
         ) => GHC.Records.HasField "use_sites_useTypedef_struct11_t" (Ptr.Ptr Use_sites) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct11_t")

instance HsBindgen.Runtime.HasCField.HasCField Use_sites "use_sites_useTypedef_struct12_t" where

  type CFieldType Use_sites "use_sites_useTypedef_struct12_t" =
    Struct12_t

  offset# = \_ -> \_ -> 48

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Use_sites) "use_sites_useTypedef_struct12_t")
         ) => GHC.Records.HasField "use_sites_useTypedef_struct12_t" (Ptr.Ptr Use_sites) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"use_sites_useTypedef_struct12_t")
