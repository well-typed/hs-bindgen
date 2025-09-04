{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as F
import Prelude ((<*>), (>>), Eq, Int, Ord, Show, pure, return)

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

{-| __C declaration:__ @struct2_t@

    __defined at:__ @typedef_analysis.h:11:16@

    __exported by:__ @typedef_analysis.h@
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

{-| __C declaration:__ @struct3_t@

    __defined at:__ @typedef_analysis.h:14:8@

    __exported by:__ @typedef_analysis.h@
-}
data Struct3_t

{-| __C declaration:__ @struct4_t@

    __defined at:__ @typedef_analysis.h:18:16@

    __exported by:__ @typedef_analysis.h@
-}
data Struct4_t

{-| __C declaration:__ @struct5@

    __defined at:__ @typedef_analysis.h:21:8@

    __exported by:__ @typedef_analysis.h@
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

{-| __C declaration:__ @struct5_t@

    __defined at:__ @typedef_analysis.h:22:25@

    __exported by:__ @typedef_analysis.h@
-}
newtype Struct5_t = Struct5_t
  { un_Struct5_t :: F.Ptr Struct5
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @struct6_Deref@

    __defined at:__ @typedef_analysis.h:25:16@

    __exported by:__ @typedef_analysis.h@
-}
data Struct6_Deref = Struct6_Deref
  {}
  deriving stock (Eq, Show)

instance F.Storable Struct6_Deref where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Struct6_Deref

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct6_Deref -> return ()

{-| __C declaration:__ @struct6@

    __defined at:__ @typedef_analysis.h:25:28@

    __exported by:__ @typedef_analysis.h@
-}
newtype Struct6 = Struct6
  { un_Struct6 :: F.Ptr Struct6_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @struct7@

    __defined at:__ @typedef_analysis.h:28:8@

    __exported by:__ @typedef_analysis.h@
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

{-| __C declaration:__ @struct7a@

    __defined at:__ @typedef_analysis.h:29:24@

    __exported by:__ @typedef_analysis.h@
-}
newtype Struct7a = Struct7a
  { un_Struct7a :: Struct7
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @struct7b@

    __defined at:__ @typedef_analysis.h:30:24@

    __exported by:__ @typedef_analysis.h@
-}
newtype Struct7b = Struct7b
  { un_Struct7b :: Struct7
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @struct8@

    __defined at:__ @typedef_analysis.h:33:8@

    __exported by:__ @typedef_analysis.h@
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

{-| __C declaration:__ @struct8b@

    __defined at:__ @typedef_analysis.h:35:24@

    __exported by:__ @typedef_analysis.h@
-}
newtype Struct8b = Struct8b
  { un_Struct8b :: Struct8
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @struct9@

    __defined at:__ @typedef_analysis.h:38:8@

    __exported by:__ @typedef_analysis.h@
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

{-| __C declaration:__ @struct9_t@

    __defined at:__ @typedef_analysis.h:40:17@

    __exported by:__ @typedef_analysis.h@
-}
newtype Struct9_t = Struct9_t
  { un_Struct9_t :: Struct9
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @struct10_t@

    __defined at:__ @typedef_analysis.h:46:8@

    __exported by:__ @typedef_analysis.h@
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

{-| __C declaration:__ @struct10_t_t@

    __defined at:__ @typedef_analysis.h:48:20@

    __exported by:__ @typedef_analysis.h@
-}
newtype Struct10_t_t = Struct10_t_t
  { un_Struct10_t_t :: Struct10_t
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @struct11_t@

    __defined at:__ @typedef_analysis.h:51:8@

    __exported by:__ @typedef_analysis.h@
-}
data Struct11_t = Struct11_t
  { struct11_t_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @typedef_analysis.h:52:7@

         __exported by:__ @typedef_analysis.h@
    -}
  , struct11_t_self :: F.Ptr Struct11_t
    {- ^ __C declaration:__ @self@

         __defined at:__ @typedef_analysis.h:53:20@

         __exported by:__ @typedef_analysis.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct11_t where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Struct11_t
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct11_t struct11_t_x2 struct11_t_self3 ->
               F.pokeByteOff ptr0 (0 :: Int) struct11_t_x2
            >> F.pokeByteOff ptr0 (8 :: Int) struct11_t_self3

{-| __C declaration:__ @struct12_t@

    __defined at:__ @typedef_analysis.h:60:8@

    __exported by:__ @typedef_analysis.h@
-}
data Struct12_t = Struct12_t
  { struct12_t_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @typedef_analysis.h:61:7@

         __exported by:__ @typedef_analysis.h@
    -}
  , struct12_t_self :: F.Ptr Struct12_t
    {- ^ __C declaration:__ @self@

         __defined at:__ @typedef_analysis.h:62:15@

         __exported by:__ @typedef_analysis.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct12_t where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Struct12_t
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct12_t struct12_t_x2 struct12_t_self3 ->
               F.pokeByteOff ptr0 (0 :: Int) struct12_t_x2
            >> F.pokeByteOff ptr0 (8 :: Int) struct12_t_self3

{-| __C declaration:__ @use_sites@

    __defined at:__ @typedef_analysis.h:66:8@

    __exported by:__ @typedef_analysis.h@
-}
data Use_sites = Use_sites
  { use_sites_useTypedef_struct1_t :: Struct1_t
    {- ^ __C declaration:__ @useTypedef_struct1_t@

         __defined at:__ @typedef_analysis.h:68:13@

         __exported by:__ @typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct2_t :: Struct2_t
    {- ^ __C declaration:__ @useTypedef_struct2_t@

         __defined at:__ @typedef_analysis.h:71:13@

         __exported by:__ @typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct3_t :: F.Ptr Struct3_t
    {- ^ __C declaration:__ @useTypedef_struct3_t@

         __defined at:__ @typedef_analysis.h:74:14@

         __exported by:__ @typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct4_t :: F.Ptr Struct4_t
    {- ^ __C declaration:__ @useTypedef_struct4_t@

         __defined at:__ @typedef_analysis.h:75:14@

         __exported by:__ @typedef_analysis.h@
    -}
  , use_sites_useStruct_struct5 :: Struct5
    {- ^ __C declaration:__ @useStruct_struct5@

         __defined at:__ @typedef_analysis.h:78:18@

         __exported by:__ @typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct5_t :: Struct5_t
    {- ^ __C declaration:__ @useTypedef_struct5_t@

         __defined at:__ @typedef_analysis.h:79:13@

         __exported by:__ @typedef_analysis.h@
    -}
  , use_sites_useStruct_struct6 :: Struct6_Deref
    {- ^ __C declaration:__ @useStruct_struct6@

         __defined at:__ @typedef_analysis.h:82:18@

         __exported by:__ @typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct6 :: Struct6
    {- ^ __C declaration:__ @useTypedef_struct6@

         __defined at:__ @typedef_analysis.h:83:11@

         __exported by:__ @typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct7a :: Struct7a
    {- ^ __C declaration:__ @useTypedef_struct7a@

         __defined at:__ @typedef_analysis.h:86:12@

         __exported by:__ @typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct7b :: Struct7b
    {- ^ __C declaration:__ @useTypedef_struct7b@

         __defined at:__ @typedef_analysis.h:87:12@

         __exported by:__ @typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct8 :: Struct8
    {- ^ __C declaration:__ @useTypedef_struct8@

         __defined at:__ @typedef_analysis.h:91:11@

         __exported by:__ @typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct8b :: Struct8b
    {- ^ __C declaration:__ @useTypedef_struct8b@

         __defined at:__ @typedef_analysis.h:92:12@

         __exported by:__ @typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct9 :: Struct9
    {- ^ __C declaration:__ @useTypedef_struct9@

         __defined at:__ @typedef_analysis.h:96:11@

         __exported by:__ @typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct9_t :: Struct9_t
    {- ^ __C declaration:__ @useTypedef_struct9_t@

         __defined at:__ @typedef_analysis.h:97:13@

         __exported by:__ @typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct10_t :: Struct10_t
    {- ^ __C declaration:__ @useTypedef_struct10_t@

         __defined at:__ @typedef_analysis.h:98:14@

         __exported by:__ @typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct10_t_t :: Struct10_t_t
    {- ^ __C declaration:__ @useTypedef_struct10_t_t@

         __defined at:__ @typedef_analysis.h:99:16@

         __exported by:__ @typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct11_t :: Struct11_t
    {- ^ __C declaration:__ @useTypedef_struct11_t@

         __defined at:__ @typedef_analysis.h:102:14@

         __exported by:__ @typedef_analysis.h@
    -}
  , use_sites_useTypedef_struct12_t :: Struct12_t
    {- ^ __C declaration:__ @useTypedef_struct12_t@

         __defined at:__ @typedef_analysis.h:103:14@

         __exported by:__ @typedef_analysis.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Use_sites where

  sizeOf = \_ -> (64 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Use_sites
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)
      <*> F.peekByteOff ptr0 (24 :: Int)
      <*> F.peekByteOff ptr0 (24 :: Int)
      <*> F.peekByteOff ptr0 (32 :: Int)
      <*> F.peekByteOff ptr0 (32 :: Int)
      <*> F.peekByteOff ptr0 (32 :: Int)
      <*> F.peekByteOff ptr0 (32 :: Int)
      <*> F.peekByteOff ptr0 (32 :: Int)
      <*> F.peekByteOff ptr0 (32 :: Int)
      <*> F.peekByteOff ptr0 (32 :: Int)
      <*> F.peekByteOff ptr0 (32 :: Int)
      <*> F.peekByteOff ptr0 (32 :: Int)
      <*> F.peekByteOff ptr0 (48 :: Int)

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
                 F.pokeByteOff ptr0 (0 :: Int) use_sites_useTypedef_struct1_t2
              >> F.pokeByteOff ptr0 (0 :: Int) use_sites_useTypedef_struct2_t3
              >> F.pokeByteOff ptr0 (0 :: Int) use_sites_useTypedef_struct3_t4
              >> F.pokeByteOff ptr0 (8 :: Int) use_sites_useTypedef_struct4_t5
              >> F.pokeByteOff ptr0 (16 :: Int) use_sites_useStruct_struct56
              >> F.pokeByteOff ptr0 (16 :: Int) use_sites_useTypedef_struct5_t7
              >> F.pokeByteOff ptr0 (24 :: Int) use_sites_useStruct_struct68
              >> F.pokeByteOff ptr0 (24 :: Int) use_sites_useTypedef_struct69
              >> F.pokeByteOff ptr0 (32 :: Int) use_sites_useTypedef_struct7a10
              >> F.pokeByteOff ptr0 (32 :: Int) use_sites_useTypedef_struct7b11
              >> F.pokeByteOff ptr0 (32 :: Int) use_sites_useTypedef_struct812
              >> F.pokeByteOff ptr0 (32 :: Int) use_sites_useTypedef_struct8b13
              >> F.pokeByteOff ptr0 (32 :: Int) use_sites_useTypedef_struct914
              >> F.pokeByteOff ptr0 (32 :: Int) use_sites_useTypedef_struct9_t15
              >> F.pokeByteOff ptr0 (32 :: Int) use_sites_useTypedef_struct10_t16
              >> F.pokeByteOff ptr0 (32 :: Int) use_sites_useTypedef_struct10_t_t17
              >> F.pokeByteOff ptr0 (32 :: Int) use_sites_useTypedef_struct11_t18
              >> F.pokeByteOff ptr0 (48 :: Int) use_sites_useTypedef_struct12_t19
