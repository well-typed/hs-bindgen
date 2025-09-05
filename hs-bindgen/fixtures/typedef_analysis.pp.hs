{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as F
import Prelude ((<*>), (>>), Eq, Int, Ord, Show, pure, return)

{-| Examples for the various cases in by `HsBindgen.Frontend.Analysis.Typedefs`

  __from C:__ @struct1@
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

data Struct3_t

data Struct4_t

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

newtype Struct5_t = Struct5_t
  { un_Struct5_t :: F.Ptr Struct5
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

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

newtype Struct6 = Struct6
  { un_Struct6 :: F.Ptr Struct6_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

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

newtype Struct7a = Struct7a
  { un_Struct7a :: Struct7
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

newtype Struct7b = Struct7b
  { un_Struct7b :: Struct7
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

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

newtype Struct8b = Struct8b
  { un_Struct8b :: Struct8
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

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

newtype Struct9_t = Struct9_t
  { un_Struct9_t :: Struct9
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

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

newtype Struct10_t_t = Struct10_t_t
  { un_Struct10_t_t :: Struct10_t
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

data Struct11_t = Struct11_t
  { struct11_t_x :: FC.CInt
  , struct11_t_self :: F.Ptr Struct11_t
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

data Struct12_t = Struct12_t
  { struct12_t_x :: FC.CInt
  , struct12_t_self :: F.Ptr Struct12_t
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

data Use_sites = Use_sites
  { use_sites_useTypedef_struct1_t :: Struct1_t
  , use_sites_useTypedef_struct2_t :: Struct2_t
  , use_sites_useTypedef_struct3_t :: F.Ptr Struct3_t
  , use_sites_useTypedef_struct4_t :: F.Ptr Struct4_t
  , use_sites_useStruct_struct5 :: Struct5
  , use_sites_useTypedef_struct5_t :: Struct5_t
  , use_sites_useStruct_struct6 :: Struct6_Deref
  , use_sites_useTypedef_struct6 :: Struct6
  , use_sites_useTypedef_struct7a :: Struct7a
  , use_sites_useTypedef_struct7b :: Struct7b
  , use_sites_useTypedef_struct8 :: Struct8
  , use_sites_useTypedef_struct8b :: Struct8b
  , use_sites_useTypedef_struct9 :: Struct9
  , use_sites_useTypedef_struct9_t :: Struct9_t
  , use_sites_useTypedef_struct10_t :: Struct10_t
  , use_sites_useTypedef_struct10_t_t :: Struct10_t_t
  , use_sites_useTypedef_struct11_t :: Struct11_t
  , use_sites_useTypedef_struct12_t :: Struct12_t
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
