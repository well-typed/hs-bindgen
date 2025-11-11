{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import Data.Bits (FiniteBits)
import Prelude ((<*>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @MC@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:4:9@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
newtype MC = MC
  { un_MC :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @TC@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:5:14@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
newtype TC = TC
  { un_TC :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @struct1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:18:16@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
data Struct1 = Struct1
  { struct1_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macro_in_fundecl_vs_typedef.h:18:30@

         __exported by:__ @macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct1 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct1
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct1 struct1_a2 ->
            F.pokeByteOff ptr0 (0 :: Int) struct1_a2

{-| __defined at:__ @macro_in_fundecl_vs_typedef.h:19:9@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
data Struct2 = Struct2
  { struct2_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macro_in_fundecl_vs_typedef.h:19:30@

         __exported by:__ @macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct2 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct2
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2 struct2_a2 ->
            F.pokeByteOff ptr0 (0 :: Int) struct2_a2

{-| __C declaration:__ @struct3@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:20:16@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
data Struct3 = Struct3
  { struct3_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macro_in_fundecl_vs_typedef.h:20:30@

         __exported by:__ @macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct3 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct3
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct3 struct3_a2 ->
            F.pokeByteOff ptr0 (0 :: Int) struct3_a2

{-| __C declaration:__ @struct3_t@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:20:35@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
newtype Struct3_t = Struct3_t
  { un_Struct3_t :: Struct3
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @struct4@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:21:16@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
data Struct4 = Struct4
  { struct4_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macro_in_fundecl_vs_typedef.h:21:30@

         __exported by:__ @macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct4 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct4
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct4 struct4_a2 ->
            F.pokeByteOff ptr0 (0 :: Int) struct4_a2
