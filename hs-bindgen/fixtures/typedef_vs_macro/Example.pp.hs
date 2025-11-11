{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import Data.Bits (FiniteBits)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @T1@

    __defined at:__ @typedef_vs_macro.h:1:13@

    __exported by:__ @typedef_vs_macro.h@
-}
newtype T1 = T1
  { un_T1 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @T2@

    __defined at:__ @typedef_vs_macro.h:2:14@

    __exported by:__ @typedef_vs_macro.h@
-}
newtype T2 = T2
  { un_T2 :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @M1@

    __defined at:__ @typedef_vs_macro.h:4:9@

    __exported by:__ @typedef_vs_macro.h@
-}
newtype M1 = M1
  { un_M1 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @M2@

    __defined at:__ @typedef_vs_macro.h:5:9@

    __exported by:__ @typedef_vs_macro.h@
-}
newtype M2 = M2
  { un_M2 :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @M3@

    __defined at:__ @typedef_vs_macro.h:6:9@

    __exported by:__ @typedef_vs_macro.h@
-}
newtype M3 = M3
  { un_M3 :: Ptr.Ptr FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @ExampleStruct@

    __defined at:__ @typedef_vs_macro.h:8:8@

    __exported by:__ @typedef_vs_macro.h@
-}
data ExampleStruct = ExampleStruct
  { exampleStruct_t1 :: T1
    {- ^ __C declaration:__ @t1@

         __defined at:__ @typedef_vs_macro.h:9:6@

         __exported by:__ @typedef_vs_macro.h@
    -}
  , exampleStruct_t2 :: T2
    {- ^ __C declaration:__ @t2@

         __defined at:__ @typedef_vs_macro.h:10:6@

         __exported by:__ @typedef_vs_macro.h@
    -}
  , exampleStruct_m1 :: M1
    {- ^ __C declaration:__ @m1@

         __defined at:__ @typedef_vs_macro.h:11:6@

         __exported by:__ @typedef_vs_macro.h@
    -}
  , exampleStruct_m2 :: M2
    {- ^ __C declaration:__ @m2@

         __defined at:__ @typedef_vs_macro.h:12:6@

         __exported by:__ @typedef_vs_macro.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable ExampleStruct where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure ExampleStruct
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (12 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExampleStruct
            exampleStruct_t12
            exampleStruct_t23
            exampleStruct_m14
            exampleStruct_m25 ->
                 F.pokeByteOff ptr0 (0 :: Int) exampleStruct_t12
              >> F.pokeByteOff ptr0 (4 :: Int) exampleStruct_t23
              >> F.pokeByteOff ptr0 (8 :: Int) exampleStruct_m14
              >> F.pokeByteOff ptr0 (12 :: Int) exampleStruct_m25

{-| __C declaration:__ @uint64_t@

    __defined at:__ @typedef_vs_macro.h:15:9@

    __exported by:__ @typedef_vs_macro.h@
-}
newtype Uint64_t = Uint64_t
  { un_Uint64_t :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @foo@

    __defined at:__ @typedef_vs_macro.h:17:8@

    __exported by:__ @typedef_vs_macro.h@
-}
data Foo = Foo
  { foo_a :: Ptr.Ptr Uint64_t
    {- ^ __C declaration:__ @a@

         __defined at:__ @typedef_vs_macro.h:18:13@

         __exported by:__ @typedef_vs_macro.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Foo where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_a2 -> F.pokeByteOff ptr0 (0 :: Int) foo_a2
