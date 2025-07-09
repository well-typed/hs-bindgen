{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import Data.Void (Void)
import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

data Foo = Foo
  { foo_c :: FC.CChar
  , foo_i :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Foo where

  sizeOf = \_ -> (5 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (1 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_c2 foo_i3 ->
               F.pokeByteOff ptr0 (0 :: Int) foo_c2
            >> F.pokeByteOff ptr0 (1 :: Int) foo_i3

data Bar = Bar
  { bar_c :: FC.CChar
  , bar_i :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Bar where

  sizeOf = \_ -> (5 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Bar
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (1 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_c2 bar_i3 ->
               F.pokeByteOff ptr0 (0 :: Int) bar_c2
            >> F.pokeByteOff ptr0 (1 :: Int) bar_i3

data Baz = Baz
  { baz_c :: FC.CChar
  , baz_i :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Baz where

  sizeOf = \_ -> (5 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Baz
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (1 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Baz baz_c2 baz_i3 ->
               F.pokeByteOff ptr0 (0 :: Int) baz_c2
            >> F.pokeByteOff ptr0 (1 :: Int) baz_i3

data Qux = Qux
  { qux_c :: FC.CChar
  , qux_i :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Qux where

  sizeOf = \_ -> (5 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Qux
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (1 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Qux qux_c2 qux_i3 ->
               F.pokeByteOff ptr0 (0 :: Int) qux_c2
            >> F.pokeByteOff ptr0 (1 :: Int) qux_i3

data FILE = FILE
  { fILE__r :: FC.CInt
  , fILE__w :: FC.CInt
  , fILE__close :: F.FunPtr ((F.Ptr Void) -> IO FC.CInt)
  }
  deriving stock (Eq, Show)

instance F.Storable FILE where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure FILE
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          FILE fILE__r2 fILE__w3 fILE__close4 ->
               F.pokeByteOff ptr0 (0 :: Int) fILE__r2
            >> F.pokeByteOff ptr0 (4 :: Int) fILE__w3
            >> F.pokeByteOff ptr0 (8 :: Int) fILE__close4
