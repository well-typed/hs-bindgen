{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import Data.Void (Void)
import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

data Foo = Foo
  { foo_c :: FC.CChar
  , foo_i :: FC.CInt
  }

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

deriving stock instance Show Foo

deriving stock instance Eq Foo

data Bar = Bar
  { bar_c :: FC.CChar
  , bar_i :: FC.CInt
  }

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

deriving stock instance Show Bar

deriving stock instance Eq Bar

data Baz = Baz
  { baz_c :: FC.CChar
  , baz_i :: FC.CInt
  }

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

deriving stock instance Show Baz

deriving stock instance Eq Baz

data Qux = Qux
  { qux_c :: FC.CChar
  , qux_i :: FC.CInt
  }

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

deriving stock instance Show Qux

deriving stock instance Eq Qux

data C__SFILE = C__SFILE
  { __sFILE__r :: FC.CInt
  , __sFILE__w :: FC.CInt
  , __sFILE__close :: F.FunPtr ((F.Ptr Void) -> IO FC.CInt)
  }

instance F.Storable C__SFILE where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure C__SFILE
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          C__SFILE __sFILE__r2 __sFILE__w3 __sFILE__close4 ->
               F.pokeByteOff ptr0 (0 :: Int) __sFILE__r2
            >> F.pokeByteOff ptr0 (4 :: Int) __sFILE__w3
            >> F.pokeByteOff ptr0 (8 :: Int) __sFILE__close4

deriving stock instance Show C__SFILE

deriving stock instance Eq C__SFILE

newtype FILE = FILE
  { un_FILE :: C__SFILE
  }

deriving newtype instance F.Storable FILE

deriving stock instance Eq FILE

deriving stock instance Show FILE
