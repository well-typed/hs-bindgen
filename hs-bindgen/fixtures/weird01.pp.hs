{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude ((<*>), Eq, IO, Int, Show, pure)

$(CAPI.addCSource "#include \"weird01.h\"\nvoid testmodule_func (struct bar *arg1) { func(arg1); }\n")

data Foo = Foo
  { foo_z :: FC.CInt
  }

instance F.Storable Foo where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_z2 -> F.pokeByteOff ptr0 (0 :: Int) foo_z2

deriving stock instance Show Foo

deriving stock instance Eq Foo

data Bar = Bar
  { bar_x :: FC.CInt
  }

instance F.Storable Bar where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Bar
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_x2 -> F.pokeByteOff ptr0 (0 :: Int) bar_x2

deriving stock instance Show Bar

deriving stock instance Eq Bar

foreign import ccall safe "testmodule_func" func :: (F.Ptr Bar) -> IO ()
