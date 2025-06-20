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

$(CAPI.addCSource "#include \"skip_over_long_double.h\"\nvoid testmodule_fun2 (signed int arg1) { fun2(arg1); }\n")

foreign import ccall safe "testmodule_fun2" fun2 :: FC.CInt -> IO ()

data Struct2 = Struct2
  { struct2_x :: FC.CInt
  }

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
          Struct2 struct2_x2 -> F.pokeByteOff ptr0 (0 :: Int) struct2_x2

deriving stock instance Show Struct2

deriving stock instance Eq Struct2
