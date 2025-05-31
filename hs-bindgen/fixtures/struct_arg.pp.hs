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

$(CAPI.addCSource "#include \"struct_arg.h\"\nsigned int testmodule_thing_fun_1 (struct thing *arg1) { return thing_fun_1(*arg1); }\nvoid testmodule_thing_fun_2 (signed int arg1, struct thing *arg2) { *arg2 = thing_fun_2(arg1); }\n")

data Thing = Thing
  { thing_x :: FC.CInt
  }

instance F.Storable Thing where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Thing
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Thing thing_x2 -> F.pokeByteOff ptr0 (0 :: Int) thing_x2

deriving stock instance Show Thing

deriving stock instance Eq Thing

foreign import ccall safe "testmodule_thing_fun_1" thing_fun_1 :: (F.Ptr Thing) -> IO FC.CInt

foreign import ccall safe "testmodule_thing_fun_2" thing_fun_2 :: FC.CInt -> (F.Ptr Thing) -> IO ()
