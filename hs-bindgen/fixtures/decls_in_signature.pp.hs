{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

$(CAPI.addCSource "#include \"decls_in_signature.h\"\nvoid testmodule_normal (struct opaque *arg1, struct outside *arg2, struct outside *arg3) { normal(arg1, arg2, *arg3); }\n")

data Opaque

data Outside = Outside
  { outside_x :: FC.CInt
  , outside_y :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Outside where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Outside
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Outside outside_x2 outside_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) outside_x2
            >> F.pokeByteOff ptr0 (4 :: Int) outside_y3

foreign import ccall safe "testmodule_normal" normal_wrapper :: (F.Ptr Opaque) -> (F.Ptr Outside) -> (F.Ptr Outside) -> IO ()

normal :: (F.Ptr Opaque) -> (F.Ptr Outside) -> Outside -> IO ()
normal = \x0 -> \x1 -> \x2 -> F.with x2 (\y3 -> normal_wrapper x0 x1 y3)
