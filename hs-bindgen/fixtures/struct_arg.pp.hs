{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude ((<*>), Eq, IO, Int, Show, pure)

$(CAPI.addCSource "#include <struct_arg.h>\nsigned int hs_bindgen_test_struct_arg_be997777eb388096 (struct thing *arg1) { return thing_fun_1(*arg1); }\nvoid hs_bindgen_test_struct_arg_c719e5e844a53956 (signed int arg1, struct thing *arg2) { *arg2 = thing_fun_2(arg1); }\nvoid hs_bindgen_test_struct_arg_9540300ca2ef6349 (signed int arg1, struct thing *arg2, double arg3, struct thing *arg4) { *arg4 = thing_fun_3a(arg1, *arg2, arg3); }\nchar hs_bindgen_test_struct_arg_f6f54b421741a2de (signed int arg1, struct thing *arg2, double arg3) { return thing_fun_3b(arg1, *arg2, arg3); }\n")

data Thing = Thing
  { thing_x :: FC.CInt
  }
  deriving stock (Eq, Show)

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
          Thing thing_x2 ->
            F.pokeByteOff ptr0 (0 :: Int) thing_x2

foreign import ccall safe "hs_bindgen_test_struct_arg_be997777eb388096" thing_fun_1_wrapper :: (F.Ptr Thing) -> IO FC.CInt

thing_fun_1 :: Thing -> IO FC.CInt
thing_fun_1 =
  \x0 -> F.with x0 (\y1 -> thing_fun_1_wrapper y1)

foreign import ccall safe "hs_bindgen_test_struct_arg_c719e5e844a53956" thing_fun_2_wrapper :: FC.CInt -> (F.Ptr Thing) -> IO ()

thing_fun_2 :: FC.CInt -> IO Thing
thing_fun_2 =
  \x0 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\z1 ->
                                            thing_fun_2_wrapper x0 z1)

foreign import ccall safe "hs_bindgen_test_struct_arg_9540300ca2ef6349" thing_fun_3a_wrapper :: FC.CInt -> (F.Ptr Thing) -> FC.CDouble -> (F.Ptr Thing) -> IO ()

thing_fun_3a :: FC.CInt -> Thing -> FC.CDouble -> IO Thing
thing_fun_3a =
  \x0 ->
    \x1 ->
      \x2 ->
        F.with x1 (\y3 ->
                     HsBindgen.Runtime.CAPI.allocaAndPeek (\z4 ->
                                                             thing_fun_3a_wrapper x0 y3 x2 z4))

foreign import ccall safe "hs_bindgen_test_struct_arg_f6f54b421741a2de" thing_fun_3b_wrapper :: FC.CInt -> (F.Ptr Thing) -> FC.CDouble -> IO FC.CChar

thing_fun_3b :: FC.CInt -> Thing -> FC.CDouble -> IO FC.CChar
thing_fun_3b =
  \x0 ->
    \x1 ->
      \x2 ->
        F.with x1 (\y3 -> thing_fun_3b_wrapper x0 y3 x2)
