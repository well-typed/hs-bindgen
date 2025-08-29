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

{-| __/Automatically generated from C/__

    __C declaration:__ @thing@

    __defined at:__ @struct_arg.h:2:8@

    __exported by:__ @struct_arg.h@
-}
data Thing = Thing
  { thing_x :: FC.CInt
    {- ^ __/Automatically generated from C/__

         __C declaration:__ @x@

         __defined at:__ @struct_arg.h:3:9@

         __exported by:__ @struct_arg.h@
    -}
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

{-| __/Automatically generated from C/__

    __C declaration:__ @thing_fun_1@

    __defined at:__ @struct_arg.h:6:5@

    __exported by:__ @struct_arg.h@
-}
foreign import ccall safe "hs_bindgen_test_struct_arg_be997777eb388096" thing_fun_1_wrapper
  :: F.Ptr Thing
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> IO FC.CInt

thing_fun_1 :: Thing -> IO FC.CInt
thing_fun_1 =
  \x0 -> F.with x0 (\y1 -> thing_fun_1_wrapper y1)

{-| __/Automatically generated from C/__

    __C declaration:__ @thing_fun_2@

    __defined at:__ @struct_arg.h:7:14@

    __exported by:__ @struct_arg.h@
-}
foreign import ccall safe "hs_bindgen_test_struct_arg_c719e5e844a53956" thing_fun_2_wrapper
  :: FC.CInt
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> F.Ptr Thing
  -> IO ()

thing_fun_2 :: FC.CInt -> IO Thing
thing_fun_2 =
  \x0 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\z1 ->
                                            thing_fun_2_wrapper x0 z1)

{-| __/Automatically generated from C/__

    __C declaration:__ @thing_fun_3a@

    __defined at:__ @struct_arg.h:9:14@

    __exported by:__ @struct_arg.h@
-}
foreign import ccall safe "hs_bindgen_test_struct_arg_9540300ca2ef6349" thing_fun_3a_wrapper
  :: FC.CInt
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> F.Ptr Thing
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @y@
     -}
  -> FC.CDouble
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @z@
     -}
  -> F.Ptr Thing
  -> IO ()

thing_fun_3a :: FC.CInt -> Thing -> FC.CDouble -> IO Thing
thing_fun_3a =
  \x0 ->
    \x1 ->
      \x2 ->
        F.with x1 (\y3 ->
                     HsBindgen.Runtime.CAPI.allocaAndPeek (\z4 ->
                                                             thing_fun_3a_wrapper x0 y3 x2 z4))

{-| __/Automatically generated from C/__

    __C declaration:__ @thing_fun_3b@

    __defined at:__ @struct_arg.h:10:6@

    __exported by:__ @struct_arg.h@
-}
foreign import ccall safe "hs_bindgen_test_struct_arg_f6f54b421741a2de" thing_fun_3b_wrapper
  :: FC.CInt
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @x@
     -}
  -> F.Ptr Thing
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @y@
     -}
  -> FC.CDouble
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @z@
     -}
  -> IO FC.CChar

thing_fun_3b :: FC.CInt -> Thing -> FC.CDouble -> IO FC.CChar
thing_fun_3b =
  \x0 ->
    \x1 ->
      \x2 ->
        F.with x1 (\y3 -> thing_fun_3b_wrapper x0 y3 x2)
