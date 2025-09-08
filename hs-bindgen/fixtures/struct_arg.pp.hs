{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude ((<*>), Eq, IO, Int, Show, pure)

$(CAPI.addCSource "#include <struct_arg.h>\nsigned int hs_bindgen_test_struct_arg_be997777eb388096 (struct thing *arg1) { return thing_fun_1(*arg1); }\n/* get_thing_fun_1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_struct_arg_d5cf000d627eba66 (void)) (struct thing arg1) { return &thing_fun_1; } \nvoid hs_bindgen_test_struct_arg_c719e5e844a53956 (signed int arg1, struct thing *arg2) { *arg2 = thing_fun_2(arg1); }\n/* get_thing_fun_2_ptr */ __attribute__ ((const)) struct thing (*hs_bindgen_test_struct_arg_c5543d9dadeca704 (void)) (signed int arg1) { return &thing_fun_2; } \nvoid hs_bindgen_test_struct_arg_9540300ca2ef6349 (signed int arg1, struct thing *arg2, double arg3, struct thing *arg4) { *arg4 = thing_fun_3a(arg1, *arg2, arg3); }\n/* get_thing_fun_3a_ptr */ __attribute__ ((const)) struct thing (*hs_bindgen_test_struct_arg_6f4d585feed7ca5e (void)) (signed int arg1, struct thing arg2, double arg3) { return &thing_fun_3a; } \nchar hs_bindgen_test_struct_arg_f6f54b421741a2de (signed int arg1, struct thing *arg2, double arg3) { return thing_fun_3b(arg1, *arg2, arg3); }\n/* get_thing_fun_3b_ptr */ __attribute__ ((const)) char (*hs_bindgen_test_struct_arg_ef6a607b6432889d (void)) (signed int arg1, struct thing arg2, double arg3) { return &thing_fun_3b; } \n")

{-| __C declaration:__ @thing@

    __defined at:__ @struct_arg.h:2:8@

    __exported by:__ @struct_arg.h@
-}
data Thing = Thing
  { thing_x :: FC.CInt
    {- ^ __C declaration:__ @x@

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

{-| __C declaration:__ @thing_fun_1@

    __defined at:__ @struct_arg.h:6:5@

    __exported by:__ @struct_arg.h@
-}
foreign import ccall safe "hs_bindgen_test_struct_arg_be997777eb388096" thing_fun_1_wrapper
  :: Ptr.Ptr Thing
     {- ^ __C declaration:__ @x@
     -}
  -> IO FC.CInt

thing_fun_1 :: Thing -> IO FC.CInt
thing_fun_1 =
  \x0 -> F.with x0 (\y1 -> thing_fun_1_wrapper y1)

{-| __C declaration:__ @thing_fun_1@

    __defined at:__ @struct_arg.h:6:5@

    __exported by:__ @struct_arg.h@
-}
foreign import ccall unsafe "hs_bindgen_test_struct_arg_d5cf000d627eba66" hs_bindgen_test_struct_arg_d5cf000d627eba66
  :: IO (Ptr.FunPtr (Thing -> IO FC.CInt))

{-# NOINLINE thing_fun_1_ptr #-}

thing_fun_1_ptr :: Ptr.FunPtr (Thing -> IO FC.CInt)
thing_fun_1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_struct_arg_d5cf000d627eba66

{-| __C declaration:__ @thing_fun_2@

    __defined at:__ @struct_arg.h:7:14@

    __exported by:__ @struct_arg.h@
-}
foreign import ccall safe "hs_bindgen_test_struct_arg_c719e5e844a53956" thing_fun_2_wrapper
  :: FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr Thing
  -> IO ()

thing_fun_2 :: FC.CInt -> IO Thing
thing_fun_2 =
  \x0 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\z1 ->
                                            thing_fun_2_wrapper x0 z1)

{-| __C declaration:__ @thing_fun_2@

    __defined at:__ @struct_arg.h:7:14@

    __exported by:__ @struct_arg.h@
-}
foreign import ccall unsafe "hs_bindgen_test_struct_arg_c5543d9dadeca704" hs_bindgen_test_struct_arg_c5543d9dadeca704
  :: IO (Ptr.FunPtr (FC.CInt -> IO Thing))

{-# NOINLINE thing_fun_2_ptr #-}

thing_fun_2_ptr :: Ptr.FunPtr (FC.CInt -> IO Thing)
thing_fun_2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_struct_arg_c5543d9dadeca704

{-| __C declaration:__ @thing_fun_3a@

    __defined at:__ @struct_arg.h:9:14@

    __exported by:__ @struct_arg.h@
-}
foreign import ccall safe "hs_bindgen_test_struct_arg_9540300ca2ef6349" thing_fun_3a_wrapper
  :: FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr Thing
     {- ^ __C declaration:__ @y@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @z@
     -}
  -> Ptr.Ptr Thing
  -> IO ()

thing_fun_3a :: FC.CInt -> Thing -> FC.CDouble -> IO Thing
thing_fun_3a =
  \x0 ->
    \x1 ->
      \x2 ->
        F.with x1 (\y3 ->
                     HsBindgen.Runtime.CAPI.allocaAndPeek (\z4 ->
                                                             thing_fun_3a_wrapper x0 y3 x2 z4))

{-| __C declaration:__ @thing_fun_3a@

    __defined at:__ @struct_arg.h:9:14@

    __exported by:__ @struct_arg.h@
-}
foreign import ccall unsafe "hs_bindgen_test_struct_arg_6f4d585feed7ca5e" hs_bindgen_test_struct_arg_6f4d585feed7ca5e
  :: IO (Ptr.FunPtr (FC.CInt -> Thing -> FC.CDouble -> IO Thing))

{-# NOINLINE thing_fun_3a_ptr #-}

thing_fun_3a_ptr :: Ptr.FunPtr (FC.CInt -> Thing -> FC.CDouble -> IO Thing)
thing_fun_3a_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_struct_arg_6f4d585feed7ca5e

{-| __C declaration:__ @thing_fun_3b@

    __defined at:__ @struct_arg.h:10:6@

    __exported by:__ @struct_arg.h@
-}
foreign import ccall safe "hs_bindgen_test_struct_arg_f6f54b421741a2de" thing_fun_3b_wrapper
  :: FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr Thing
     {- ^ __C declaration:__ @y@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @z@
     -}
  -> IO FC.CChar

thing_fun_3b :: FC.CInt -> Thing -> FC.CDouble -> IO FC.CChar
thing_fun_3b =
  \x0 ->
    \x1 ->
      \x2 ->
        F.with x1 (\y3 -> thing_fun_3b_wrapper x0 y3 x2)

{-| __C declaration:__ @thing_fun_3b@

    __defined at:__ @struct_arg.h:10:6@

    __exported by:__ @struct_arg.h@
-}
foreign import ccall unsafe "hs_bindgen_test_struct_arg_ef6a607b6432889d" hs_bindgen_test_struct_arg_ef6a607b6432889d
  :: IO (Ptr.FunPtr (FC.CInt -> Thing -> FC.CDouble -> IO FC.CChar))

{-# NOINLINE thing_fun_3b_ptr #-}

thing_fun_3b_ptr :: Ptr.FunPtr (FC.CInt -> Thing -> FC.CDouble -> IO FC.CChar)
thing_fun_3b_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_struct_arg_ef6a607b6432889d
