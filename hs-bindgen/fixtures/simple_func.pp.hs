{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <simple_func.h>"
  , "double hs_bindgen_test_simple_func_4b858faf89c6033a ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return erf(arg1);"
  , "}"
  , "double hs_bindgen_test_simple_func_175251e70d29cd73 ("
  , "  double arg1,"
  , "  double arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return bad_fma(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_test_simple_func_bda1aaa13afe437a (void)"
  , "{"
  , "  no_args();"
  , "}"
  , "void hs_bindgen_test_simple_func_8d4283a1963012db (void)"
  , "{"
  , "  no_args_no_void();"
  , "}"
  , "signed int hs_bindgen_test_simple_func_51ad12b64aea929d ("
  , "  char arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return fun(arg1, arg2);"
  , "}"
  , "/* get_erf_ptr */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_test_simple_func_723348151ff43970 (void)) ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return &erf;"
  , "}"
  , "/* get_bad_fma_ptr */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_test_simple_func_f3190cb919f94cd9 (void)) ("
  , "  double arg1,"
  , "  double arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return &bad_fma;"
  , "}"
  , "/* get_no_args_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_simple_func_fbdbb067d942094e (void)) (void)"
  , "{"
  , "  return &no_args;"
  , "}"
  , "/* get_no_args_no_void_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_simple_func_452280b5085b4ccd (void)) (void)"
  , "{"
  , "  return &no_args_no_void;"
  , "}"
  , "/* get_fun_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_simple_func_b16b846810561073 (void)) ("
  , "  char arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &fun;"
  , "}"
  ]))

{-| __C declaration:__ @erf@

    __defined at:__ @simple_func.h:1:8@

    __exported by:__ @simple_func.h@
-}
foreign import ccall safe "hs_bindgen_test_simple_func_4b858faf89c6033a" erf ::
     FC.CDouble
     {- ^ __C declaration:__ @arg@
     -}
  -> IO FC.CDouble

{-| __C declaration:__ @bad_fma@

    __defined at:__ @simple_func.h:3:22@

    __exported by:__ @simple_func.h@
-}
foreign import ccall safe "hs_bindgen_test_simple_func_175251e70d29cd73" bad_fma ::
     FC.CDouble
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @y@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @z@
     -}
  -> IO FC.CDouble

{-| __C declaration:__ @no_args@

    __defined at:__ @simple_func.h:7:6@

    __exported by:__ @simple_func.h@
-}
foreign import ccall safe "hs_bindgen_test_simple_func_bda1aaa13afe437a" no_args ::
     IO ()

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @simple_func.h:9:6@

    __exported by:__ @simple_func.h@
-}
foreign import ccall safe "hs_bindgen_test_simple_func_8d4283a1963012db" no_args_no_void ::
     IO ()

{-| __C declaration:__ @fun@

    __defined at:__ @simple_func.h:11:5@

    __exported by:__ @simple_func.h@
-}
foreign import ccall safe "hs_bindgen_test_simple_func_51ad12b64aea929d" fun ::
     FC.CChar
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @y@
     -}
  -> IO FC.CInt

foreign import ccall unsafe "hs_bindgen_test_simple_func_723348151ff43970" hs_bindgen_test_simple_func_723348151ff43970 ::
     IO (Ptr.FunPtr (FC.CDouble -> IO FC.CDouble))

{-# NOINLINE erf_ptr #-}

{-| __C declaration:__ @erf@

    __defined at:__ @simple_func.h:1:8@

    __exported by:__ @simple_func.h@
-}
erf_ptr :: Ptr.FunPtr (FC.CDouble -> IO FC.CDouble)
erf_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_simple_func_723348151ff43970

foreign import ccall unsafe "hs_bindgen_test_simple_func_f3190cb919f94cd9" hs_bindgen_test_simple_func_f3190cb919f94cd9 ::
     IO (Ptr.FunPtr (FC.CDouble -> FC.CDouble -> FC.CDouble -> IO FC.CDouble))

{-# NOINLINE bad_fma_ptr #-}

{-| __C declaration:__ @bad_fma@

    __defined at:__ @simple_func.h:3:22@

    __exported by:__ @simple_func.h@
-}
bad_fma_ptr :: Ptr.FunPtr (FC.CDouble -> FC.CDouble -> FC.CDouble -> IO FC.CDouble)
bad_fma_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_simple_func_f3190cb919f94cd9

foreign import ccall unsafe "hs_bindgen_test_simple_func_fbdbb067d942094e" hs_bindgen_test_simple_func_fbdbb067d942094e ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE no_args_ptr #-}

{-| __C declaration:__ @no_args@

    __defined at:__ @simple_func.h:7:6@

    __exported by:__ @simple_func.h@
-}
no_args_ptr :: Ptr.FunPtr (IO ())
no_args_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_simple_func_fbdbb067d942094e

foreign import ccall unsafe "hs_bindgen_test_simple_func_452280b5085b4ccd" hs_bindgen_test_simple_func_452280b5085b4ccd ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE no_args_no_void_ptr #-}

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @simple_func.h:9:6@

    __exported by:__ @simple_func.h@
-}
no_args_no_void_ptr :: Ptr.FunPtr (IO ())
no_args_no_void_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_simple_func_452280b5085b4ccd

foreign import ccall unsafe "hs_bindgen_test_simple_func_b16b846810561073" hs_bindgen_test_simple_func_b16b846810561073 ::
     IO (Ptr.FunPtr (FC.CChar -> FC.CDouble -> IO FC.CInt))

{-# NOINLINE fun_ptr #-}

{-| __C declaration:__ @fun@

    __defined at:__ @simple_func.h:11:5@

    __exported by:__ @simple_func.h@
-}
fun_ptr :: Ptr.FunPtr (FC.CChar -> FC.CDouble -> IO FC.CInt)
fun_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_simple_func_b16b846810561073
