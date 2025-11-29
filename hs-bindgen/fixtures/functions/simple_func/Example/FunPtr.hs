{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/simple_func.h>"
  , "/* ExampleNothingget_erf_ptr */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_test_functionssimple_func_723348151ff43970 (void)) ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return &erf;"
  , "}"
  , "/* ExampleNothingget_bad_fma_ptr */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_test_functionssimple_func_f3190cb919f94cd9 (void)) ("
  , "  double arg1,"
  , "  double arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return &bad_fma;"
  , "}"
  , "/* ExampleNothingget_no_args_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionssimple_func_fbdbb067d942094e (void)) (void)"
  , "{"
  , "  return &no_args;"
  , "}"
  , "/* ExampleNothingget_no_args_no_void_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionssimple_func_452280b5085b4ccd (void)) (void)"
  , "{"
  , "  return &no_args_no_void;"
  , "}"
  , "/* ExampleNothingget_fun_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionssimple_func_b16b846810561073 (void)) ("
  , "  char arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &fun;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_erf_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionssimple_func_723348151ff43970" hs_bindgen_test_functionssimple_func_723348151ff43970 ::
     IO (Ptr.FunPtr (FC.CDouble -> IO FC.CDouble))

{-# NOINLINE erf_ptr #-}

{-| __C declaration:__ @erf@

    __defined at:__ @functions\/simple_func.h:1:8@

    __exported by:__ @functions\/simple_func.h@
-}
erf_ptr :: Ptr.FunPtr (FC.CDouble -> IO FC.CDouble)
erf_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionssimple_func_723348151ff43970

{-| __unique:__ @ExampleNothingget_bad_fma_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionssimple_func_f3190cb919f94cd9" hs_bindgen_test_functionssimple_func_f3190cb919f94cd9 ::
     IO (Ptr.FunPtr (FC.CDouble -> FC.CDouble -> FC.CDouble -> IO FC.CDouble))

{-# NOINLINE bad_fma_ptr #-}

{-| __C declaration:__ @bad_fma@

    __defined at:__ @functions\/simple_func.h:3:22@

    __exported by:__ @functions\/simple_func.h@
-}
bad_fma_ptr :: Ptr.FunPtr (FC.CDouble -> FC.CDouble -> FC.CDouble -> IO FC.CDouble)
bad_fma_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionssimple_func_f3190cb919f94cd9

{-| __unique:__ @ExampleNothingget_no_args_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionssimple_func_fbdbb067d942094e" hs_bindgen_test_functionssimple_func_fbdbb067d942094e ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE no_args_ptr #-}

{-| __C declaration:__ @no_args@

    __defined at:__ @functions\/simple_func.h:7:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args_ptr :: Ptr.FunPtr (IO ())
no_args_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionssimple_func_fbdbb067d942094e

{-| __unique:__ @ExampleNothingget_no_args_no_void_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionssimple_func_452280b5085b4ccd" hs_bindgen_test_functionssimple_func_452280b5085b4ccd ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE no_args_no_void_ptr #-}

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @functions\/simple_func.h:9:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args_no_void_ptr :: Ptr.FunPtr (IO ())
no_args_no_void_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionssimple_func_452280b5085b4ccd

{-| __unique:__ @ExampleNothingget_fun_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionssimple_func_b16b846810561073" hs_bindgen_test_functionssimple_func_b16b846810561073 ::
     IO (Ptr.FunPtr (FC.CChar -> FC.CDouble -> IO FC.CInt))

{-# NOINLINE fun_ptr #-}

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/simple_func.h:11:5@

    __exported by:__ @functions\/simple_func.h@
-}
fun_ptr :: Ptr.FunPtr (FC.CChar -> FC.CDouble -> IO FC.CInt)
fun_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionssimple_func_b16b846810561073
