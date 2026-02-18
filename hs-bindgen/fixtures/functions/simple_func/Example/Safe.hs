{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/simple_func.h>"
  , "double hs_bindgen_1c811bfb80de8f77 ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return erf(arg1);"
  , "}"
  , "double hs_bindgen_180022d3518c53bd ("
  , "  double arg1,"
  , "  double arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return bad_fma(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_d72558f6f977200c (void)"
  , "{"
  , "  no_args();"
  , "}"
  , "void hs_bindgen_d8523e2ccea5c7ba (void)"
  , "{"
  , "  no_args_no_void();"
  , "}"
  , "signed int hs_bindgen_48cbd3cd1c6e874f ("
  , "  char arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return fun(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_functionssimple_func_Example_Safe_erf@
foreign import ccall safe "hs_bindgen_1c811bfb80de8f77" hs_bindgen_1c811bfb80de8f77_base ::
     Double
  -> Double

-- __unique:__ @test_functionssimple_func_Example_Safe_erf@
hs_bindgen_1c811bfb80de8f77 ::
     RIP.CDouble
  -> RIP.CDouble
hs_bindgen_1c811bfb80de8f77 =
  RIP.fromFFIType hs_bindgen_1c811bfb80de8f77_base

{-|

  Marked @__attribute((const))__@

__C declaration:__ @erf@

__defined at:__ @functions\/simple_func.h 1:8@

__exported by:__ @functions\/simple_func.h@
-}
erf ::
     RIP.CDouble
     -- ^ __C declaration:__ @arg@
  -> RIP.CDouble
erf = hs_bindgen_1c811bfb80de8f77

-- __unique:__ @test_functionssimple_func_Example_Safe_bad_fma@
foreign import ccall safe "hs_bindgen_180022d3518c53bd" hs_bindgen_180022d3518c53bd_base ::
     Double
  -> Double
  -> Double
  -> IO Double

-- __unique:__ @test_functionssimple_func_Example_Safe_bad_fma@
hs_bindgen_180022d3518c53bd ::
     RIP.CDouble
  -> RIP.CDouble
  -> RIP.CDouble
  -> IO RIP.CDouble
hs_bindgen_180022d3518c53bd =
  RIP.fromFFIType hs_bindgen_180022d3518c53bd_base

{-| __C declaration:__ @bad_fma@

    __defined at:__ @functions\/simple_func.h 3:22@

    __exported by:__ @functions\/simple_func.h@
-}
bad_fma ::
     RIP.CDouble
     -- ^ __C declaration:__ @x@
  -> RIP.CDouble
     -- ^ __C declaration:__ @y@
  -> RIP.CDouble
     -- ^ __C declaration:__ @z@
  -> IO RIP.CDouble
bad_fma = hs_bindgen_180022d3518c53bd

-- __unique:__ @test_functionssimple_func_Example_Safe_no_args@
foreign import ccall safe "hs_bindgen_d72558f6f977200c" hs_bindgen_d72558f6f977200c_base ::
     IO ()

-- __unique:__ @test_functionssimple_func_Example_Safe_no_args@
hs_bindgen_d72558f6f977200c :: IO ()
hs_bindgen_d72558f6f977200c =
  RIP.fromFFIType hs_bindgen_d72558f6f977200c_base

{-| __C declaration:__ @no_args@

    __defined at:__ @functions\/simple_func.h 7:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args :: IO ()
no_args = hs_bindgen_d72558f6f977200c

-- __unique:__ @test_functionssimple_func_Example_Safe_no_args_no_void@
foreign import ccall safe "hs_bindgen_d8523e2ccea5c7ba" hs_bindgen_d8523e2ccea5c7ba_base ::
     IO ()

-- __unique:__ @test_functionssimple_func_Example_Safe_no_args_no_void@
hs_bindgen_d8523e2ccea5c7ba :: IO ()
hs_bindgen_d8523e2ccea5c7ba =
  RIP.fromFFIType hs_bindgen_d8523e2ccea5c7ba_base

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @functions\/simple_func.h 9:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args_no_void :: IO ()
no_args_no_void = hs_bindgen_d8523e2ccea5c7ba

-- __unique:__ @test_functionssimple_func_Example_Safe_fun@
foreign import ccall safe "hs_bindgen_48cbd3cd1c6e874f" hs_bindgen_48cbd3cd1c6e874f_base ::
     RIP.Int8
  -> Double
  -> IO RIP.Int32

-- __unique:__ @test_functionssimple_func_Example_Safe_fun@
hs_bindgen_48cbd3cd1c6e874f ::
     RIP.CChar
  -> RIP.CDouble
  -> IO RIP.CInt
hs_bindgen_48cbd3cd1c6e874f =
  RIP.fromFFIType hs_bindgen_48cbd3cd1c6e874f_base

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/simple_func.h 11:5@

    __exported by:__ @functions\/simple_func.h@
-}
fun ::
     RIP.CChar
     -- ^ __C declaration:__ @x@
  -> RIP.CDouble
     -- ^ __C declaration:__ @y@
  -> IO RIP.CInt
fun = hs_bindgen_48cbd3cd1c6e874f
