{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Internal.CAPI
import Prelude (Double, IO)

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
     FC.CDouble
  -> FC.CDouble
hs_bindgen_1c811bfb80de8f77 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_1c811bfb80de8f77_base

{-|

  Marked @__attribute((const))__@

__C declaration:__ @erf@

__defined at:__ @functions\/simple_func.h 1:8@

__exported by:__ @functions\/simple_func.h@
-}
erf ::
     FC.CDouble
     -- ^ __C declaration:__ @arg@
  -> FC.CDouble
erf = hs_bindgen_1c811bfb80de8f77

-- __unique:__ @test_functionssimple_func_Example_Safe_bad_fma@
foreign import ccall safe "hs_bindgen_180022d3518c53bd" hs_bindgen_180022d3518c53bd_base ::
     Double
  -> Double
  -> Double
  -> IO Double

-- __unique:__ @test_functionssimple_func_Example_Safe_bad_fma@
hs_bindgen_180022d3518c53bd ::
     FC.CDouble
  -> FC.CDouble
  -> FC.CDouble
  -> IO FC.CDouble
hs_bindgen_180022d3518c53bd =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_180022d3518c53bd_base

{-| __C declaration:__ @bad_fma@

    __defined at:__ @functions\/simple_func.h 3:22@

    __exported by:__ @functions\/simple_func.h@
-}
bad_fma ::
     FC.CDouble
     -- ^ __C declaration:__ @x@
  -> FC.CDouble
     -- ^ __C declaration:__ @y@
  -> FC.CDouble
     -- ^ __C declaration:__ @z@
  -> IO FC.CDouble
bad_fma = hs_bindgen_180022d3518c53bd

-- __unique:__ @test_functionssimple_func_Example_Safe_no_args@
foreign import ccall safe "hs_bindgen_d72558f6f977200c" hs_bindgen_d72558f6f977200c_base ::
     IO ()

-- __unique:__ @test_functionssimple_func_Example_Safe_no_args@
hs_bindgen_d72558f6f977200c :: IO ()
hs_bindgen_d72558f6f977200c =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d72558f6f977200c_base

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
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d8523e2ccea5c7ba_base

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @functions\/simple_func.h 9:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args_no_void :: IO ()
no_args_no_void = hs_bindgen_d8523e2ccea5c7ba

-- __unique:__ @test_functionssimple_func_Example_Safe_fun@
foreign import ccall safe "hs_bindgen_48cbd3cd1c6e874f" hs_bindgen_48cbd3cd1c6e874f_base ::
     GHC.Int.Int8
  -> Double
  -> IO GHC.Int.Int32

-- __unique:__ @test_functionssimple_func_Example_Safe_fun@
hs_bindgen_48cbd3cd1c6e874f ::
     FC.CChar
  -> FC.CDouble
  -> IO FC.CInt
hs_bindgen_48cbd3cd1c6e874f =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_48cbd3cd1c6e874f_base

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/simple_func.h 11:5@

    __exported by:__ @functions\/simple_func.h@
-}
fun ::
     FC.CChar
     -- ^ __C declaration:__ @x@
  -> FC.CDouble
     -- ^ __C declaration:__ @y@
  -> IO FC.CInt
fun = hs_bindgen_48cbd3cd1c6e874f
