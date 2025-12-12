{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_1c811bfb80de8f77" erf_base ::
     FC.CDouble
  -> FC.CDouble

{-| __C declaration:__ @erf@

    __defined at:__ @functions\/simple_func.h:1:8@

    __exported by:__ @functions\/simple_func.h@

    __unique:__ @test_functionssimple_func_Example_Safe_erf@
-}
erf ::
     FC.CDouble
     -- ^ __C declaration:__ @arg@
  -> FC.CDouble
erf =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType erf_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_180022d3518c53bd" bad_fma_base ::
     FC.CDouble
  -> FC.CDouble
  -> FC.CDouble
  -> IO FC.CDouble

{-| __C declaration:__ @bad_fma@

    __defined at:__ @functions\/simple_func.h:3:22@

    __exported by:__ @functions\/simple_func.h@

    __unique:__ @test_functionssimple_func_Example_Safe_bad_fma@
-}
bad_fma ::
     FC.CDouble
     -- ^ __C declaration:__ @x@
  -> FC.CDouble
     -- ^ __C declaration:__ @y@
  -> FC.CDouble
     -- ^ __C declaration:__ @z@
  -> IO FC.CDouble
bad_fma =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType bad_fma_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_d72558f6f977200c" no_args_base ::
     IO ()

{-| __C declaration:__ @no_args@

    __defined at:__ @functions\/simple_func.h:7:6@

    __exported by:__ @functions\/simple_func.h@

    __unique:__ @test_functionssimple_func_Example_Safe_no_args@
-}
no_args ::
     IO ()
no_args =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType no_args_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_d8523e2ccea5c7ba" no_args_no_void_base ::
     IO ()

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @functions\/simple_func.h:9:6@

    __exported by:__ @functions\/simple_func.h@

    __unique:__ @test_functionssimple_func_Example_Safe_no_args_no_void@
-}
no_args_no_void ::
     IO ()
no_args_no_void =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType no_args_no_void_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_48cbd3cd1c6e874f" fun_base ::
     FC.CChar
  -> FC.CDouble
  -> IO FC.CInt

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/simple_func.h:11:5@

    __exported by:__ @functions\/simple_func.h@

    __unique:__ @test_functionssimple_func_Example_Safe_fun@
-}
fun ::
     FC.CChar
     -- ^ __C declaration:__ @x@
  -> FC.CDouble
     -- ^ __C declaration:__ @y@
  -> IO FC.CInt
fun =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_base
