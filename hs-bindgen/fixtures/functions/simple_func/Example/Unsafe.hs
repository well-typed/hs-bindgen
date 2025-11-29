{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/simple_func.h>"
  , "double hs_bindgen_test_functionssimple_func_a9fb5e2e602effce ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return erf(arg1);"
  , "}"
  , "double hs_bindgen_test_functionssimple_func_41ddff0def5e758e ("
  , "  double arg1,"
  , "  double arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return bad_fma(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_test_functionssimple_func_ebeb06ac43ef8e19 (void)"
  , "{"
  , "  no_args();"
  , "}"
  , "void hs_bindgen_test_functionssimple_func_e9b7b21f91fcea4c (void)"
  , "{"
  , "  no_args_no_void();"
  , "}"
  , "signed int hs_bindgen_test_functionssimple_func_46fc7e0d859c341d ("
  , "  char arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return fun(arg1, arg2);"
  , "}"
  ]))

{-| __C declaration:__ @erf@

    __defined at:__ @functions\/simple_func.h:1:8@

    __exported by:__ @functions\/simple_func.h@

    __unique:__ @Example_Unsafe_erf@
-}
foreign import ccall unsafe "hs_bindgen_test_functionssimple_func_a9fb5e2e602effce" erf ::
     FC.CDouble
     {- ^ __C declaration:__ @arg@
     -}
  -> FC.CDouble

{-| __C declaration:__ @bad_fma@

    __defined at:__ @functions\/simple_func.h:3:22@

    __exported by:__ @functions\/simple_func.h@

    __unique:__ @Example_Unsafe_bad_fma@
-}
foreign import ccall unsafe "hs_bindgen_test_functionssimple_func_41ddff0def5e758e" bad_fma ::
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

    __defined at:__ @functions\/simple_func.h:7:6@

    __exported by:__ @functions\/simple_func.h@

    __unique:__ @Example_Unsafe_no_args@
-}
foreign import ccall unsafe "hs_bindgen_test_functionssimple_func_ebeb06ac43ef8e19" no_args ::
     IO ()

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @functions\/simple_func.h:9:6@

    __exported by:__ @functions\/simple_func.h@

    __unique:__ @Example_Unsafe_no_args_no_void@
-}
foreign import ccall unsafe "hs_bindgen_test_functionssimple_func_e9b7b21f91fcea4c" no_args_no_void ::
     IO ()

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/simple_func.h:11:5@

    __exported by:__ @functions\/simple_func.h@

    __unique:__ @Example_Unsafe_fun@
-}
foreign import ccall unsafe "hs_bindgen_test_functionssimple_func_46fc7e0d859c341d" fun ::
     FC.CChar
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @y@
     -}
  -> IO FC.CInt
