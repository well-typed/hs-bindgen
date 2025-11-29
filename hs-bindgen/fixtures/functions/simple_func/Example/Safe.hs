{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/simple_func.h>"
  , "double hs_bindgen_test_functionssimple_func_54020b261f5609f1 ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return erf(arg1);"
  , "}"
  , "double hs_bindgen_test_functionssimple_func_14d8e1fc3093ef05 ("
  , "  double arg1,"
  , "  double arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return bad_fma(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_test_functionssimple_func_7d0ceafab567ccb7 (void)"
  , "{"
  , "  no_args();"
  , "}"
  , "void hs_bindgen_test_functionssimple_func_090f829d86c0108c (void)"
  , "{"
  , "  no_args_no_void();"
  , "}"
  , "signed int hs_bindgen_test_functionssimple_func_ee985d516e39a879 ("
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

    __unique:__ @Example_Safe_erf@
-}
foreign import ccall safe "hs_bindgen_test_functionssimple_func_54020b261f5609f1" erf ::
     FC.CDouble
     {- ^ __C declaration:__ @arg@
     -}
  -> FC.CDouble

{-| __C declaration:__ @bad_fma@

    __defined at:__ @functions\/simple_func.h:3:22@

    __exported by:__ @functions\/simple_func.h@

    __unique:__ @Example_Safe_bad_fma@
-}
foreign import ccall safe "hs_bindgen_test_functionssimple_func_14d8e1fc3093ef05" bad_fma ::
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

    __unique:__ @Example_Safe_no_args@
-}
foreign import ccall safe "hs_bindgen_test_functionssimple_func_7d0ceafab567ccb7" no_args ::
     IO ()

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @functions\/simple_func.h:9:6@

    __exported by:__ @functions\/simple_func.h@

    __unique:__ @Example_Safe_no_args_no_void@
-}
foreign import ccall safe "hs_bindgen_test_functionssimple_func_090f829d86c0108c" no_args_no_void ::
     IO ()

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/simple_func.h:11:5@

    __exported by:__ @functions\/simple_func.h@

    __unique:__ @Example_Safe_fun@
-}
foreign import ccall safe "hs_bindgen_test_functionssimple_func_ee985d516e39a879" fun ::
     FC.CChar
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @y@
     -}
  -> IO FC.CInt
