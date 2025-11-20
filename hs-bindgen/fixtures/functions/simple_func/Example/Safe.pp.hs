{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/simple_func.h>"
  , "double hs_bindgen_test_functionssimple_func_4b858faf89c6033a ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return erf(arg1);"
  , "}"
  , "double hs_bindgen_test_functionssimple_func_175251e70d29cd73 ("
  , "  double arg1,"
  , "  double arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return bad_fma(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_test_functionssimple_func_bda1aaa13afe437a (void)"
  , "{"
  , "  no_args();"
  , "}"
  , "void hs_bindgen_test_functionssimple_func_8d4283a1963012db (void)"
  , "{"
  , "  no_args_no_void();"
  , "}"
  , "signed int hs_bindgen_test_functionssimple_func_51ad12b64aea929d ("
  , "  char arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return fun(arg1, arg2);"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_functionssimple_func_4b858faf89c6033a" erf_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CDouble
    -> IO FC.CDouble
    )

{-| __C declaration:__ @erf@

    __defined at:__ @functions\/simple_func.h:1:8@

    __exported by:__ @functions\/simple_func.h@
-}
erf ::
     FC.CDouble
     {- ^ __C declaration:__ @arg@
     -}
  -> IO FC.CDouble
erf =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType erf_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_functionssimple_func_175251e70d29cd73" bad_fma_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CDouble
    -> FC.CDouble
    -> FC.CDouble
    -> IO FC.CDouble
    )

{-| __C declaration:__ @bad_fma@

    __defined at:__ @functions\/simple_func.h:3:22@

    __exported by:__ @functions\/simple_func.h@
-}
bad_fma ::
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
bad_fma =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType bad_fma_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_functionssimple_func_bda1aaa13afe437a" no_args_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @no_args@

    __defined at:__ @functions\/simple_func.h:7:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args ::
     IO ()
no_args =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType no_args_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_functionssimple_func_8d4283a1963012db" no_args_no_void_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @functions\/simple_func.h:9:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args_no_void ::
     IO ()
no_args_no_void =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType no_args_no_void_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_functionssimple_func_51ad12b64aea929d" fun_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CChar
    -> FC.CDouble
    -> IO FC.CInt
    )

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/simple_func.h:11:5@

    __exported by:__ @functions\/simple_func.h@
-}
fun ::
     FC.CChar
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @y@
     -}
  -> IO FC.CInt
fun =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_base
