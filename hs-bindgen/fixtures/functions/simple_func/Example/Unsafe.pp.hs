{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/simple_func.h>"
  , "double hs_bindgen_test_functionssimple_func_95132d8ec7fd8434 ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return erf(arg1);"
  , "}"
  , "double hs_bindgen_test_functionssimple_func_a5a0efe959abb90a ("
  , "  double arg1,"
  , "  double arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return bad_fma(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_test_functionssimple_func_3a78cdef137ffe84 (void)"
  , "{"
  , "  no_args();"
  , "}"
  , "void hs_bindgen_test_functionssimple_func_26de47450bbc61e3 (void)"
  , "{"
  , "  no_args_no_void();"
  , "}"
  , "signed int hs_bindgen_test_functionssimple_func_8735e0253ab4e9ad ("
  , "  char arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return fun(arg1, arg2);"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionssimple_func_95132d8ec7fd8434" erf_base ::
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
foreign import ccall unsafe "hs_bindgen_test_functionssimple_func_a5a0efe959abb90a" bad_fma_base ::
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
foreign import ccall unsafe "hs_bindgen_test_functionssimple_func_3a78cdef137ffe84" no_args_base ::
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
foreign import ccall unsafe "hs_bindgen_test_functionssimple_func_26de47450bbc61e3" no_args_no_void_base ::
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
foreign import ccall unsafe "hs_bindgen_test_functionssimple_func_8735e0253ab4e9ad" fun_base ::
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
