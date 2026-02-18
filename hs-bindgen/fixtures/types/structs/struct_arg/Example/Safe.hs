{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/structs/struct_arg.h>"
  , "signed int hs_bindgen_4ad25504590fdd2b ("
  , "  struct thing *arg1"
  , ")"
  , "{"
  , "  return thing_fun_1(*arg1);"
  , "}"
  , "void hs_bindgen_04a435522bf64978 ("
  , "  signed int arg1,"
  , "  struct thing *arg2"
  , ")"
  , "{"
  , "  *arg2 = thing_fun_2(arg1);"
  , "}"
  , "void hs_bindgen_5e3271324df7ced2 ("
  , "  signed int arg1,"
  , "  struct thing *arg2,"
  , "  double arg3,"
  , "  struct thing *arg4"
  , ")"
  , "{"
  , "  *arg4 = thing_fun_3a(arg1, *arg2, arg3);"
  , "}"
  , "char hs_bindgen_3525c7d1c72f2fae ("
  , "  signed int arg1,"
  , "  struct thing *arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return thing_fun_3b(arg1, *arg2, arg3);"
  , "}"
  ]))

-- __unique:__ @test_typesstructsstruct_arg_Example_Safe_thing_fun_1@
foreign import ccall safe "hs_bindgen_4ad25504590fdd2b" hs_bindgen_4ad25504590fdd2b_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_typesstructsstruct_arg_Example_Safe_thing_fun_1@
hs_bindgen_4ad25504590fdd2b ::
     RIP.Ptr Thing
  -> IO RIP.CInt
hs_bindgen_4ad25504590fdd2b =
  RIP.fromFFIType hs_bindgen_4ad25504590fdd2b_base

{-| __C declaration:__ @thing_fun_1@

    __defined at:__ @types\/structs\/struct_arg.h 6:5@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_1 ::
     Thing
     -- ^ __C declaration:__ @x@
  -> IO RIP.CInt
thing_fun_1 =
  \x0 ->
    RIP.with x0 (\x1 -> hs_bindgen_4ad25504590fdd2b x1)

-- __unique:__ @test_typesstructsstruct_arg_Example_Safe_thing_fun_2@
foreign import ccall safe "hs_bindgen_04a435522bf64978" hs_bindgen_04a435522bf64978_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_typesstructsstruct_arg_Example_Safe_thing_fun_2@
hs_bindgen_04a435522bf64978 ::
     RIP.CInt
  -> RIP.Ptr Thing
  -> IO ()
hs_bindgen_04a435522bf64978 =
  RIP.fromFFIType hs_bindgen_04a435522bf64978_base

{-| __C declaration:__ @thing_fun_2@

    __defined at:__ @types\/structs\/struct_arg.h 7:14@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_2 ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO Thing
thing_fun_2 =
  \x0 ->
    RIP.allocaAndPeek (\res1 ->
                         hs_bindgen_04a435522bf64978 x0 res1)

-- __unique:__ @test_typesstructsstruct_arg_Example_Safe_thing_fun_3a@
foreign import ccall safe "hs_bindgen_5e3271324df7ced2" hs_bindgen_5e3271324df7ced2_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> Double
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_typesstructsstruct_arg_Example_Safe_thing_fun_3a@
hs_bindgen_5e3271324df7ced2 ::
     RIP.CInt
  -> RIP.Ptr Thing
  -> RIP.CDouble
  -> RIP.Ptr Thing
  -> IO ()
hs_bindgen_5e3271324df7ced2 =
  RIP.fromFFIType hs_bindgen_5e3271324df7ced2_base

{-| __C declaration:__ @thing_fun_3a@

    __defined at:__ @types\/structs\/struct_arg.h 9:14@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_3a ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> Thing
     -- ^ __C declaration:__ @y@
  -> RIP.CDouble
     -- ^ __C declaration:__ @z@
  -> IO Thing
thing_fun_3a =
  \x0 ->
    \y1 ->
      \z2 ->
        RIP.with y1 (\y3 ->
                       RIP.allocaAndPeek (\res4 ->
                                            hs_bindgen_5e3271324df7ced2 x0 y3 z2 res4))

-- __unique:__ @test_typesstructsstruct_arg_Example_Safe_thing_fun_3b@
foreign import ccall safe "hs_bindgen_3525c7d1c72f2fae" hs_bindgen_3525c7d1c72f2fae_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> Double
  -> IO RIP.Int8

-- __unique:__ @test_typesstructsstruct_arg_Example_Safe_thing_fun_3b@
hs_bindgen_3525c7d1c72f2fae ::
     RIP.CInt
  -> RIP.Ptr Thing
  -> RIP.CDouble
  -> IO RIP.CChar
hs_bindgen_3525c7d1c72f2fae =
  RIP.fromFFIType hs_bindgen_3525c7d1c72f2fae_base

{-| __C declaration:__ @thing_fun_3b@

    __defined at:__ @types\/structs\/struct_arg.h 10:6@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_3b ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> Thing
     -- ^ __C declaration:__ @y@
  -> RIP.CDouble
     -- ^ __C declaration:__ @z@
  -> IO RIP.CChar
thing_fun_3b =
  \x0 ->
    \y1 ->
      \z2 ->
        RIP.with y1 (\y3 ->
                       hs_bindgen_3525c7d1c72f2fae x0 y3 z2)
