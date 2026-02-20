{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/structs/struct_arg.h>"
  , "signed int hs_bindgen_0bdddf60550fc97b ("
  , "  struct thing *arg1"
  , ")"
  , "{"
  , "  return (thing_fun_1)(*arg1);"
  , "}"
  , "void hs_bindgen_c293d866e22be6fc ("
  , "  signed int arg1,"
  , "  struct thing *arg2"
  , ")"
  , "{"
  , "  *arg2 = (thing_fun_2)(arg1);"
  , "}"
  , "void hs_bindgen_cfd51a9e490a997c ("
  , "  signed int arg1,"
  , "  struct thing *arg2,"
  , "  double arg3,"
  , "  struct thing *arg4"
  , ")"
  , "{"
  , "  *arg4 = (thing_fun_3a)(arg1, *arg2, arg3);"
  , "}"
  , "char hs_bindgen_23fac8ee5044da6e ("
  , "  signed int arg1,"
  , "  struct thing *arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return (thing_fun_3b)(arg1, *arg2, arg3);"
  , "}"
  ]))

-- __unique:__ @test_typesstructsstruct_arg_Example_Unsafe_thing_fun_1@
foreign import ccall unsafe "hs_bindgen_0bdddf60550fc97b" hs_bindgen_0bdddf60550fc97b_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_typesstructsstruct_arg_Example_Unsafe_thing_fun_1@
hs_bindgen_0bdddf60550fc97b ::
     RIP.Ptr Thing
  -> IO RIP.CInt
hs_bindgen_0bdddf60550fc97b =
  RIP.fromFFIType hs_bindgen_0bdddf60550fc97b_base

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
    RIP.with x0 (\x1 -> hs_bindgen_0bdddf60550fc97b x1)

-- __unique:__ @test_typesstructsstruct_arg_Example_Unsafe_thing_fun_2@
foreign import ccall unsafe "hs_bindgen_c293d866e22be6fc" hs_bindgen_c293d866e22be6fc_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_typesstructsstruct_arg_Example_Unsafe_thing_fun_2@
hs_bindgen_c293d866e22be6fc ::
     RIP.CInt
  -> RIP.Ptr Thing
  -> IO ()
hs_bindgen_c293d866e22be6fc =
  RIP.fromFFIType hs_bindgen_c293d866e22be6fc_base

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
                         hs_bindgen_c293d866e22be6fc x0 res1)

-- __unique:__ @test_typesstructsstruct_arg_Example_Unsafe_thing_fun_3a@
foreign import ccall unsafe "hs_bindgen_cfd51a9e490a997c" hs_bindgen_cfd51a9e490a997c_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> Double
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_typesstructsstruct_arg_Example_Unsafe_thing_fun_3a@
hs_bindgen_cfd51a9e490a997c ::
     RIP.CInt
  -> RIP.Ptr Thing
  -> RIP.CDouble
  -> RIP.Ptr Thing
  -> IO ()
hs_bindgen_cfd51a9e490a997c =
  RIP.fromFFIType hs_bindgen_cfd51a9e490a997c_base

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
                                            hs_bindgen_cfd51a9e490a997c x0 y3 z2 res4))

-- __unique:__ @test_typesstructsstruct_arg_Example_Unsafe_thing_fun_3b@
foreign import ccall unsafe "hs_bindgen_23fac8ee5044da6e" hs_bindgen_23fac8ee5044da6e_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> Double
  -> IO RIP.Int8

-- __unique:__ @test_typesstructsstruct_arg_Example_Unsafe_thing_fun_3b@
hs_bindgen_23fac8ee5044da6e ::
     RIP.CInt
  -> RIP.Ptr Thing
  -> RIP.CDouble
  -> IO RIP.CChar
hs_bindgen_23fac8ee5044da6e =
  RIP.fromFFIType hs_bindgen_23fac8ee5044da6e_base

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
                       hs_bindgen_23fac8ee5044da6e x0 y3 z2)
