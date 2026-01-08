{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/structs/struct_arg.h>"
  , "signed int hs_bindgen_0bdddf60550fc97b ("
  , "  struct thing *arg1"
  , ")"
  , "{"
  , "  return thing_fun_1(*arg1);"
  , "}"
  , "void hs_bindgen_c293d866e22be6fc ("
  , "  signed int arg1,"
  , "  struct thing *arg2"
  , ")"
  , "{"
  , "  *arg2 = thing_fun_2(arg1);"
  , "}"
  , "void hs_bindgen_cfd51a9e490a997c ("
  , "  signed int arg1,"
  , "  struct thing *arg2,"
  , "  double arg3,"
  , "  struct thing *arg4"
  , ")"
  , "{"
  , "  *arg4 = thing_fun_3a(arg1, *arg2, arg3);"
  , "}"
  , "char hs_bindgen_23fac8ee5044da6e ("
  , "  signed int arg1,"
  , "  struct thing *arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return thing_fun_3b(arg1, *arg2, arg3);"
  , "}"
  ]))

-- __unique:__ @test_typesstructsstruct_arg_Example_Unsafe_thing_fun_1@
foreign import ccall unsafe "hs_bindgen_0bdddf60550fc97b" hs_bindgen_0bdddf60550fc97b ::
     Ptr.Ptr Thing
  -> IO FC.CInt

{-| Pointer-based API for 'thing_fun_1'
-}
thing_fun_1_wrapper ::
     Ptr.Ptr Thing
     -- ^ __C declaration:__ @x@
  -> IO FC.CInt
thing_fun_1_wrapper = hs_bindgen_0bdddf60550fc97b

{-| __C declaration:__ @thing_fun_1@

    __defined at:__ @types\/structs\/struct_arg.h 6:5@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_1 ::
     Thing
     -- ^ __C declaration:__ @x@
  -> IO FC.CInt
thing_fun_1 =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_0bdddf60550fc97b y1)

-- __unique:__ @test_typesstructsstruct_arg_Example_Unsafe_thing_fun_2@
foreign import ccall unsafe "hs_bindgen_c293d866e22be6fc" hs_bindgen_c293d866e22be6fc ::
     FC.CInt
  -> Ptr.Ptr Thing
  -> IO ()

{-| Pointer-based API for 'thing_fun_2'
-}
thing_fun_2_wrapper ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> Ptr.Ptr Thing
  -> IO ()
thing_fun_2_wrapper = hs_bindgen_c293d866e22be6fc

{-| __C declaration:__ @thing_fun_2@

    __defined at:__ @types\/structs\/struct_arg.h 7:14@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_2 ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO Thing
thing_fun_2 =
  \x0 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\z1 ->
                                            hs_bindgen_c293d866e22be6fc x0 z1)

-- __unique:__ @test_typesstructsstruct_arg_Example_Unsafe_thing_fun_3a@
foreign import ccall unsafe "hs_bindgen_cfd51a9e490a997c" hs_bindgen_cfd51a9e490a997c ::
     FC.CInt
  -> Ptr.Ptr Thing
  -> FC.CDouble
  -> Ptr.Ptr Thing
  -> IO ()

{-| Pointer-based API for 'thing_fun_3a'
-}
thing_fun_3a_wrapper ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> Ptr.Ptr Thing
     -- ^ __C declaration:__ @y@
  -> FC.CDouble
     -- ^ __C declaration:__ @z@
  -> Ptr.Ptr Thing
  -> IO ()
thing_fun_3a_wrapper = hs_bindgen_cfd51a9e490a997c

{-| __C declaration:__ @thing_fun_3a@

    __defined at:__ @types\/structs\/struct_arg.h 9:14@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_3a ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> Thing
     -- ^ __C declaration:__ @y@
  -> FC.CDouble
     -- ^ __C declaration:__ @z@
  -> IO Thing
thing_fun_3a =
  \x0 ->
    \x1 ->
      \x2 ->
        F.with x1 (\y3 ->
                     HsBindgen.Runtime.CAPI.allocaAndPeek (\z4 ->
                                                             hs_bindgen_cfd51a9e490a997c x0 y3 x2 z4))

-- __unique:__ @test_typesstructsstruct_arg_Example_Unsafe_thing_fun_3b@
foreign import ccall unsafe "hs_bindgen_23fac8ee5044da6e" hs_bindgen_23fac8ee5044da6e ::
     FC.CInt
  -> Ptr.Ptr Thing
  -> FC.CDouble
  -> IO FC.CChar

{-| Pointer-based API for 'thing_fun_3b'
-}
thing_fun_3b_wrapper ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> Ptr.Ptr Thing
     -- ^ __C declaration:__ @y@
  -> FC.CDouble
     -- ^ __C declaration:__ @z@
  -> IO FC.CChar
thing_fun_3b_wrapper = hs_bindgen_23fac8ee5044da6e

{-| __C declaration:__ @thing_fun_3b@

    __defined at:__ @types\/structs\/struct_arg.h 10:6@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_3b ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> Thing
     -- ^ __C declaration:__ @y@
  -> FC.CDouble
     -- ^ __C declaration:__ @z@
  -> IO FC.CChar
thing_fun_3b =
  \x0 ->
    \x1 ->
      \x2 ->
        F.with x1 (\y3 ->
                     hs_bindgen_23fac8ee5044da6e x0 y3 x2)
