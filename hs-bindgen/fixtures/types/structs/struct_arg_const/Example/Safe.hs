{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign as F
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/structs/struct_arg_const.h>"
  , "void hs_bindgen_6cb8b51c110839ff ("
  , "  struct thing const *arg1"
  , ")"
  , "{"
  , "  fun_const_arg(*arg1);"
  , "}"
  , "void hs_bindgen_44ee619fd7228cf2 ("
  , "  struct thing const *arg1"
  , ")"
  , "{"
  , "  *arg1 = fun_const_result();"
  , "}"
  , "void hs_bindgen_3794a17277299018 ("
  , "  struct thing const *arg1,"
  , "  struct thing const *arg2"
  , ")"
  , "{"
  , "  *arg2 = fun_const(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_typesstructsstruct_arg_const_Example_Safe_fun_const_arg@
foreign import ccall safe "hs_bindgen_6cb8b51c110839ff" hs_bindgen_6cb8b51c110839ff ::
     HsBindgen.Runtime.ConstPtr.ConstPtr Thing
  -> IO ()

{-| Pointer-based API for 'fun_const_arg'
-}
fun_const_arg_wrapper ::
     HsBindgen.Runtime.ConstPtr.ConstPtr Thing
     -- ^ __C declaration:__ @x@
  -> IO ()
fun_const_arg_wrapper = hs_bindgen_6cb8b51c110839ff

{-| __C declaration:__ @fun_const_arg@

    __defined at:__ @types\/structs\/struct_arg_const.h:7:20@

    __exported by:__ @types\/structs\/struct_arg_const.h@
-}
fun_const_arg ::
     Thing
     -- ^ __C declaration:__ @x@
  -> IO ()
fun_const_arg =
  \x0 ->
    F.with x0 (\y1 ->
                 hs_bindgen_6cb8b51c110839ff (HsBindgen.Runtime.ConstPtr.ConstPtr y1))

-- __unique:__ @test_typesstructsstruct_arg_const_Example_Safe_fun_const_result@
foreign import ccall safe "hs_bindgen_44ee619fd7228cf2" hs_bindgen_44ee619fd7228cf2 ::
     HsBindgen.Runtime.ConstPtr.ConstPtr Thing
  -> IO ()

{-| Pointer-based API for 'fun_const_result'
-}
fun_const_result_wrapper ::
     HsBindgen.Runtime.ConstPtr.ConstPtr Thing
  -> IO ()
fun_const_result_wrapper =
  hs_bindgen_44ee619fd7228cf2

{-| __C declaration:__ @fun_const_result@

    __defined at:__ @types\/structs\/struct_arg_const.h:8:20@

    __exported by:__ @types\/structs\/struct_arg_const.h@
-}
fun_const_result :: IO Thing
fun_const_result =
  HsBindgen.Runtime.CAPI.allocaAndPeekConst (\z0 ->
                                               hs_bindgen_44ee619fd7228cf2 z0)

-- __unique:__ @test_typesstructsstruct_arg_const_Example_Safe_fun_const@
foreign import ccall safe "hs_bindgen_3794a17277299018" hs_bindgen_3794a17277299018 ::
     HsBindgen.Runtime.ConstPtr.ConstPtr Thing
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Thing
  -> IO ()

{-| Pointer-based API for 'fun_const'
-}
fun_const_wrapper ::
     HsBindgen.Runtime.ConstPtr.ConstPtr Thing
     -- ^ __C declaration:__ @x@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Thing
  -> IO ()
fun_const_wrapper = hs_bindgen_3794a17277299018

{-| __C declaration:__ @fun_const@

    __defined at:__ @types\/structs\/struct_arg_const.h:9:20@

    __exported by:__ @types\/structs\/struct_arg_const.h@
-}
fun_const ::
     Thing
     -- ^ __C declaration:__ @x@
  -> IO Thing
fun_const =
  \x0 ->
    F.with x0 (\y1 ->
                 HsBindgen.Runtime.CAPI.allocaAndPeekConst (\z2 ->
                                                              hs_bindgen_3794a17277299018 (HsBindgen.Runtime.ConstPtr.ConstPtr y1) z2))
