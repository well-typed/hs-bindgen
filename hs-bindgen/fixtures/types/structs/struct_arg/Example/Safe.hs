{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/structs/struct_arg.h>"
  , "signed int hs_bindgen_test_typesstructsstruct_arg_61dfa2c4506feb8f ("
  , "  struct thing *arg1"
  , ")"
  , "{"
  , "  return thing_fun_1(*arg1);"
  , "}"
  , "void hs_bindgen_test_typesstructsstruct_arg_022cc8107f565c95 ("
  , "  signed int arg1,"
  , "  struct thing *arg2"
  , ")"
  , "{"
  , "  *arg2 = thing_fun_2(arg1);"
  , "}"
  , "void hs_bindgen_test_typesstructsstruct_arg_4d9304280cca3098 ("
  , "  signed int arg1,"
  , "  struct thing *arg2,"
  , "  double arg3,"
  , "  struct thing *arg4"
  , ")"
  , "{"
  , "  *arg4 = thing_fun_3a(arg1, *arg2, arg3);"
  , "}"
  , "char hs_bindgen_test_typesstructsstruct_arg_f39687b254852452 ("
  , "  signed int arg1,"
  , "  struct thing *arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return thing_fun_3b(arg1, *arg2, arg3);"
  , "}"
  ]))

{-| Pointer-based API for 'thing_fun_1'

-}
foreign import ccall safe "hs_bindgen_test_typesstructsstruct_arg_61dfa2c4506feb8f" thing_fun_1_wrapper ::
     Ptr.Ptr Thing
  -> IO FC.CInt

{-| __C declaration:__ @thing_fun_1@

    __defined at:__ @types\/structs\/struct_arg.h:6:5@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_1 ::
     Thing
     {- ^ __C declaration:__ @x@
     -}
  -> IO FC.CInt
thing_fun_1 =
  \x0 -> F.with x0 (\y1 -> thing_fun_1_wrapper y1)

{-| Pointer-based API for 'thing_fun_2'

-}
foreign import ccall safe "hs_bindgen_test_typesstructsstruct_arg_022cc8107f565c95" thing_fun_2_wrapper ::
     FC.CInt
  -> Ptr.Ptr Thing
  -> IO ()

{-| __C declaration:__ @thing_fun_2@

    __defined at:__ @types\/structs\/struct_arg.h:7:14@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_2 ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> IO Thing
thing_fun_2 =
  \x0 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\z1 ->
                                            thing_fun_2_wrapper x0 z1)

{-| Pointer-based API for 'thing_fun_3a'

-}
foreign import ccall safe "hs_bindgen_test_typesstructsstruct_arg_4d9304280cca3098" thing_fun_3a_wrapper ::
     FC.CInt
  -> Ptr.Ptr Thing
  -> FC.CDouble
  -> Ptr.Ptr Thing
  -> IO ()

{-| __C declaration:__ @thing_fun_3a@

    __defined at:__ @types\/structs\/struct_arg.h:9:14@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_3a ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> Thing
     {- ^ __C declaration:__ @y@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @z@
     -}
  -> IO Thing
thing_fun_3a =
  \x0 ->
    \x1 ->
      \x2 ->
        F.with x1 (\y3 ->
                     HsBindgen.Runtime.CAPI.allocaAndPeek (\z4 ->
                                                             thing_fun_3a_wrapper x0 y3 x2 z4))

{-| Pointer-based API for 'thing_fun_3b'

-}
foreign import ccall safe "hs_bindgen_test_typesstructsstruct_arg_f39687b254852452" thing_fun_3b_wrapper ::
     FC.CInt
  -> Ptr.Ptr Thing
  -> FC.CDouble
  -> IO FC.CChar

{-| __C declaration:__ @thing_fun_3b@

    __defined at:__ @types\/structs\/struct_arg.h:10:6@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_3b ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> Thing
     {- ^ __C declaration:__ @y@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @z@
     -}
  -> IO FC.CChar
thing_fun_3b =
  \x0 ->
    \x1 ->
      \x2 ->
        F.with x1 (\y3 -> thing_fun_3b_wrapper x0 y3 x2)
