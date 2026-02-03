{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Data.Void (Void)
import Example
import Prelude (Double, IO)

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
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_typesstructsstruct_arg_Example_Safe_thing_fun_1@
hs_bindgen_4ad25504590fdd2b ::
     Ptr.Ptr Thing
  -> IO FC.CInt
hs_bindgen_4ad25504590fdd2b =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_4ad25504590fdd2b_base

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
    F.with x0 (\x1 -> hs_bindgen_4ad25504590fdd2b x1)

-- __unique:__ @test_typesstructsstruct_arg_Example_Safe_thing_fun_2@
foreign import ccall safe "hs_bindgen_04a435522bf64978" hs_bindgen_04a435522bf64978_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_typesstructsstruct_arg_Example_Safe_thing_fun_2@
hs_bindgen_04a435522bf64978 ::
     FC.CInt
  -> Ptr.Ptr Thing
  -> IO ()
hs_bindgen_04a435522bf64978 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_04a435522bf64978_base

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
    HsBindgen.Runtime.Internal.CAPI.allocaAndPeek (\res1 ->
                                                     hs_bindgen_04a435522bf64978 x0 res1)

-- __unique:__ @test_typesstructsstruct_arg_Example_Safe_thing_fun_3a@
foreign import ccall safe "hs_bindgen_5e3271324df7ced2" hs_bindgen_5e3271324df7ced2_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> Double
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_typesstructsstruct_arg_Example_Safe_thing_fun_3a@
hs_bindgen_5e3271324df7ced2 ::
     FC.CInt
  -> Ptr.Ptr Thing
  -> FC.CDouble
  -> Ptr.Ptr Thing
  -> IO ()
hs_bindgen_5e3271324df7ced2 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_5e3271324df7ced2_base

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
    \y1 ->
      \z2 ->
        F.with y1 (\y3 ->
                     HsBindgen.Runtime.Internal.CAPI.allocaAndPeek (\res4 ->
                                                                      hs_bindgen_5e3271324df7ced2 x0 y3 z2 res4))

-- __unique:__ @test_typesstructsstruct_arg_Example_Safe_thing_fun_3b@
foreign import ccall safe "hs_bindgen_3525c7d1c72f2fae" hs_bindgen_3525c7d1c72f2fae_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> Double
  -> IO GHC.Int.Int8

-- __unique:__ @test_typesstructsstruct_arg_Example_Safe_thing_fun_3b@
hs_bindgen_3525c7d1c72f2fae ::
     FC.CInt
  -> Ptr.Ptr Thing
  -> FC.CDouble
  -> IO FC.CChar
hs_bindgen_3525c7d1c72f2fae =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_3525c7d1c72f2fae_base

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
    \y1 ->
      \z2 ->
        F.with y1 (\y3 ->
                     hs_bindgen_3525c7d1c72f2fae x0 y3 z2)
