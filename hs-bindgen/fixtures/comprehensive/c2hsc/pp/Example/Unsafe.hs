{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo_function
    , Example.Unsafe.foo_33_1
    , Example.Unsafe.foo_33_2
    , Example.Unsafe.foo_33_3
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <comprehensive/c2hsc.h>"
  , "void hs_bindgen_bf542a744554a894 ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (foo_function)(arg1);"
  , "}"
  , "unsigned int hs_bindgen_11ca53f1ea9c8230 ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return (foo_33_1)(arg1);"
  , "}"
  , "unsigned int hs_bindgen_2685a75b0196568e ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return (foo_33_2)(arg1);"
  , "}"
  , "signed long long hs_bindgen_bc94ff083e65f7e0 ("
  , "  signed long long arg1"
  , ")"
  , "{"
  , "  return (foo_33_3)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_comprehensivec2hsc_Example_Unsafe_foo_function@
foreign import ccall unsafe "hs_bindgen_bf542a744554a894" hs_bindgen_bf542a744554a894_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_comprehensivec2hsc_Example_Unsafe_foo_function@
hs_bindgen_bf542a744554a894 ::
     RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
  -> IO ()
hs_bindgen_bf542a744554a894 =
  RIP.fromFFIType hs_bindgen_bf542a744554a894_base

{-| __C declaration:__ @foo_function@

    __defined at:__ @comprehensive\/c2hsc.h 38:6@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
foo_function ::
     RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
     -- ^ __C declaration:__ @foo@
  -> IO ()
foo_function = hs_bindgen_bf542a744554a894

-- __unique:__ @test_comprehensivec2hsc_Example_Unsafe_foo_33_1@
foreign import ccall unsafe "hs_bindgen_11ca53f1ea9c8230" hs_bindgen_11ca53f1ea9c8230_base ::
     RIP.Word32
  -> IO RIP.Word32

-- __unique:__ @test_comprehensivec2hsc_Example_Unsafe_foo_33_1@
hs_bindgen_11ca53f1ea9c8230 ::
     RIP.CUInt
  -> IO RIP.CUInt
hs_bindgen_11ca53f1ea9c8230 =
  RIP.fromFFIType hs_bindgen_11ca53f1ea9c8230_base

{-| __C declaration:__ @foo_33_1@

    __defined at:__ @comprehensive\/c2hsc.h 73:10@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
foo_33_1 ::
     RIP.CUInt
  -> IO RIP.CUInt
foo_33_1 = hs_bindgen_11ca53f1ea9c8230

-- __unique:__ @test_comprehensivec2hsc_Example_Unsafe_foo_33_2@
foreign import ccall unsafe "hs_bindgen_2685a75b0196568e" hs_bindgen_2685a75b0196568e_base ::
     RIP.Word32
  -> IO RIP.Word32

-- __unique:__ @test_comprehensivec2hsc_Example_Unsafe_foo_33_2@
hs_bindgen_2685a75b0196568e ::
     RIP.CUInt
  -> IO RIP.CUInt
hs_bindgen_2685a75b0196568e =
  RIP.fromFFIType hs_bindgen_2685a75b0196568e_base

{-| __C declaration:__ @foo_33_2@

    __defined at:__ @comprehensive\/c2hsc.h 74:51@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
foo_33_2 ::
     RIP.CUInt
  -> IO RIP.CUInt
foo_33_2 = hs_bindgen_2685a75b0196568e

-- __unique:__ @test_comprehensivec2hsc_Example_Unsafe_foo_33_3@
foreign import ccall unsafe "hs_bindgen_bc94ff083e65f7e0" hs_bindgen_bc94ff083e65f7e0_base ::
     RIP.Int64
  -> IO RIP.Int64

-- __unique:__ @test_comprehensivec2hsc_Example_Unsafe_foo_33_3@
hs_bindgen_bc94ff083e65f7e0 ::
     RIP.CLLong
  -> IO RIP.CLLong
hs_bindgen_bc94ff083e65f7e0 =
  RIP.fromFFIType hs_bindgen_bc94ff083e65f7e0_base

{-| __C declaration:__ @foo_33_3@

    __defined at:__ @comprehensive\/c2hsc.h 75:52@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
foo_33_3 ::
     RIP.CLLong
  -> IO RIP.CLLong
foo_33_3 = hs_bindgen_bc94ff083e65f7e0
