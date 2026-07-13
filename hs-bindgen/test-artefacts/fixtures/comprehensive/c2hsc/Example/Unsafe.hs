{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo_function
    , Example.Unsafe.foo_33_1
    , Example.Unsafe.foo_33_2
    , Example.Unsafe.foo_33_3
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_comprehensivec2hsc_Example_Unsafe_foo_function@
hs_bindgen_bf542a744554a894 ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO ()
hs_bindgen_bf542a744554a894 =
  BG.fromFFIType hs_bindgen_bf542a744554a894_base

{-| __C declaration:__ @foo_function@

    __defined at:__ @comprehensive\/c2hsc.h 38:6@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
foo_function ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
     -- ^ __C declaration:__ @foo@
  -> IO ()
foo_function = hs_bindgen_bf542a744554a894

-- __unique:__ @test_comprehensivec2hsc_Example_Unsafe_foo_33_1@
foreign import ccall unsafe "hs_bindgen_11ca53f1ea9c8230" hs_bindgen_11ca53f1ea9c8230_base ::
     BG.Word32
  -> IO BG.Word32

-- __unique:__ @test_comprehensivec2hsc_Example_Unsafe_foo_33_1@
hs_bindgen_11ca53f1ea9c8230 ::
     BG.CUInt
  -> IO BG.CUInt
hs_bindgen_11ca53f1ea9c8230 =
  BG.fromFFIType hs_bindgen_11ca53f1ea9c8230_base

{-| __C declaration:__ @foo_33_1@

    __defined at:__ @comprehensive\/c2hsc.h 73:10@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
foo_33_1 ::
     BG.CUInt
  -> IO BG.CUInt
foo_33_1 = hs_bindgen_11ca53f1ea9c8230

-- __unique:__ @test_comprehensivec2hsc_Example_Unsafe_foo_33_2@
foreign import ccall unsafe "hs_bindgen_2685a75b0196568e" hs_bindgen_2685a75b0196568e_base ::
     BG.Word32
  -> IO BG.Word32

-- __unique:__ @test_comprehensivec2hsc_Example_Unsafe_foo_33_2@
hs_bindgen_2685a75b0196568e ::
     BG.CUInt
  -> IO BG.CUInt
hs_bindgen_2685a75b0196568e =
  BG.fromFFIType hs_bindgen_2685a75b0196568e_base

{-| __C declaration:__ @foo_33_2@

    __defined at:__ @comprehensive\/c2hsc.h 74:51@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
foo_33_2 ::
     BG.CUInt
  -> IO BG.CUInt
foo_33_2 = hs_bindgen_2685a75b0196568e

-- __unique:__ @test_comprehensivec2hsc_Example_Unsafe_foo_33_3@
foreign import ccall unsafe "hs_bindgen_bc94ff083e65f7e0" hs_bindgen_bc94ff083e65f7e0_base ::
     BG.Int64
  -> IO BG.Int64

-- __unique:__ @test_comprehensivec2hsc_Example_Unsafe_foo_33_3@
hs_bindgen_bc94ff083e65f7e0 ::
     BG.CLLong
  -> IO BG.CLLong
hs_bindgen_bc94ff083e65f7e0 =
  BG.fromFFIType hs_bindgen_bc94ff083e65f7e0_base

{-| __C declaration:__ @foo_33_3@

    __defined at:__ @comprehensive\/c2hsc.h 75:52@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
foo_33_3 ::
     BG.CLLong
  -> IO BG.CLLong
foo_33_3 = hs_bindgen_bc94ff083e65f7e0
