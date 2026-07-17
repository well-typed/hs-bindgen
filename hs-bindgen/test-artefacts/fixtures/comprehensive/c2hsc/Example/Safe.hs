{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo_function
    , Example.Safe.foo_33_1
    , Example.Safe.foo_33_2
    , Example.Safe.foo_33_3
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <comprehensive/c2hsc.h>"
  , "void hs_bindgen_3fd2a5c6e681c44b ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (foo_function)(arg1);"
  , "}"
  , "unsigned int hs_bindgen_7978c370c45d3898 ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return (foo_33_1)(arg1);"
  , "}"
  , "unsigned int hs_bindgen_44aef5175928343d ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return (foo_33_2)(arg1);"
  , "}"
  , "signed long long hs_bindgen_9f0b65c4b469b3d8 ("
  , "  signed long long arg1"
  , ")"
  , "{"
  , "  return (foo_33_3)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_comprehensivec2hsc_Example_Safe_foo_function@
foreign import ccall safe "hs_bindgen_3fd2a5c6e681c44b" hs_bindgen_3fd2a5c6e681c44b_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_comprehensivec2hsc_Example_Safe_foo_function@
hs_bindgen_3fd2a5c6e681c44b ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO ()
hs_bindgen_3fd2a5c6e681c44b =
  BG.fromFFIType hs_bindgen_3fd2a5c6e681c44b_base

{-| __C declaration:__ @foo_function@

    __defined at:__ @comprehensive\/c2hsc.h 38:6@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
foo_function ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
     -- ^ __C declaration:__ @foo@
  -> IO ()
foo_function = hs_bindgen_3fd2a5c6e681c44b

-- __unique:__ @test_comprehensivec2hsc_Example_Safe_foo_33_1@
foreign import ccall safe "hs_bindgen_7978c370c45d3898" hs_bindgen_7978c370c45d3898_base ::
     BG.Word32
  -> IO BG.Word32

-- __unique:__ @test_comprehensivec2hsc_Example_Safe_foo_33_1@
hs_bindgen_7978c370c45d3898 ::
     BG.CUInt
  -> IO BG.CUInt
hs_bindgen_7978c370c45d3898 =
  BG.fromFFIType hs_bindgen_7978c370c45d3898_base

{-| __C declaration:__ @foo_33_1@

    __defined at:__ @comprehensive\/c2hsc.h 73:10@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
foo_33_1 ::
     BG.CUInt
  -> IO BG.CUInt
foo_33_1 = hs_bindgen_7978c370c45d3898

-- __unique:__ @test_comprehensivec2hsc_Example_Safe_foo_33_2@
foreign import ccall safe "hs_bindgen_44aef5175928343d" hs_bindgen_44aef5175928343d_base ::
     BG.Word32
  -> IO BG.Word32

-- __unique:__ @test_comprehensivec2hsc_Example_Safe_foo_33_2@
hs_bindgen_44aef5175928343d ::
     BG.CUInt
  -> IO BG.CUInt
hs_bindgen_44aef5175928343d =
  BG.fromFFIType hs_bindgen_44aef5175928343d_base

{-| __C declaration:__ @foo_33_2@

    __defined at:__ @comprehensive\/c2hsc.h 74:51@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
foo_33_2 ::
     BG.CUInt
  -> IO BG.CUInt
foo_33_2 = hs_bindgen_44aef5175928343d

-- __unique:__ @test_comprehensivec2hsc_Example_Safe_foo_33_3@
foreign import ccall safe "hs_bindgen_9f0b65c4b469b3d8" hs_bindgen_9f0b65c4b469b3d8_base ::
     BG.Int64
  -> IO BG.Int64

-- __unique:__ @test_comprehensivec2hsc_Example_Safe_foo_33_3@
hs_bindgen_9f0b65c4b469b3d8 ::
     BG.CLLong
  -> IO BG.CLLong
hs_bindgen_9f0b65c4b469b3d8 =
  BG.fromFFIType hs_bindgen_9f0b65c4b469b3d8_base

{-| __C declaration:__ @foo_33_3@

    __defined at:__ @comprehensive\/c2hsc.h 75:52@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
foo_33_3 ::
     BG.CLLong
  -> IO BG.CLLong
foo_33_3 = hs_bindgen_9f0b65c4b469b3d8
