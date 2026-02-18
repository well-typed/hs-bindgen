{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <edge-cases/spec_examples.h>"
  , "/* test_edgecasesspec_examples_Example_get_resample */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2b8fc800dad87ec8 (void)) ("
  , "  int32_T *arg1,"
  , "  cint16_T arg2[30720000],"
  , "  int64_T arg3,"
  , "  int64_T arg4,"
  , "  cint16_T arg5[30720000]"
  , ")"
  , "{"
  , "  return &resample;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesspec_examples_Example_get_resample@
foreign import ccall unsafe "hs_bindgen_2b8fc800dad87ec8" hs_bindgen_2b8fc800dad87ec8_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_edgecasesspec_examples_Example_get_resample@
hs_bindgen_2b8fc800dad87ec8 :: IO (RIP.FunPtr ((RIP.Ptr Int32_T) -> ((CA.ConstantArray 30720000) Cint16_T) -> Int64_T -> Int64_T -> ((CA.ConstantArray 30720000) Cint16_T) -> IO ()))
hs_bindgen_2b8fc800dad87ec8 =
  RIP.fromFFIType hs_bindgen_2b8fc800dad87ec8_base

{-# NOINLINE resample #-}
{-| __C declaration:__ @resample@

    __defined at:__ @edge-cases\/spec_examples.h 31:6@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
resample :: RIP.FunPtr ((RIP.Ptr Int32_T) -> ((CA.ConstantArray 30720000) Cint16_T) -> Int64_T -> Int64_T -> ((CA.ConstantArray 30720000) Cint16_T) -> IO ())
resample =
  RIP.unsafePerformIO hs_bindgen_2b8fc800dad87ec8
