{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.resample
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <edge-cases/spec_examples.h>"
  , "/* test_edgecasesspec_examples_Example_get_resample */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2b8fc800dad87ec8 (void)) ("
  , "  int32_T *arg1,"
  , "  cint16_T *arg2,"
  , "  int64_T arg3,"
  , "  int64_T arg4,"
  , "  cint16_T *arg5"
  , ")"
  , "{"
  , "  return &resample;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesspec_examples_Example_get_resample@
foreign import ccall unsafe "hs_bindgen_2b8fc800dad87ec8" hs_bindgen_2b8fc800dad87ec8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesspec_examples_Example_get_resample@
hs_bindgen_2b8fc800dad87ec8 :: IO (BG.FunPtr (BG.Ptr Int32_T -> BG.Ptr (IsA.Elem (CA.ConstantArray 30720000 Cint16_T)) -> Int64_T -> Int64_T -> BG.Ptr (IsA.Elem (CA.ConstantArray 30720000 Cint16_T)) -> IO ()))
hs_bindgen_2b8fc800dad87ec8 =
  BG.fromFFIType hs_bindgen_2b8fc800dad87ec8_base

{-# NOINLINE resample #-}
{-| __C declaration:__ @resample@

    __defined at:__ @edge-cases\/spec_examples.h 31:6@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
resample :: BG.FunPtr (BG.Ptr Int32_T -> BG.Ptr (IsA.Elem (CA.ConstantArray 30720000 Cint16_T)) -> Int64_T -> Int64_T -> BG.Ptr (IsA.Elem (CA.ConstantArray 30720000 Cint16_T)) -> IO ())
resample =
  BG.unsafePerformIO hs_bindgen_2b8fc800dad87ec8
