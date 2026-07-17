{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.test_array
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <edge-cases/enum_as_array_size.h>"
  , "/* test_edgecasesenum_as_array_size_Example_get_test_array */"
  , "__attribute__ ((const))"
  , "char const (*hs_bindgen_30b94bcf7e387817 (void))[1]"
  , "{"
  , "  return &test_array;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesenum_as_array_size_Example_get_test_array@
foreign import ccall unsafe "hs_bindgen_30b94bcf7e387817" hs_bindgen_30b94bcf7e387817_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_edgecasesenum_as_array_size_Example_get_test_array@
hs_bindgen_30b94bcf7e387817 :: IO (PtrConst.PtrConst (CA.ConstantArray 1 BG.CChar))
hs_bindgen_30b94bcf7e387817 =
  BG.fromFFIType hs_bindgen_30b94bcf7e387817_base

{-# NOINLINE hs_bindgen_e30c033f156164cc #-}
{-| __C declaration:__ @test_array@

    __defined at:__ @edge-cases\/enum_as_array_size.h 8:19@

    __exported by:__ @edge-cases\/enum_as_array_size.h@

    __unique:__ @test_edgecasesenum_as_array_size_Example_test_array@
-}
hs_bindgen_e30c033f156164cc :: PtrConst.PtrConst (CA.ConstantArray 1 BG.CChar)
hs_bindgen_e30c033f156164cc =
  BG.unsafePerformIO hs_bindgen_30b94bcf7e387817

{-# NOINLINE test_array #-}
test_array :: CA.ConstantArray 1 BG.CChar
test_array =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_e30c033f156164cc)
