{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.resample
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <edge-cases/spec_examples.h>"
  , "void hs_bindgen_8a72aafc705daf44 ("
  , "  int32_T *arg1,"
  , "  cint16_T *arg2,"
  , "  int64_T arg3,"
  , "  int64_T arg4,"
  , "  cint16_T *arg5"
  , ")"
  , "{"
  , "  (resample)(arg1, arg2, arg3, arg4, arg5);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesspec_examples_Example_Safe_resample@
foreign import ccall safe "hs_bindgen_8a72aafc705daf44" hs_bindgen_8a72aafc705daf44_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> BG.Int64
  -> BG.Int64
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_edgecasesspec_examples_Example_Safe_resample@
hs_bindgen_8a72aafc705daf44 ::
     BG.Ptr Int32_T
  -> BG.Ptr (IsA.Elem (CA.ConstantArray 30720000 Cint16_T))
  -> Int64_T
  -> Int64_T
  -> BG.Ptr (IsA.Elem (CA.ConstantArray 30720000 Cint16_T))
  -> IO ()
hs_bindgen_8a72aafc705daf44 =
  BG.fromFFIType hs_bindgen_8a72aafc705daf44_base

{-| __C declaration:__ @resample@

    __defined at:__ @edge-cases\/spec_examples.h 31:6@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
resample ::
     BG.Ptr Int32_T
     -- ^ __C declaration:__ @res_m_num_valid_samples@
  -> BG.Ptr (IsA.Elem (CA.ConstantArray 30720000 Cint16_T))
     -- ^ __C declaration:__ @res_m_iq_int@
  -> Int64_T
     -- ^ __C declaration:__ @res_m_old_rate@
  -> Int64_T
     -- ^ __C declaration:__ @res_m_new_rate@
  -> BG.Ptr (IsA.Elem (CA.ConstantArray 30720000 Cint16_T))
     -- ^ __C declaration:__ @res_m_iq_resampled_int@
  -> IO ()
resample = hs_bindgen_8a72aafc705daf44
