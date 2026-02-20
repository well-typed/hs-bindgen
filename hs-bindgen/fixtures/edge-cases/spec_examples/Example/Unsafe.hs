{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <edge-cases/spec_examples.h>"
  , "void hs_bindgen_2311fa9c0d0d6d06 ("
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

-- __unique:__ @test_edgecasesspec_examples_Example_Unsafe_resample@
foreign import ccall unsafe "hs_bindgen_2311fa9c0d0d6d06" hs_bindgen_2311fa9c0d0d6d06_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int64
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_edgecasesspec_examples_Example_Unsafe_resample@
hs_bindgen_2311fa9c0d0d6d06 ::
     RIP.Ptr Int32_T
  -> RIP.Ptr (IsA.Elem ((CA.ConstantArray 30720000) Cint16_T))
  -> Int64_T
  -> Int64_T
  -> RIP.Ptr (IsA.Elem ((CA.ConstantArray 30720000) Cint16_T))
  -> IO ()
hs_bindgen_2311fa9c0d0d6d06 =
  RIP.fromFFIType hs_bindgen_2311fa9c0d0d6d06_base

{-| __C declaration:__ @resample@

    __defined at:__ @edge-cases\/spec_examples.h 31:6@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
resample ::
     RIP.Ptr Int32_T
     -- ^ __C declaration:__ @res_m_num_valid_samples@
  -> RIP.Ptr (IsA.Elem ((CA.ConstantArray 30720000) Cint16_T))
     -- ^ __C declaration:__ @res_m_iq_int@
  -> Int64_T
     -- ^ __C declaration:__ @res_m_old_rate@
  -> Int64_T
     -- ^ __C declaration:__ @res_m_new_rate@
  -> RIP.Ptr (IsA.Elem ((CA.ConstantArray 30720000) Cint16_T))
     -- ^ __C declaration:__ @res_m_iq_resampled_int@
  -> IO ()
resample = hs_bindgen_2311fa9c0d0d6d06
