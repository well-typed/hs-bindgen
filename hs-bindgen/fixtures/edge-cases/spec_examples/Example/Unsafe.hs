{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Array.Class
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Data.Void (Void)
import Example
import Prelude (IO)

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
  , "  resample(arg1, arg2, arg3, arg4, arg5);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesspec_examples_Example_Unsafe_resample@
foreign import ccall unsafe "hs_bindgen_2311fa9c0d0d6d06" hs_bindgen_2311fa9c0d0d6d06_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> GHC.Int.Int64
  -> GHC.Int.Int64
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_edgecasesspec_examples_Example_Unsafe_resample@
hs_bindgen_2311fa9c0d0d6d06 ::
     Ptr.Ptr Int32_T
  -> Ptr.Ptr (HsBindgen.Runtime.Array.Class.Elem ((HsBindgen.Runtime.ConstantArray.ConstantArray 30720000) Cint16_T))
  -> Int64_T
  -> Int64_T
  -> Ptr.Ptr (HsBindgen.Runtime.Array.Class.Elem ((HsBindgen.Runtime.ConstantArray.ConstantArray 30720000) Cint16_T))
  -> IO ()
hs_bindgen_2311fa9c0d0d6d06 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_2311fa9c0d0d6d06_base

{-| __C declaration:__ @resample@

    __defined at:__ @edge-cases\/spec_examples.h 31:6@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
resample ::
     Ptr.Ptr Int32_T
     -- ^ __C declaration:__ @res_m_num_valid_samples@
  -> Ptr.Ptr (HsBindgen.Runtime.Array.Class.Elem ((HsBindgen.Runtime.ConstantArray.ConstantArray 30720000) Cint16_T))
     -- ^ __C declaration:__ @res_m_iq_int@
  -> Int64_T
     -- ^ __C declaration:__ @res_m_old_rate@
  -> Int64_T
     -- ^ __C declaration:__ @res_m_new_rate@
  -> Ptr.Ptr (HsBindgen.Runtime.Array.Class.Elem ((HsBindgen.Runtime.ConstantArray.ConstantArray 30720000) Cint16_T))
     -- ^ __C declaration:__ @res_m_iq_resampled_int@
  -> IO ()
resample = hs_bindgen_2311fa9c0d0d6d06
