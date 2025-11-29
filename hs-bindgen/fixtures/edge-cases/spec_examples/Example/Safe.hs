{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/spec_examples.h>"
  , "void hs_bindgen_test_edgecasesspec_examples_7d4128962cfce15d ("
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

{-| __C declaration:__ @resample@

    __defined at:__ @edge-cases\/spec_examples.h:31:6@

    __exported by:__ @edge-cases\/spec_examples.h@

    __unique:__ @ExampleJust Saferesample@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesspec_examples_7d4128962cfce15d" resample ::
     Ptr.Ptr Int32_T
     {- ^ __C declaration:__ @res_m_num_valid_samples@
     -}
  -> Ptr.Ptr Cint16_T
     {- ^ __C declaration:__ @res_m_iq_int@
     -}
  -> Int64_T
     {- ^ __C declaration:__ @res_m_old_rate@
     -}
  -> Int64_T
     {- ^ __C declaration:__ @res_m_new_rate@
     -}
  -> Ptr.Ptr Cint16_T
     {- ^ __C declaration:__ @res_m_iq_resampled_int@
     -}
  -> IO ()
