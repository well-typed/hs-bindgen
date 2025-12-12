{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2311fa9c0d0d6d06" resample_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> FC.CLLong
  -> FC.CLLong
  -> Ptr.Ptr Void
  -> IO ()

{-| __C declaration:__ @resample@

    __defined at:__ @edge-cases\/spec_examples.h:31:6@

    __exported by:__ @edge-cases\/spec_examples.h@

    __unique:__ @test_edgecasesspec_examples_Example_Unsafe_resample@
-}
resample ::
     Ptr.Ptr Int32_T
     -- ^ __C declaration:__ @res_m_num_valid_samples@
  -> Ptr.Ptr Cint16_T
     -- ^ __C declaration:__ @res_m_iq_int@
  -> Int64_T
     -- ^ __C declaration:__ @res_m_old_rate@
  -> Int64_T
     -- ^ __C declaration:__ @res_m_new_rate@
  -> Ptr.Ptr Cint16_T
     -- ^ __C declaration:__ @res_m_iq_resampled_int@
  -> IO ()
resample =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType resample_base
