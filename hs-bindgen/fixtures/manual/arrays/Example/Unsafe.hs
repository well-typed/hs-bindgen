{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <manual/arrays.h>"
  , "void hs_bindgen_f9f2776d121db261 ("
  , "  matrix const *arg1,"
  , "  matrix *arg2"
  , ")"
  , "{"
  , "  transpose(*arg1, *arg2);"
  , "}"
  , "void hs_bindgen_e43b4d44aa0abd14 ("
  , "  triplet_ptrs *arg1"
  , ")"
  , "{"
  , "  pretty_print_triplets(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_manualarrays_Example_Unsafe_transpose@
foreign import ccall unsafe "hs_bindgen_f9f2776d121db261" hs_bindgen_f9f2776d121db261_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_manualarrays_Example_Unsafe_transpose@
hs_bindgen_f9f2776d121db261 ::
     PtrConst.PtrConst Matrix
  -> RIP.Ptr Matrix
  -> IO ()
hs_bindgen_f9f2776d121db261 =
  RIP.fromFFIType hs_bindgen_f9f2776d121db261_base

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/arrays.h 36:6@

    __exported by:__ @manual\/arrays.h@
-}
transpose ::
     PtrConst.PtrConst Matrix
     -- ^ __C declaration:__ @input@
  -> RIP.Ptr Matrix
     -- ^ __C declaration:__ @output@
  -> IO ()
transpose = hs_bindgen_f9f2776d121db261

-- __unique:__ @test_manualarrays_Example_Unsafe_pretty_print_triplets@
foreign import ccall unsafe "hs_bindgen_e43b4d44aa0abd14" hs_bindgen_e43b4d44aa0abd14_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_manualarrays_Example_Unsafe_pretty_print_triplets@
hs_bindgen_e43b4d44aa0abd14 ::
     RIP.Ptr Triplet_ptrs
  -> IO ()
hs_bindgen_e43b4d44aa0abd14 =
  RIP.fromFFIType hs_bindgen_e43b4d44aa0abd14_base

{-| A function that prints the given triplet_ptrs

__C declaration:__ @pretty_print_triplets@

__defined at:__ @manual\/arrays.h 50:13@

__exported by:__ @manual\/arrays.h@
-}
pretty_print_triplets ::
     RIP.Ptr Triplet_ptrs
     -- ^ __C declaration:__ @x@
  -> IO ()
pretty_print_triplets = hs_bindgen_e43b4d44aa0abd14
