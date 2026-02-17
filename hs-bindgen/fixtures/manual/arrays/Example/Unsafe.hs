{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Array.Class
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.PtrConst
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <manual/arrays.h>"
  , "void hs_bindgen_f9f2776d121db261 ("
  , "  triplet const *arg1,"
  , "  triplet *arg2"
  , ")"
  , "{"
  , "  transpose(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e43b4d44aa0abd14 ("
  , "  signed int (**arg1)[3]"
  , ")"
  , "{"
  , "  pretty_print_triplets(arg1);"
  , "}"
  ]))

-- __unique:__ @test_manualarrays_Example_Unsafe_transpose@
foreign import ccall unsafe "hs_bindgen_f9f2776d121db261" hs_bindgen_f9f2776d121db261_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_manualarrays_Example_Unsafe_transpose@
hs_bindgen_f9f2776d121db261 ::
     HsBindgen.Runtime.PtrConst.PtrConst (HsBindgen.Runtime.Array.Class.Elem Matrix)
  -> Ptr.Ptr (HsBindgen.Runtime.Array.Class.Elem Matrix)
  -> IO ()
hs_bindgen_f9f2776d121db261 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f9f2776d121db261_base

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/arrays.h 36:6@

    __exported by:__ @manual\/arrays.h@
-}
transpose ::
     HsBindgen.Runtime.PtrConst.PtrConst (HsBindgen.Runtime.Array.Class.Elem Matrix)
     -- ^ __C declaration:__ @input@
  -> Ptr.Ptr (HsBindgen.Runtime.Array.Class.Elem Matrix)
     -- ^ __C declaration:__ @output@
  -> IO ()
transpose = hs_bindgen_f9f2776d121db261

-- __unique:__ @test_manualarrays_Example_Unsafe_pretty_print_triplets@
foreign import ccall unsafe "hs_bindgen_e43b4d44aa0abd14" hs_bindgen_e43b4d44aa0abd14_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_manualarrays_Example_Unsafe_pretty_print_triplets@
hs_bindgen_e43b4d44aa0abd14 ::
     Ptr.Ptr (HsBindgen.Runtime.Array.Class.Elem Triplet_ptrs)
  -> IO ()
hs_bindgen_e43b4d44aa0abd14 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_e43b4d44aa0abd14_base

{-| A function that prints the given triplet_ptrs

__C declaration:__ @pretty_print_triplets@

__defined at:__ @manual\/arrays.h 50:13@

__exported by:__ @manual\/arrays.h@
-}
pretty_print_triplets ::
     Ptr.Ptr (HsBindgen.Runtime.Array.Class.Elem Triplet_ptrs)
     -- ^ __C declaration:__ @x@
  -> IO ()
pretty_print_triplets = hs_bindgen_e43b4d44aa0abd14
