{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <manual/arrays.h>"
  , "void hs_bindgen_cba7011c6d25362b ("
  , "  triplet const *arg1,"
  , "  triplet *arg2"
  , ")"
  , "{"
  , "  (transpose)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_45d15697a99c626a ("
  , "  signed int (**arg1)[3]"
  , ")"
  , "{"
  , "  (pretty_print_triplets)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_manualarrays_Example_Safe_transpose@
foreign import ccall safe "hs_bindgen_cba7011c6d25362b" hs_bindgen_cba7011c6d25362b_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_manualarrays_Example_Safe_transpose@
hs_bindgen_cba7011c6d25362b ::
     PtrConst.PtrConst (IsA.Elem Matrix)
  -> RIP.Ptr (IsA.Elem Matrix)
  -> IO ()
hs_bindgen_cba7011c6d25362b =
  RIP.fromFFIType hs_bindgen_cba7011c6d25362b_base

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/arrays.h 36:6@

    __exported by:__ @manual\/arrays.h@
-}
transpose ::
     PtrConst.PtrConst (IsA.Elem Matrix)
     -- ^ __C declaration:__ @input@
  -> RIP.Ptr (IsA.Elem Matrix)
     -- ^ __C declaration:__ @output@
  -> IO ()
transpose = hs_bindgen_cba7011c6d25362b

-- __unique:__ @test_manualarrays_Example_Safe_pretty_print_triplets@
foreign import ccall safe "hs_bindgen_45d15697a99c626a" hs_bindgen_45d15697a99c626a_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_manualarrays_Example_Safe_pretty_print_triplets@
hs_bindgen_45d15697a99c626a ::
     RIP.Ptr (IsA.Elem Triplet_ptrs)
  -> IO ()
hs_bindgen_45d15697a99c626a =
  RIP.fromFFIType hs_bindgen_45d15697a99c626a_base

{-| A function that prints the given triplet_ptrs

__C declaration:__ @pretty_print_triplets@

__defined at:__ @manual\/arrays.h 50:13@

__exported by:__ @manual\/arrays.h@
-}
pretty_print_triplets ::
     RIP.Ptr (IsA.Elem Triplet_ptrs)
     -- ^ __C declaration:__ @x@
  -> IO ()
pretty_print_triplets = hs_bindgen_45d15697a99c626a
