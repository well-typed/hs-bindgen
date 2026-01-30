{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.PtrConst
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <manual/arrays.h>"
  , "void hs_bindgen_cba7011c6d25362b ("
  , "  matrix const *arg1,"
  , "  matrix *arg2"
  , ")"
  , "{"
  , "  transpose(*arg1, *arg2);"
  , "}"
  , "void hs_bindgen_45d15697a99c626a ("
  , "  triplet_ptrs *arg1"
  , ")"
  , "{"
  , "  pretty_print_triplets(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_manualarrays_Example_Safe_transpose@
foreign import ccall safe "hs_bindgen_cba7011c6d25362b" hs_bindgen_cba7011c6d25362b_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_manualarrays_Example_Safe_transpose@
hs_bindgen_cba7011c6d25362b ::
     HsBindgen.Runtime.PtrConst.PtrConst Matrix
  -> Ptr.Ptr Matrix
  -> IO ()
hs_bindgen_cba7011c6d25362b =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_cba7011c6d25362b_base

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/arrays.h 36:6@

    __exported by:__ @manual\/arrays.h@
-}
transpose ::
     HsBindgen.Runtime.PtrConst.PtrConst Matrix
     -- ^ __C declaration:__ @input@
  -> Ptr.Ptr Matrix
     -- ^ __C declaration:__ @output@
  -> IO ()
transpose = hs_bindgen_cba7011c6d25362b

-- __unique:__ @test_manualarrays_Example_Safe_pretty_print_triplets@
foreign import ccall safe "hs_bindgen_45d15697a99c626a" hs_bindgen_45d15697a99c626a_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_manualarrays_Example_Safe_pretty_print_triplets@
hs_bindgen_45d15697a99c626a ::
     Ptr.Ptr Triplet_ptrs
  -> IO ()
hs_bindgen_45d15697a99c626a =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_45d15697a99c626a_base

{-| A function that prints the given triplet_ptrs

__C declaration:__ @pretty_print_triplets@

__defined at:__ @manual\/arrays.h 50:13@

__exported by:__ @manual\/arrays.h@
-}
pretty_print_triplets ::
     Ptr.Ptr Triplet_ptrs
     -- ^ __C declaration:__ @x@
  -> IO ()
pretty_print_triplets = hs_bindgen_45d15697a99c626a
