{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <manual/arrays.h>"
  , "/* Example_get_transpose_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_manualarrays_c8ec17b4322959b0 (void)) ("
  , "  matrix const arg1,"
  , "  matrix arg2"
  , ")"
  , "{"
  , "  return &transpose;"
  , "}"
  , "/* Example_get_pretty_print_triplets_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_manualarrays_2e4a6fee995639ce (void)) ("
  , "  triplet_ptrs arg1"
  , ")"
  , "{"
  , "  return &pretty_print_triplets;"
  , "}"
  ]))

{-| __unique:__ @Example_get_transpose_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualarrays_c8ec17b4322959b0" hs_bindgen_test_manualarrays_c8ec17b4322959b0 ::
     IO (Ptr.FunPtr (Matrix -> Matrix -> IO ()))

{-# NOINLINE transpose_ptr #-}

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/arrays.h:36:6@

    __exported by:__ @manual\/arrays.h@
-}
transpose_ptr :: Ptr.FunPtr (Matrix -> Matrix -> IO ())
transpose_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualarrays_c8ec17b4322959b0

{-| __unique:__ @Example_get_pretty_print_triplets_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualarrays_2e4a6fee995639ce" hs_bindgen_test_manualarrays_2e4a6fee995639ce ::
     IO (Ptr.FunPtr (Triplet_ptrs -> IO ()))

{-# NOINLINE pretty_print_triplets_ptr #-}

{-| A function that prints the given triplet_ptrs

__C declaration:__ @pretty_print_triplets@

__defined at:__ @manual\/arrays.h:50:13@

__exported by:__ @manual\/arrays.h@
-}
pretty_print_triplets_ptr :: Ptr.FunPtr (Triplet_ptrs -> IO ())
pretty_print_triplets_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualarrays_2e4a6fee995639ce
