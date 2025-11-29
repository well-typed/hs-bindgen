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
  , "/* get_transpose_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_manualarrays_fc1dad225b555299 (void)) ("
  , "  matrix const arg1,"
  , "  matrix arg2"
  , ")"
  , "{"
  , "  return &transpose;"
  , "}"
  , "/* get_pretty_print_triplets_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_manualarrays_0b485cea747ee35d (void)) ("
  , "  triplet_ptrs arg1"
  , ")"
  , "{"
  , "  return &pretty_print_triplets;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_transpose_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualarrays_fc1dad225b555299" hs_bindgen_test_manualarrays_fc1dad225b555299 ::
     IO (Ptr.FunPtr (Matrix -> Matrix -> IO ()))

{-# NOINLINE transpose_ptr #-}

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/arrays.h:36:6@

    __exported by:__ @manual\/arrays.h@
-}
transpose_ptr :: Ptr.FunPtr (Matrix -> Matrix -> IO ())
transpose_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualarrays_fc1dad225b555299

{-| __unique:__ @ExampleNothingget_pretty_print_triplets_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualarrays_0b485cea747ee35d" hs_bindgen_test_manualarrays_0b485cea747ee35d ::
     IO (Ptr.FunPtr (Triplet_ptrs -> IO ()))

{-# NOINLINE pretty_print_triplets_ptr #-}

{-| A function that prints the given triplet_ptrs

__C declaration:__ @pretty_print_triplets@

__defined at:__ @manual\/arrays.h:50:13@

__exported by:__ @manual\/arrays.h@
-}
pretty_print_triplets_ptr :: Ptr.FunPtr (Triplet_ptrs -> IO ())
pretty_print_triplets_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualarrays_0b485cea747ee35d
