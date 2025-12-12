{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <manual/arrays.h>"
  , "/* test_manualarrays_Example_get_transpose_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_24c867a3e91cab5d (void)) ("
  , "  matrix const arg1,"
  , "  matrix arg2"
  , ")"
  , "{"
  , "  return &transpose;"
  , "}"
  , "/* test_manualarrays_Example_get_pretty_print_triplets_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_66af82cae0c5134a (void)) ("
  , "  triplet_ptrs arg1"
  , ")"
  , "{"
  , "  return &pretty_print_triplets;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_24c867a3e91cab5d" hs_bindgen_24c867a3e91cab5d_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_manualarrays_Example_get_transpose_ptr@
hs_bindgen_24c867a3e91cab5d ::
     IO (Ptr.FunPtr (Matrix -> Matrix -> IO ()))
hs_bindgen_24c867a3e91cab5d =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_24c867a3e91cab5d_base

{-# NOINLINE transpose_ptr #-}

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/arrays.h:36:6@

    __exported by:__ @manual\/arrays.h@
-}
transpose_ptr :: Ptr.FunPtr (Matrix -> Matrix -> IO ())
transpose_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_24c867a3e91cab5d

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_66af82cae0c5134a" hs_bindgen_66af82cae0c5134a_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_manualarrays_Example_get_pretty_print_triplets_ptr@
hs_bindgen_66af82cae0c5134a ::
     IO (Ptr.FunPtr (Triplet_ptrs -> IO ()))
hs_bindgen_66af82cae0c5134a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_66af82cae0c5134a_base

{-# NOINLINE pretty_print_triplets_ptr #-}

{-| A function that prints the given triplet_ptrs

__C declaration:__ @pretty_print_triplets@

__defined at:__ @manual\/arrays.h:50:13@

__exported by:__ @manual\/arrays.h@
-}
pretty_print_triplets_ptr :: Ptr.FunPtr (Triplet_ptrs -> IO ())
pretty_print_triplets_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_66af82cae0c5134a
