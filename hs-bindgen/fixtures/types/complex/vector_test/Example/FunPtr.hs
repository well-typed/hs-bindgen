{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/complex/vector_test.h>"
  , "/* test_typescomplexvector_test_Example_get_new_vector_ptr */"
  , "__attribute__ ((const))"
  , "vector *(*hs_bindgen_a9bd57bd55b4e697 (void)) ("
  , "  double arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &new_vector;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a9bd57bd55b4e697" hs_bindgen_a9bd57bd55b4e697_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_typescomplexvector_test_Example_get_new_vector_ptr@
hs_bindgen_a9bd57bd55b4e697 ::
     IO (Ptr.FunPtr (FC.CDouble -> FC.CDouble -> IO (Ptr.Ptr Vector)))
hs_bindgen_a9bd57bd55b4e697 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a9bd57bd55b4e697_base

{-# NOINLINE new_vector_ptr #-}

{-| __C declaration:__ @new_vector@

    __defined at:__ @types\/complex\/vector_test.h:6:9@

    __exported by:__ @types\/complex\/vector_test.h@
-}
new_vector_ptr :: Ptr.FunPtr (FC.CDouble -> FC.CDouble -> IO (Ptr.Ptr Vector))
new_vector_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a9bd57bd55b4e697
