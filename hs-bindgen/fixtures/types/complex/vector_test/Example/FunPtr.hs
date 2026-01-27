{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/complex/vector_test.h>"
  , "/* test_typescomplexvector_test_Example_get_new_vector */"
  , "__attribute__ ((const))"
  , "vector *(*hs_bindgen_cb36cf0957839e33 (void)) ("
  , "  double arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &new_vector;"
  , "}"
  ]))

-- __unique:__ @test_typescomplexvector_test_Example_get_new_vector@
foreign import ccall unsafe "hs_bindgen_cb36cf0957839e33" hs_bindgen_cb36cf0957839e33_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typescomplexvector_test_Example_get_new_vector@
hs_bindgen_cb36cf0957839e33 :: IO (Ptr.FunPtr (FC.CDouble -> FC.CDouble -> IO (Ptr.Ptr Vector)))
hs_bindgen_cb36cf0957839e33 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_cb36cf0957839e33_base

{-# NOINLINE new_vector #-}
{-| __C declaration:__ @new_vector@

    __defined at:__ @types\/complex\/vector_test.h 6:9@

    __exported by:__ @types\/complex\/vector_test.h@
-}
new_vector :: Ptr.FunPtr (FC.CDouble -> FC.CDouble -> IO (Ptr.Ptr Vector))
new_vector =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cb36cf0957839e33
