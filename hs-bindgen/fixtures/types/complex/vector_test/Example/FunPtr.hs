{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/complex/vector_test.h>"
  , "/* ExampleNothingget_new_vector_ptr */"
  , "__attribute__ ((const))"
  , "vector *(*hs_bindgen_test_typescomplexvector_test_7672b9e7f001c998 (void)) ("
  , "  double arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &new_vector;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_new_vector_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexvector_test_7672b9e7f001c998" hs_bindgen_test_typescomplexvector_test_7672b9e7f001c998 ::
     IO (Ptr.FunPtr (FC.CDouble -> FC.CDouble -> IO (Ptr.Ptr Vector)))

{-# NOINLINE new_vector_ptr #-}

{-| __C declaration:__ @new_vector@

    __defined at:__ @types\/complex\/vector_test.h:6:9@

    __exported by:__ @types\/complex\/vector_test.h@
-}
new_vector_ptr :: Ptr.FunPtr (FC.CDouble -> FC.CDouble -> IO (Ptr.Ptr Vector))
new_vector_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexvector_test_7672b9e7f001c998
