{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <manual/zero_copy.h>"
  , "signed int hs_bindgen_test_manualzero_copy_ca203e332b4afe73 ("
  , "  struct vector const *arg1,"
  , "  struct vector *arg2"
  , ")"
  , "{"
  , "  return reverse(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_manualzero_copy_d7aa7016f1b951b2 ("
  , "  triplet *arg1,"
  , "  triplet *arg2"
  , ")"
  , "{"
  , "  transpose(arg1, arg2);"
  , "}"
  ]))

{-| __C declaration:__ @reverse@

    __defined at:__ @manual\/zero_copy.h:77:5@

    __exported by:__ @manual\/zero_copy.h@

    __unique:__ @ExampleJust Unsafereverse@
-}
foreign import ccall unsafe "hs_bindgen_test_manualzero_copy_ca203e332b4afe73" reverse ::
     Ptr.Ptr Vector
     {- ^ __C declaration:__ @input@
     -}
  -> Ptr.Ptr Vector
     {- ^ __C declaration:__ @output@
     -}
  -> IO FC.CInt

{-| Pointer-based API for 'transpose'

__unique:__ @ExampleJust Unsafetranspose@
-}
foreign import ccall unsafe "hs_bindgen_test_manualzero_copy_d7aa7016f1b951b2" transpose_wrapper ::
     Ptr.Ptr Triplet
  -> Ptr.Ptr Triplet
  -> IO ()

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/zero_copy.h:85:6@

    __exported by:__ @manual\/zero_copy.h@
-}
transpose ::
     Matrix
     {- ^ __C declaration:__ @input@
     -}
  -> Ptr.Ptr Triplet
     {- ^ __C declaration:__ @output@
     -}
  -> IO ()
transpose =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr2 ->
                                                    transpose_wrapper ptr2 x1)
