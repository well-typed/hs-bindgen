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
  , "signed int hs_bindgen_f9655173d51bbaac ("
  , "  struct vector const *arg1,"
  , "  struct vector *arg2"
  , ")"
  , "{"
  , "  return reverse(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ea25667627dd5ed2 ("
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

    __unique:__ @test_manualzero_copy_Example_Unsafe_reverse@
-}
foreign import ccall unsafe "hs_bindgen_f9655173d51bbaac" reverse ::
     Ptr.Ptr Vector
     {- ^ __C declaration:__ @input@
     -}
  -> Ptr.Ptr Vector
     {- ^ __C declaration:__ @output@
     -}
  -> IO FC.CInt

{-| Pointer-based API for 'transpose'

__unique:__ @test_manualzero_copy_Example_Unsafe_transpose@
-}
foreign import ccall unsafe "hs_bindgen_ea25667627dd5ed2" transpose_wrapper ::
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
