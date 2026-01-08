{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <manual/zero_copy.h>"
  , "signed int hs_bindgen_350cceac1101d344 ("
  , "  struct vector const *arg1,"
  , "  struct vector *arg2"
  , ")"
  , "{"
  , "  return reverse(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2ff371c815d92b04 ("
  , "  triplet const *arg1,"
  , "  triplet *arg2"
  , ")"
  , "{"
  , "  transpose(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_manualzero_copy_Example_Safe_reverse@
foreign import ccall safe "hs_bindgen_350cceac1101d344" hs_bindgen_350cceac1101d344 ::
     HsBindgen.Runtime.ConstPtr.ConstPtr Vector
  -> Ptr.Ptr Vector
  -> IO FC.CInt

{-| __C declaration:__ @reverse@

    __defined at:__ @manual\/zero_copy.h 77:5@

    __exported by:__ @manual\/zero_copy.h@
-}
reverse ::
     HsBindgen.Runtime.ConstPtr.ConstPtr Vector
     -- ^ __C declaration:__ @input@
  -> Ptr.Ptr Vector
     -- ^ __C declaration:__ @output@
  -> IO FC.CInt
reverse = hs_bindgen_350cceac1101d344

-- __unique:__ @test_manualzero_copy_Example_Safe_transpose@
foreign import ccall safe "hs_bindgen_2ff371c815d92b04" hs_bindgen_2ff371c815d92b04 ::
     HsBindgen.Runtime.ConstPtr.ConstPtr Triplet
  -> Ptr.Ptr Triplet
  -> IO ()

{-| Pointer-based API for 'transpose'
-}
transpose_wrapper ::
     HsBindgen.Runtime.ConstPtr.ConstPtr Triplet
     -- ^ __C declaration:__ @input@
  -> Ptr.Ptr Triplet
     -- ^ __C declaration:__ @output@
  -> IO ()
transpose_wrapper = hs_bindgen_2ff371c815d92b04

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/zero_copy.h 85:6@

    __exported by:__ @manual\/zero_copy.h@
-}
transpose ::
     Matrix
     -- ^ __C declaration:__ @input@
  -> Ptr.Ptr Triplet
     -- ^ __C declaration:__ @output@
  -> IO ()
transpose =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr2 ->
                                                    hs_bindgen_2ff371c815d92b04 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr2) x1)
