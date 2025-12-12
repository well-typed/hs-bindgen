{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
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

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_350cceac1101d344" reverse_base ::
     HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> Ptr.Ptr Void
  -> IO FC.CInt

{-| __C declaration:__ @reverse@

    __defined at:__ @manual\/zero_copy.h:77:5@

    __exported by:__ @manual\/zero_copy.h@

    __unique:__ @test_manualzero_copy_Example_Safe_reverse@
-}
reverse ::
     HsBindgen.Runtime.ConstPtr.ConstPtr Vector
     -- ^ __C declaration:__ @input@
  -> Ptr.Ptr Vector
     -- ^ __C declaration:__ @output@
  -> IO FC.CInt
reverse =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType reverse_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_2ff371c815d92b04" transpose_wrapper_base ::
     HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> Ptr.Ptr Void
  -> IO ()

{-| Pointer-based API for 'transpose'

__unique:__ @test_manualzero_copy_Example_Safe_transpose@
-}
transpose_wrapper ::
     HsBindgen.Runtime.ConstPtr.ConstPtr Triplet
  -> Ptr.Ptr Triplet
  -> IO ()
transpose_wrapper =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType transpose_wrapper_base

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/zero_copy.h:85:6@

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
                                                    transpose_wrapper (HsBindgen.Runtime.ConstPtr.ConstPtr ptr2) x1)
