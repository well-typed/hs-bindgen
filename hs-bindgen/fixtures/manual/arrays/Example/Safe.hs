{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <manual/arrays.h>"
  , "void hs_bindgen_cba7011c6d25362b ("
  , "  triplet *arg1,"
  , "  triplet *arg2"
  , ")"
  , "{"
  , "  transpose(arg1, arg2);"
  , "}"
  , "void hs_bindgen_45d15697a99c626a ("
  , "  signed int (**arg1)[3]"
  , ")"
  , "{"
  , "  pretty_print_triplets(arg1);"
  , "}"
  ]))

{-| Pointer-based API for 'transpose'

__unique:__ @test_manualarrays_Example_Safe_transpose@
-}
foreign import ccall safe "hs_bindgen_cba7011c6d25362b" transpose_wrapper ::
     Ptr.Ptr Triplet
  -> Ptr.Ptr Triplet
  -> IO ()

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/arrays.h:36:6@

    __exported by:__ @manual\/arrays.h@
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

{-| A function that prints the given triplet_ptrs

__C declaration:__ @pretty_print_triplets@

__defined at:__ @manual\/arrays.h:50:13@

__exported by:__ @manual\/arrays.h@

__unique:__ @test_manualarrays_Example_Safe_pretty_print_triplets@
-}
foreign import ccall safe "hs_bindgen_45d15697a99c626a" pretty_print_triplets ::
     Ptr.Ptr (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()
