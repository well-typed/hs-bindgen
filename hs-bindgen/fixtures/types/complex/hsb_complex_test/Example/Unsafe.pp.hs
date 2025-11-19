{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Data.Complex
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/complex/hsb_complex_test.h>"
  , "void hs_bindgen_test_typescomplexhsb_complex_test_5b05fdb10924da35 ("
  , "  float _Complex *arg1,"
  , "  float _Complex *arg2,"
  , "  float _Complex *arg3"
  , ")"
  , "{"
  , "  *arg3 = multiply_complex_f(*arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_typescomplexhsb_complex_test_59f299d5d991ed72 ("
  , "  double _Complex *arg1,"
  , "  double _Complex *arg2,"
  , "  double _Complex *arg3"
  , ")"
  , "{"
  , "  *arg3 = add_complex(*arg1, *arg2);"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_5b05fdb10924da35" multiply_complex_f_wrapper_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr (Data.Complex.Complex FC.CFloat)
    -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
    -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
    -> IO ()
    )

{-| Pointer-based API for 'multiply_complex_f'
-}
multiply_complex_f_wrapper ::
     Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> IO ()
multiply_complex_f_wrapper =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType multiply_complex_f_wrapper_base

{-| __C declaration:__ @multiply_complex_f@

    __defined at:__ @types\/complex\/hsb_complex_test.h:21:16@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
multiply_complex_f ::
     Data.Complex.Complex FC.CFloat
     {- ^ __C declaration:__ @a@
     -}
  -> Data.Complex.Complex FC.CFloat
     {- ^ __C declaration:__ @b@
     -}
  -> IO (Data.Complex.Complex FC.CFloat)
multiply_complex_f =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   F.with x0 (\y3 ->
                                HsBindgen.Runtime.CAPI.allocaAndPeek (\z4 ->
                                                                        multiply_complex_f_wrapper y3 y2 z4)))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_59f299d5d991ed72" add_complex_wrapper_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr (Data.Complex.Complex FC.CDouble)
    -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
    -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
    -> IO ()
    )

{-| Pointer-based API for 'add_complex'
-}
add_complex_wrapper ::
     Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> IO ()
add_complex_wrapper =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType add_complex_wrapper_base

{-| __C declaration:__ @add_complex@

    __defined at:__ @types\/complex\/hsb_complex_test.h:22:16@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
add_complex ::
     Data.Complex.Complex FC.CDouble
     {- ^ __C declaration:__ @a@
     -}
  -> Data.Complex.Complex FC.CDouble
     {- ^ __C declaration:__ @b@
     -}
  -> IO (Data.Complex.Complex FC.CDouble)
add_complex =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   F.with x0 (\y3 ->
                                HsBindgen.Runtime.CAPI.allocaAndPeek (\z4 ->
                                                                        add_complex_wrapper y3 y2 z4)))
