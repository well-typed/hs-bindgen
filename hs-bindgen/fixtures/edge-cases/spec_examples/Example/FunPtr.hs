{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/spec_examples.h>"
  , "/* ExampleNothingget_resample_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesspec_examples_46b04422dcd0bbd5 (void)) ("
  , "  int32_T *arg1,"
  , "  cint16_T arg2[30720000],"
  , "  int64_T arg3,"
  , "  int64_T arg4,"
  , "  cint16_T arg5[30720000]"
  , ")"
  , "{"
  , "  return &resample;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_resample_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesspec_examples_46b04422dcd0bbd5" hs_bindgen_test_edgecasesspec_examples_46b04422dcd0bbd5 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Int32_T) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 30720000) Cint16_T) -> Int64_T -> Int64_T -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 30720000) Cint16_T) -> IO ()))

{-# NOINLINE resample_ptr #-}

{-| __C declaration:__ @resample@

    __defined at:__ @edge-cases\/spec_examples.h:31:6@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
resample_ptr :: Ptr.FunPtr ((Ptr.Ptr Int32_T) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 30720000) Cint16_T) -> Int64_T -> Int64_T -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 30720000) Cint16_T) -> IO ())
resample_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesspec_examples_46b04422dcd0bbd5
