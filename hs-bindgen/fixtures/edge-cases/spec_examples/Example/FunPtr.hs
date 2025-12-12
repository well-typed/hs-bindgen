{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/spec_examples.h>"
  , "/* test_edgecasesspec_examples_Example_get_resample_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c940a9562d0838b1 (void)) ("
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

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c940a9562d0838b1" hs_bindgen_c940a9562d0838b1_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_edgecasesspec_examples_Example_get_resample_ptr@
hs_bindgen_c940a9562d0838b1 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Int32_T) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 30720000) Cint16_T) -> Int64_T -> Int64_T -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 30720000) Cint16_T) -> IO ()))
hs_bindgen_c940a9562d0838b1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_c940a9562d0838b1_base

{-# NOINLINE resample_ptr #-}

{-| __C declaration:__ @resample@

    __defined at:__ @edge-cases\/spec_examples.h:31:6@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
resample_ptr :: Ptr.FunPtr ((Ptr.Ptr Int32_T) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 30720000) Cint16_T) -> Int64_T -> Int64_T -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 30720000) Cint16_T) -> IO ())
resample_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c940a9562d0838b1
