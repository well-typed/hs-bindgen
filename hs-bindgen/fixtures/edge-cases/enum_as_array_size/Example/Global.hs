{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/enum_as_array_size.h>"
  , "/* test_edgecasesenum_as_array_size_Example_get_test_array */"
  , "__attribute__ ((const))"
  , "char const (*hs_bindgen_30b94bcf7e387817 (void))[1]"
  , "{"
  , "  return &test_array;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesenum_as_array_size_Example_get_test_array@
foreign import ccall unsafe "hs_bindgen_30b94bcf7e387817" hs_bindgen_30b94bcf7e387817_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_edgecasesenum_as_array_size_Example_get_test_array@
hs_bindgen_30b94bcf7e387817 :: IO (HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CChar))
hs_bindgen_30b94bcf7e387817 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_30b94bcf7e387817_base

{-# NOINLINE hs_bindgen_e30c033f156164cc #-}
{-| __C declaration:__ @test_array@

    __defined at:__ @edge-cases\/enum_as_array_size.h 8:19@

    __exported by:__ @edge-cases\/enum_as_array_size.h@

    __unique:__ @test_edgecasesenum_as_array_size_Example_test_array@
-}
hs_bindgen_e30c033f156164cc :: HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CChar)
hs_bindgen_e30c033f156164cc =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_30b94bcf7e387817

{-# NOINLINE test_array #-}
test_array :: (HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CChar
test_array =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_e30c033f156164cc))
