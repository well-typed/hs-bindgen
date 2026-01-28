{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.HasFFIType
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <program-analysis/program_slicing_simple.h>"
  , "/* test_programanalysisprogram_slici_Example_get_bar */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_993162e0dadfa2c9 (void)) ("
  , "  uint64_t arg1,"
  , "  uint32_t arg2"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  ]))

-- __unique:__ @test_programanalysisprogram_slici_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_993162e0dadfa2c9" hs_bindgen_993162e0dadfa2c9_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_programanalysisprogram_slici_Example_get_bar@
hs_bindgen_993162e0dadfa2c9 :: IO (Ptr.FunPtr (Foreign.Word64 -> Uint32_t -> IO FC.CInt))
hs_bindgen_993162e0dadfa2c9 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_993162e0dadfa2c9_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @program-analysis\/program_slicing_simple.h 8:5@

    __exported by:__ @program-analysis\/program_slicing_simple.h@
-}
bar :: Ptr.FunPtr (Foreign.Word64 -> Uint32_t -> IO FC.CInt)
bar =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_993162e0dadfa2c9
