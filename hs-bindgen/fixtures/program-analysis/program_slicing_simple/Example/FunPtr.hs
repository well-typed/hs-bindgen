{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_programanalysisprogram_slici_Example_get_bar@
hs_bindgen_993162e0dadfa2c9 :: IO (RIP.FunPtr (Foreign.Word64 -> Uint32_t -> IO RIP.CInt))
hs_bindgen_993162e0dadfa2c9 =
  RIP.fromFFIType hs_bindgen_993162e0dadfa2c9_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @program-analysis\/program_slicing_simple.h 8:5@

    __exported by:__ @program-analysis\/program_slicing_simple.h@
-}
bar :: RIP.FunPtr (Foreign.Word64 -> Uint32_t -> IO RIP.CInt)
bar = RIP.unsafePerformIO hs_bindgen_993162e0dadfa2c9
