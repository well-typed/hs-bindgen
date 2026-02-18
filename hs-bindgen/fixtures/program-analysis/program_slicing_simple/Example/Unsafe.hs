{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <program-analysis/program_slicing_simple.h>"
  , "signed int hs_bindgen_fe855d53295ba8ab ("
  , "  uint64_t arg1,"
  , "  uint32_t arg2"
  , ")"
  , "{"
  , "  return bar(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_programanalysisprogram_slici_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_fe855d53295ba8ab" hs_bindgen_fe855d53295ba8ab_base ::
     RIP.Word64
  -> RIP.Word32
  -> IO RIP.Int32

-- __unique:__ @test_programanalysisprogram_slici_Example_Unsafe_bar@
hs_bindgen_fe855d53295ba8ab ::
     Foreign.Word64
  -> Uint32_t
  -> IO RIP.CInt
hs_bindgen_fe855d53295ba8ab =
  RIP.fromFFIType hs_bindgen_fe855d53295ba8ab_base

{-| __C declaration:__ @bar@

    __defined at:__ @program-analysis\/program_slicing_simple.h 8:5@

    __exported by:__ @program-analysis\/program_slicing_simple.h@
-}
bar ::
     Foreign.Word64
     -- ^ __C declaration:__ @x@
  -> Uint32_t
     -- ^ __C declaration:__ @y@
  -> IO RIP.CInt
bar = hs_bindgen_fe855d53295ba8ab
