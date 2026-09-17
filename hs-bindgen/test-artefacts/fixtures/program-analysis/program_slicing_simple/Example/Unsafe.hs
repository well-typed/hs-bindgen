{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.bar
    )
  where

import qualified Foreign.C.Types
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <program-analysis/program_slicing_simple.h>"
  , "signed int hs_bindgen_fe855d53295ba8ab ("
  , "  uint64_t arg1,"
  , "  uint32_t arg2"
  , ")"
  , "{"
  , "  return (bar)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_programanalysisprogram_slici_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_fe855d53295ba8ab" hs_bindgen_fe855d53295ba8ab_base ::
     BG.CULong
  -> BG.CUInt
  -> IO BG.CInt

-- __unique:__ @test_programanalysisprogram_slici_Example_Unsafe_bar@
hs_bindgen_fe855d53295ba8ab ::
     Foreign.C.Types.CULong
  -> Uint32_t
  -> IO BG.CInt
hs_bindgen_fe855d53295ba8ab =
  \x0 ->
    \x1 ->
      fmap BG.fromFFIType (hs_bindgen_fe855d53295ba8ab_base (BG.toFFIType x0) (BG.toFFIType x1))

{-| __C declaration:__ @bar@

    __defined at:__ @program-analysis\/program_slicing_simple.h 8:5@

    __exported by:__ @program-analysis\/program_slicing_simple.h@
-}
bar ::
     Foreign.C.Types.CULong
     -- ^ __C declaration:__ @x@
  -> Uint32_t
     -- ^ __C declaration:__ @y@
  -> IO BG.CInt
bar = hs_bindgen_fe855d53295ba8ab
