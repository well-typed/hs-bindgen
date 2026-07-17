{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.asm_labeled_function
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <attributes/asm.h>"
  , "signed int hs_bindgen_369133049bfc1e73 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return (asm_labeled_function)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_attributesasm_Example_Safe_asm_labeled_function@
foreign import ccall safe "hs_bindgen_369133049bfc1e73" hs_bindgen_369133049bfc1e73_base ::
     BG.Int32
  -> BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_attributesasm_Example_Safe_asm_labeled_function@
hs_bindgen_369133049bfc1e73 ::
     BG.CInt
  -> BG.CInt
  -> IO BG.CInt
hs_bindgen_369133049bfc1e73 =
  BG.fromFFIType hs_bindgen_369133049bfc1e73_base

{-| __C declaration:__ @asm_labeled_function@

    __defined at:__ @attributes\/asm.h 4:5@

    __exported by:__ @attributes\/asm.h@
-}
asm_labeled_function ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> BG.CInt
     -- ^ __C declaration:__ @y@
  -> IO BG.CInt
asm_labeled_function = hs_bindgen_369133049bfc1e73
