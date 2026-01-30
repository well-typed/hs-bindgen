{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <attributes/asm.h>"
  , "signed int hs_bindgen_369133049bfc1e73 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return asm_labeled_function(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_attributesasm_Example_Safe_asm_labeled_function@
foreign import ccall safe "hs_bindgen_369133049bfc1e73" hs_bindgen_369133049bfc1e73_base ::
     GHC.Int.Int32
  -> GHC.Int.Int32
  -> IO GHC.Int.Int32

-- __unique:__ @test_attributesasm_Example_Safe_asm_labeled_function@
hs_bindgen_369133049bfc1e73 ::
     FC.CInt
  -> FC.CInt
  -> IO FC.CInt
hs_bindgen_369133049bfc1e73 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_369133049bfc1e73_base

{-| __C declaration:__ @asm_labeled_function@

    __defined at:__ @attributes\/asm.h 4:5@

    __exported by:__ @attributes\/asm.h@
-}
asm_labeled_function ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> FC.CInt
     -- ^ __C declaration:__ @y@
  -> IO FC.CInt
asm_labeled_function = hs_bindgen_369133049bfc1e73
