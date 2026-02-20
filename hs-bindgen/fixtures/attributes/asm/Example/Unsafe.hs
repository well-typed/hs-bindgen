{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <attributes/asm.h>"
  , "signed int hs_bindgen_3ad6c287a2386382 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return (asm_labeled_function)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_attributesasm_Example_Unsafe_asm_labeled_function@
foreign import ccall unsafe "hs_bindgen_3ad6c287a2386382" hs_bindgen_3ad6c287a2386382_base ::
     RIP.Int32
  -> RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_attributesasm_Example_Unsafe_asm_labeled_function@
hs_bindgen_3ad6c287a2386382 ::
     RIP.CInt
  -> RIP.CInt
  -> IO RIP.CInt
hs_bindgen_3ad6c287a2386382 =
  RIP.fromFFIType hs_bindgen_3ad6c287a2386382_base

{-| __C declaration:__ @asm_labeled_function@

    __defined at:__ @attributes\/asm.h 4:5@

    __exported by:__ @attributes\/asm.h@
-}
asm_labeled_function ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> RIP.CInt
     -- ^ __C declaration:__ @y@
  -> IO RIP.CInt
asm_labeled_function = hs_bindgen_3ad6c287a2386382
