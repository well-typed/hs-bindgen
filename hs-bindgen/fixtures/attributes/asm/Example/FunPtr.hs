{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <attributes/asm.h>"
  , "/* test_attributesasm_Example_get_asm_labeled_function */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_4f7c4fceed28f171 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &asm_labeled_function;"
  , "}"
  ]))

-- __unique:__ @test_attributesasm_Example_get_asm_labeled_function@
foreign import ccall unsafe "hs_bindgen_4f7c4fceed28f171" hs_bindgen_4f7c4fceed28f171_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_attributesasm_Example_get_asm_labeled_function@
hs_bindgen_4f7c4fceed28f171 :: IO (RIP.FunPtr (RIP.CInt -> RIP.CInt -> IO RIP.CInt))
hs_bindgen_4f7c4fceed28f171 =
  RIP.fromFFIType hs_bindgen_4f7c4fceed28f171_base

{-# NOINLINE asm_labeled_function #-}
{-| __C declaration:__ @asm_labeled_function@

    __defined at:__ @attributes\/asm.h 4:5@

    __exported by:__ @attributes\/asm.h@
-}
asm_labeled_function :: RIP.FunPtr (RIP.CInt -> RIP.CInt -> IO RIP.CInt)
asm_labeled_function =
  RIP.unsafePerformIO hs_bindgen_4f7c4fceed28f171
