{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.asm_labeled_function
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesasm_Example_get_asm_labeled_function@
hs_bindgen_4f7c4fceed28f171 :: IO (BG.FunPtr (BG.CInt -> BG.CInt -> IO BG.CInt))
hs_bindgen_4f7c4fceed28f171 =
  BG.fromFFIType hs_bindgen_4f7c4fceed28f171_base

{-# NOINLINE asm_labeled_function #-}
{-| __C declaration:__ @asm_labeled_function@

    __defined at:__ @attributes\/asm.h 4:5@

    __exported by:__ @attributes\/asm.h@
-}
asm_labeled_function :: BG.FunPtr (BG.CInt -> BG.CInt -> IO BG.CInt)
asm_labeled_function =
  BG.unsafePerformIO hs_bindgen_4f7c4fceed28f171
