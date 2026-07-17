{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.jmp_buf_val
    )
  where

import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/stdlib/return_values.h>"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_jmp_buf_val */"
  , "__attribute__ ((const))"
  , "jmp_buf *hs_bindgen_68c2b692efbc4734 (void)"
  , "{"
  , "  return &jmp_buf_val;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_jmp_buf_val@
foreign import ccall unsafe "hs_bindgen_68c2b692efbc4734" hs_bindgen_68c2b692efbc4734_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_jmp_buf_val@
hs_bindgen_68c2b692efbc4734 :: IO (BG.Ptr HsBindgen.Runtime.LibC.CJmpBuf)
hs_bindgen_68c2b692efbc4734 =
  BG.fromFFIType hs_bindgen_68c2b692efbc4734_base

{-# NOINLINE jmp_buf_val #-}
{-| __C declaration:__ @jmp_buf_val@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 46:16@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
jmp_buf_val :: BG.Ptr HsBindgen.Runtime.LibC.CJmpBuf
jmp_buf_val =
  BG.unsafePerformIO hs_bindgen_68c2b692efbc4734
