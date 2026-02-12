{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.LibC
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (Ptr.Ptr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_jmp_buf_val@
hs_bindgen_68c2b692efbc4734 :: IO (Ptr.Ptr HsBindgen.Runtime.LibC.CJmpBuf)
hs_bindgen_68c2b692efbc4734 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_68c2b692efbc4734_base

{-# NOINLINE jmp_buf_val #-}
{-| __C declaration:__ @jmp_buf_val@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 35:16@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
jmp_buf_val :: Ptr.Ptr HsBindgen.Runtime.LibC.CJmpBuf
jmp_buf_val =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_68c2b692efbc4734
