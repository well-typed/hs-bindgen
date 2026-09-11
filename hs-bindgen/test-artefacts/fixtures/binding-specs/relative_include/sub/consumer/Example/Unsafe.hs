{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.use_widget
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/relative_include/sub/consumer.h>"
  , "void hs_bindgen_206a5cae524a3fcb ("
  , "  widget_legacy_t *arg1"
  , ")"
  , "{"
  , "  (use_widget)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsrelative_include_Example_Unsafe_use_widget@
foreign import ccall unsafe "hs_bindgen_206a5cae524a3fcb" hs_bindgen_206a5cae524a3fcb_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsrelative_include_Example_Unsafe_use_widget@
hs_bindgen_206a5cae524a3fcb ::
     BG.Ptr Widget_legacy_t
  -> IO ()
hs_bindgen_206a5cae524a3fcb =
  BG.fromFFIType hs_bindgen_206a5cae524a3fcb_base

{-| __C declaration:__ @use_widget@

    __defined at:__ @binding-specs\/relative_include\/sub\/consumer.h 5:6@

    __exported by:__ @binding-specs\/relative_include\/sub\/consumer.h@
-}
use_widget ::
     BG.Ptr Widget_legacy_t
     -- ^ __C declaration:__ @w@
  -> IO ()
use_widget = hs_bindgen_206a5cae524a3fcb
