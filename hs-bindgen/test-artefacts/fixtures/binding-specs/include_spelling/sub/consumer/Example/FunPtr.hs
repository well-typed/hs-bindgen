{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.use_widget
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/include_spelling/sub/consumer.h>"
  , "/* test_bindingspecsinclude_spelling_Example_get_use_widget */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0a228b1efb019468 (void)) ("
  , "  widget_legacy_t *arg1"
  , ")"
  , "{"
  , "  return &use_widget;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsinclude_spelling_Example_get_use_widget@
foreign import ccall unsafe "hs_bindgen_0a228b1efb019468" hs_bindgen_0a228b1efb019468_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsinclude_spelling_Example_get_use_widget@
hs_bindgen_0a228b1efb019468 :: IO (BG.FunPtr (BG.Ptr Widget_legacy_t -> IO ()))
hs_bindgen_0a228b1efb019468 =
  BG.fromFFIType hs_bindgen_0a228b1efb019468_base

{-# NOINLINE use_widget #-}
{-| __C declaration:__ @use_widget@

    __defined at:__ @binding-specs\/include_spelling\/sub\/consumer.h 5:6@

    __exported by:__ @binding-specs\/include_spelling\/sub\/consumer.h@
-}
use_widget :: BG.FunPtr (BG.Ptr Widget_legacy_t -> IO ())
use_widget =
  BG.unsafePerformIO hs_bindgen_0a228b1efb019468
