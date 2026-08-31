{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.use_widget
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/include_spelling/sub/consumer.h>"
  , "void hs_bindgen_4113e841e5cc0e72 ("
  , "  widget_legacy_t *arg1"
  , ")"
  , "{"
  , "  (use_widget)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsinclude_spelling_Example_Safe_use_widget@
foreign import ccall safe "hs_bindgen_4113e841e5cc0e72" hs_bindgen_4113e841e5cc0e72_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsinclude_spelling_Example_Safe_use_widget@
hs_bindgen_4113e841e5cc0e72 ::
     BG.Ptr Widget_legacy_t
  -> IO ()
hs_bindgen_4113e841e5cc0e72 =
  BG.fromFFIType hs_bindgen_4113e841e5cc0e72_base

{-| __C declaration:__ @use_widget@

    __defined at:__ @binding-specs\/include_spelling\/sub\/consumer.h 5:6@

    __exported by:__ @binding-specs\/include_spelling\/sub\/consumer.h@
-}
use_widget ::
     BG.Ptr Widget_legacy_t
     -- ^ __C declaration:__ @w@
  -> IO ()
use_widget = hs_bindgen_4113e841e5cc0e72
