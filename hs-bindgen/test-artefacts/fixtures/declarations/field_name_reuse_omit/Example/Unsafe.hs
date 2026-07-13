{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.bar
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <declarations/field_name_reuse_omit.h>"
  , "void hs_bindgen_b64ebb06c53fc1b3 (void)"
  , "{"
  , "  (bar)();"
  , "}"
  ]))

-- __unique:__ @test_declarationsfield_name_reuse__Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_b64ebb06c53fc1b3" hs_bindgen_b64ebb06c53fc1b3_base ::
     IO ()

-- __unique:__ @test_declarationsfield_name_reuse__Example_Unsafe_bar@
hs_bindgen_b64ebb06c53fc1b3 :: IO ()
hs_bindgen_b64ebb06c53fc1b3 =
  BG.fromFFIType hs_bindgen_b64ebb06c53fc1b3_base

{-| __C declaration:__ @bar@

    __defined at:__ @declarations\/field_name_reuse_omit.h 28:6@

    __exported by:__ @declarations\/field_name_reuse_omit.h@
-}
bar :: IO ()
bar = hs_bindgen_b64ebb06c53fc1b3
