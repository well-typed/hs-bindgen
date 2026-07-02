{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.bar
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <declarations/field_name_reuse_omit.h>"
  , "void hs_bindgen_1a3fb33e9aadacd9 (void)"
  , "{"
  , "  (bar)();"
  , "}"
  ]))

-- __unique:__ @test_declarationsfield_name_reuse__Example_Safe_bar@
foreign import ccall safe "hs_bindgen_1a3fb33e9aadacd9" hs_bindgen_1a3fb33e9aadacd9_base ::
     IO ()

-- __unique:__ @test_declarationsfield_name_reuse__Example_Safe_bar@
hs_bindgen_1a3fb33e9aadacd9 :: IO ()
hs_bindgen_1a3fb33e9aadacd9 =
  RIP.fromFFIType hs_bindgen_1a3fb33e9aadacd9_base

{-| __C declaration:__ @bar@

    __defined at:__ @declarations\/field_name_reuse_omit.h 28:6@

    __exported by:__ @declarations\/field_name_reuse_omit.h@
-}
bar :: IO ()
bar = hs_bindgen_1a3fb33e9aadacd9
