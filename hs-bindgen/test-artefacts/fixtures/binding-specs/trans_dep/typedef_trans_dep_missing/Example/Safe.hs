{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/trans_dep/typedef_trans_dep_missing.h>"
  , "void hs_bindgen_6687afc6aaabd927 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecstrans_deptypede_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_6687afc6aaabd927" hs_bindgen_6687afc6aaabd927_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecstrans_deptypede_Example_Safe_foo@
hs_bindgen_6687afc6aaabd927 ::
     BG.Ptr M.B
  -> IO ()
hs_bindgen_6687afc6aaabd927 =
  BG.fromFFIType hs_bindgen_6687afc6aaabd927_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/trans_dep\/typedef_trans_dep_missing.h 9:6@

    __exported by:__ @binding-specs\/trans_dep\/typedef_trans_dep_missing.h@
-}
foo ::
     BG.Ptr M.B
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_6687afc6aaabd927
