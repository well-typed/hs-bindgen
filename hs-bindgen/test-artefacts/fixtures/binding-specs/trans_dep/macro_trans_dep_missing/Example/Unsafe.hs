{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/trans_dep/macro_trans_dep_missing.h>"
  , "void hs_bindgen_83453c8683cd3ff7 ("
  , "  B arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecstrans_depmacro__Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_83453c8683cd3ff7" hs_bindgen_83453c8683cd3ff7_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_bindingspecstrans_depmacro__Example_Unsafe_foo@
hs_bindgen_83453c8683cd3ff7 ::
     M.B
  -> IO ()
hs_bindgen_83453c8683cd3ff7 =
  BG.fromFFIType hs_bindgen_83453c8683cd3ff7_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/trans_dep\/macro_trans_dep_missing.h 7:6@

    __exported by:__ @binding-specs\/trans_dep\/macro_trans_dep_missing.h@
-}
foo ::
     M.B
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_83453c8683cd3ff7
