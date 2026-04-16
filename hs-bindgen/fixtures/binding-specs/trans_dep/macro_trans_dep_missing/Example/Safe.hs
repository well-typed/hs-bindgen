{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified M

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/trans_dep/macro_trans_dep_missing.h>"
  , "void hs_bindgen_361ca1eaf8f4495c ("
  , "  B arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecstrans_depmacro__Example_Safe_foo@
foreign import ccall safe "hs_bindgen_361ca1eaf8f4495c" hs_bindgen_361ca1eaf8f4495c_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_bindingspecstrans_depmacro__Example_Safe_foo@
hs_bindgen_361ca1eaf8f4495c ::
     M.B
  -> IO ()
hs_bindgen_361ca1eaf8f4495c =
  RIP.fromFFIType hs_bindgen_361ca1eaf8f4495c_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/trans_dep\/macro_trans_dep_missing.h 7:6@

    __exported by:__ @binding-specs\/trans_dep\/macro_trans_dep_missing.h@
-}
foo ::
     M.B
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_361ca1eaf8f4495c
