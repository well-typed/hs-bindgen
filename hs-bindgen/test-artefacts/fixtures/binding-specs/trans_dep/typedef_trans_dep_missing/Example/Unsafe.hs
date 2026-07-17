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
  [ "#include <binding-specs/trans_dep/typedef_trans_dep_missing.h>"
  , "void hs_bindgen_38d7300ed18eb652 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecstrans_deptypede_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_38d7300ed18eb652" hs_bindgen_38d7300ed18eb652_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecstrans_deptypede_Example_Unsafe_foo@
hs_bindgen_38d7300ed18eb652 ::
     BG.Ptr M.B
  -> IO ()
hs_bindgen_38d7300ed18eb652 =
  BG.fromFFIType hs_bindgen_38d7300ed18eb652_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/trans_dep\/typedef_trans_dep_missing.h 9:6@

    __exported by:__ @binding-specs\/trans_dep\/typedef_trans_dep_missing.h@
-}
foo ::
     BG.Ptr M.B
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_38d7300ed18eb652
