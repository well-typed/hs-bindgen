{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.f
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <declarations/declarations_required_for_scoping.h>"
  , "void hs_bindgen_0d1c75136a36e326 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  (f)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_declarationsdeclarations_requ_Example_Safe_f@
foreign import ccall safe "hs_bindgen_0d1c75136a36e326" hs_bindgen_0d1c75136a36e326_base ::
     BG.Word64
  -> IO ()

-- __unique:__ @test_declarationsdeclarations_requ_Example_Safe_f@
hs_bindgen_0d1c75136a36e326 ::
     A
  -> IO ()
hs_bindgen_0d1c75136a36e326 =
  BG.fromFFIType hs_bindgen_0d1c75136a36e326_base

{-| __C declaration:__ @f@

    __defined at:__ @declarations\/declarations_required_for_scoping.h 7:6@

    __exported by:__ @declarations\/declarations_required_for_scoping.h@
-}
f ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
f = hs_bindgen_0d1c75136a36e326
