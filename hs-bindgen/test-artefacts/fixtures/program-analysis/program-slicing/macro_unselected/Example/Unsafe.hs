{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.bar
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <program-analysis/program-slicing/macro_unselected.h>"
  , "void hs_bindgen_401f15168ffec8ae ("
  , "  U arg1"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_programanalysisprogramslici_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_401f15168ffec8ae" hs_bindgen_401f15168ffec8ae_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_programanalysisprogramslici_Example_Unsafe_bar@
hs_bindgen_401f15168ffec8ae ::
     U
  -> IO ()
hs_bindgen_401f15168ffec8ae =
  BG.fromFFIType hs_bindgen_401f15168ffec8ae_base

{-| __C declaration:__ @bar@

    __defined at:__ @program-analysis\/program-slicing\/macro_unselected.h 12:6@

    __exported by:__ @program-analysis\/program-slicing\/macro_unselected.h@
-}
bar ::
     U
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_401f15168ffec8ae
