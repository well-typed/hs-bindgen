{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.functionWithAssignedHaskellNameByNameMangler
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <program-analysis/selection_matches_c_names.h>"
  , "signed int hs_bindgen_9a13a53e6a2f2416 (void)"
  , "{"
  , "  return (FunctionWithAssignedHaskellNameByNameMangler)();"
  , "}"
  ]))

-- __unique:__ @test_programanalysisselection_mat_Example_Unsafe_FunctionWithAssignedHaskellNameByNameMangler@
foreign import ccall unsafe "hs_bindgen_9a13a53e6a2f2416" hs_bindgen_9a13a53e6a2f2416_base ::
     IO BG.Int32

-- __unique:__ @test_programanalysisselection_mat_Example_Unsafe_FunctionWithAssignedHaskellNameByNameMangler@
hs_bindgen_9a13a53e6a2f2416 :: IO BG.CInt
hs_bindgen_9a13a53e6a2f2416 =
  BG.fromFFIType hs_bindgen_9a13a53e6a2f2416_base

{-| __C declaration:__ @FunctionWithAssignedHaskellNameByNameMangler@

    __defined at:__ @program-analysis\/selection_matches_c_names.h 5:5@

    __exported by:__ @program-analysis\/selection_matches_c_names.h@
-}
functionWithAssignedHaskellNameByNameMangler :: IO BG.CInt
functionWithAssignedHaskellNameByNameMangler =
  hs_bindgen_9a13a53e6a2f2416
