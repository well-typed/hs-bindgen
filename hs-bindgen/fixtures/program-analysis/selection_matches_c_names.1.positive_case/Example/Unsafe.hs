{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <program-analysis/selection_matches_c_names.h>"
  , "signed int hs_bindgen_9a13a53e6a2f2416 (void)"
  , "{"
  , "  return FunctionWithAssignedHaskellNameByNameMangler();"
  , "}"
  ]))

-- __unique:__ @test_programanalysisselection_mat_Example_Unsafe_FunctionWithAssignedHaskellNameByNameMangler@
foreign import ccall unsafe "hs_bindgen_9a13a53e6a2f2416" hs_bindgen_9a13a53e6a2f2416_base ::
     IO RIP.Int32

-- __unique:__ @test_programanalysisselection_mat_Example_Unsafe_FunctionWithAssignedHaskellNameByNameMangler@
hs_bindgen_9a13a53e6a2f2416 :: IO RIP.CInt
hs_bindgen_9a13a53e6a2f2416 =
  RIP.fromFFIType hs_bindgen_9a13a53e6a2f2416_base

{-| __C declaration:__ @FunctionWithAssignedHaskellNameByNameMangler@

    __defined at:__ @program-analysis\/selection_matches_c_names.h 5:5@

    __exported by:__ @program-analysis\/selection_matches_c_names.h@
-}
functionWithAssignedHaskellNameByNameMangler :: IO RIP.CInt
functionWithAssignedHaskellNameByNameMangler =
  hs_bindgen_9a13a53e6a2f2416
