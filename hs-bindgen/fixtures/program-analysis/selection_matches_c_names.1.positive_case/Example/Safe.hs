{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <program-analysis/selection_matches_c_names.h>"
  , "signed int hs_bindgen_c9b1dc5577fd8ced (void)"
  , "{"
  , "  return (FunctionWithAssignedHaskellNameByNameMangler)();"
  , "}"
  ]))

-- __unique:__ @test_programanalysisselection_mat_Example_Safe_FunctionWithAssignedHaskellNameByNameMangler@
foreign import ccall safe "hs_bindgen_c9b1dc5577fd8ced" hs_bindgen_c9b1dc5577fd8ced_base ::
     IO RIP.Int32

-- __unique:__ @test_programanalysisselection_mat_Example_Safe_FunctionWithAssignedHaskellNameByNameMangler@
hs_bindgen_c9b1dc5577fd8ced :: IO RIP.CInt
hs_bindgen_c9b1dc5577fd8ced =
  RIP.fromFFIType hs_bindgen_c9b1dc5577fd8ced_base

{-| __C declaration:__ @FunctionWithAssignedHaskellNameByNameMangler@

    __defined at:__ @program-analysis\/selection_matches_c_names.h 5:5@

    __exported by:__ @program-analysis\/selection_matches_c_names.h@
-}
functionWithAssignedHaskellNameByNameMangler :: IO RIP.CInt
functionWithAssignedHaskellNameByNameMangler =
  hs_bindgen_c9b1dc5577fd8ced
