{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.HasFFIType
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <program-analysis/selection_matches_c_names.h>"
  , "signed int hs_bindgen_c9b1dc5577fd8ced (void)"
  , "{"
  , "  return FunctionWithAssignedHaskellNameByNameMangler();"
  , "}"
  ]))

-- __unique:__ @test_programanalysisselection_mat_Example_Safe_FunctionWithAssignedHaskellNameByNameMangler@
foreign import ccall safe "hs_bindgen_c9b1dc5577fd8ced" hs_bindgen_c9b1dc5577fd8ced_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_programanalysisselection_mat_Example_Safe_FunctionWithAssignedHaskellNameByNameMangler@
hs_bindgen_c9b1dc5577fd8ced :: IO FC.CInt
hs_bindgen_c9b1dc5577fd8ced =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_c9b1dc5577fd8ced_base

{-| __C declaration:__ @FunctionWithAssignedHaskellNameByNameMangler@

    __defined at:__ @program-analysis\/selection_matches_c_names.h 5:5@

    __exported by:__ @program-analysis\/selection_matches_c_names.h@
-}
functionWithAssignedHaskellNameByNameMangler :: IO FC.CInt
functionWithAssignedHaskellNameByNameMangler =
  hs_bindgen_c9b1dc5577fd8ced
