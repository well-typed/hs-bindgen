{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <program-analysis/selection_matches_c_names.h>"
  , "/* test_programanalysisselection_mat_Example_get_FunctionWithAssignedHaskellNameByNameMangler */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_1ccc97a484f823b2 (void)) (void)"
  , "{"
  , "  return &FunctionWithAssignedHaskellNameByNameMangler;"
  , "}"
  ]))

-- __unique:__ @test_programanalysisselection_mat_Example_get_FunctionWithAssignedHaskellNameByNameMangler@
foreign import ccall unsafe "hs_bindgen_1ccc97a484f823b2" hs_bindgen_1ccc97a484f823b2_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_programanalysisselection_mat_Example_get_FunctionWithAssignedHaskellNameByNameMangler@
hs_bindgen_1ccc97a484f823b2 :: IO (RIP.FunPtr (IO RIP.CInt))
hs_bindgen_1ccc97a484f823b2 =
  RIP.fromFFIType hs_bindgen_1ccc97a484f823b2_base

{-# NOINLINE functionWithAssignedHaskellNameByNameMangler #-}
{-| __C declaration:__ @FunctionWithAssignedHaskellNameByNameMangler@

    __defined at:__ @program-analysis\/selection_matches_c_names.h 5:5@

    __exported by:__ @program-analysis\/selection_matches_c_names.h@
-}
functionWithAssignedHaskellNameByNameMangler :: RIP.FunPtr (IO RIP.CInt)
functionWithAssignedHaskellNameByNameMangler =
  RIP.unsafePerformIO hs_bindgen_1ccc97a484f823b2
