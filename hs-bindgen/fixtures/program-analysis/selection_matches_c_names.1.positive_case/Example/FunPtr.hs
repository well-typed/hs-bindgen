{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Internal.CAPI
import Data.Void (Void)
import Prelude (IO)

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
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_programanalysisselection_mat_Example_get_FunctionWithAssignedHaskellNameByNameMangler@
hs_bindgen_1ccc97a484f823b2 :: IO (Ptr.FunPtr (IO FC.CInt))
hs_bindgen_1ccc97a484f823b2 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_1ccc97a484f823b2_base

{-# NOINLINE functionWithAssignedHaskellNameByNameMangler #-}
{-| __C declaration:__ @FunctionWithAssignedHaskellNameByNameMangler@

    __defined at:__ @program-analysis\/selection_matches_c_names.h 5:5@

    __exported by:__ @program-analysis\/selection_matches_c_names.h@
-}
functionWithAssignedHaskellNameByNameMangler :: Ptr.FunPtr (IO FC.CInt)
functionWithAssignedHaskellNameByNameMangler =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1ccc97a484f823b2
