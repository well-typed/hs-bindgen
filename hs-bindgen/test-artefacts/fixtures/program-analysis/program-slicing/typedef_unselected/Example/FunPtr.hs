{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.bar
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <program-analysis/program-slicing/typedef_unselected.h>"
  , "/* test_programanalysisprogramslici_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e57577b970e09cca (void)) ("
  , "  U arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  ]))

-- __unique:__ @test_programanalysisprogramslici_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_e57577b970e09cca" hs_bindgen_e57577b970e09cca_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_programanalysisprogramslici_Example_get_bar@
hs_bindgen_e57577b970e09cca :: IO (BG.FunPtr (U -> IO ()))
hs_bindgen_e57577b970e09cca =
  BG.fromFFIType hs_bindgen_e57577b970e09cca_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @program-analysis\/program-slicing\/typedef_unselected.h 12:6@

    __exported by:__ @program-analysis\/program-slicing\/typedef_unselected.h@
-}
bar :: BG.FunPtr (U -> IO ())
bar = BG.unsafePerformIO hs_bindgen_e57577b970e09cca
