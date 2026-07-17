{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.list_example
    )
  where

import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <types/qualifiers/type_qualifiers.h>"
  , "/* test_typesqualifierstype_qualifie_Example_get_list_example */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_a19bc138e7f2759b (void)) ("
  , "  char const **arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &list_example;"
  , "}"
  ]))

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_list_example@
foreign import ccall unsafe "hs_bindgen_a19bc138e7f2759b" hs_bindgen_a19bc138e7f2759b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_list_example@
hs_bindgen_a19bc138e7f2759b :: IO (BG.FunPtr (BG.Ptr (PtrConst.PtrConst BG.CChar) -> HsBindgen.Runtime.LibC.CSize -> IO BG.CBool))
hs_bindgen_a19bc138e7f2759b =
  BG.fromFFIType hs_bindgen_a19bc138e7f2759b_base

{-# NOINLINE list_example #-}
{-| __C declaration:__ @list_example@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 14:6@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
list_example :: BG.FunPtr (BG.Ptr (PtrConst.PtrConst BG.CChar) -> HsBindgen.Runtime.LibC.CSize -> IO BG.CBool)
list_example =
  BG.unsafePerformIO hs_bindgen_a19bc138e7f2759b
