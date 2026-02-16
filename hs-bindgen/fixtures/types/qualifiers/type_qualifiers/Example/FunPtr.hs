{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.PtrConst as PtrConst

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_list_example@
hs_bindgen_a19bc138e7f2759b :: IO (RIP.FunPtr ((RIP.Ptr (PtrConst.PtrConst RIP.CChar)) -> HsBindgen.Runtime.LibC.CSize -> IO RIP.CBool))
hs_bindgen_a19bc138e7f2759b =
  RIP.fromFFIType hs_bindgen_a19bc138e7f2759b_base

{-# NOINLINE list_example #-}
{-| __C declaration:__ @list_example@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 14:6@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
list_example :: RIP.FunPtr ((RIP.Ptr (PtrConst.PtrConst RIP.CChar)) -> HsBindgen.Runtime.LibC.CSize -> IO RIP.CBool)
list_example =
  RIP.unsafePerformIO hs_bindgen_a19bc138e7f2759b
