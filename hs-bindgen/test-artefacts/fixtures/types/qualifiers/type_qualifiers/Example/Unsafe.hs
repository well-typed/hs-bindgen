{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.list_example
    )
  where

import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <types/qualifiers/type_qualifiers.h>"
  , "_Bool hs_bindgen_360934a08f19eaab ("
  , "  char const **arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return (list_example)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_typesqualifierstype_qualifie_Example_Unsafe_list_example@
foreign import ccall unsafe "hs_bindgen_360934a08f19eaab" hs_bindgen_360934a08f19eaab_base ::
     BG.Ptr BG.Void
  -> BG.Word64
  -> IO BG.Word8

-- __unique:__ @test_typesqualifierstype_qualifie_Example_Unsafe_list_example@
hs_bindgen_360934a08f19eaab ::
     BG.Ptr (PtrConst.PtrConst BG.CChar)
  -> HsBindgen.Runtime.LibC.CSize
  -> IO BG.CBool
hs_bindgen_360934a08f19eaab =
  BG.fromFFIType hs_bindgen_360934a08f19eaab_base

{-| __C declaration:__ @list_example@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 14:6@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
list_example ::
     BG.Ptr (PtrConst.PtrConst BG.CChar)
     -- ^ __C declaration:__ @items@
  -> HsBindgen.Runtime.LibC.CSize
     -- ^ __C declaration:__ @count@
  -> IO BG.CBool
list_example = hs_bindgen_360934a08f19eaab
