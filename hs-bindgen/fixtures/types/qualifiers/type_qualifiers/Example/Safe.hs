{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.PtrConst as PtrConst

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/qualifiers/type_qualifiers.h>"
  , "_Bool hs_bindgen_32187cc02676ee72 ("
  , "  char const **arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return (list_example)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_typesqualifierstype_qualifie_Example_Safe_list_example@
foreign import ccall safe "hs_bindgen_32187cc02676ee72" hs_bindgen_32187cc02676ee72_base ::
     RIP.Ptr RIP.Void
  -> RIP.Word64
  -> IO RIP.Word8

-- __unique:__ @test_typesqualifierstype_qualifie_Example_Safe_list_example@
hs_bindgen_32187cc02676ee72 ::
     RIP.Ptr (PtrConst.PtrConst RIP.CChar)
  -> HsBindgen.Runtime.LibC.CSize
  -> IO RIP.CBool
hs_bindgen_32187cc02676ee72 =
  RIP.fromFFIType hs_bindgen_32187cc02676ee72_base

{-| __C declaration:__ @list_example@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 14:6@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
list_example ::
     RIP.Ptr (PtrConst.PtrConst RIP.CChar)
     -- ^ __C declaration:__ @items@
  -> HsBindgen.Runtime.LibC.CSize
     -- ^ __C declaration:__ @count@
  -> IO RIP.CBool
list_example = hs_bindgen_32187cc02676ee72
