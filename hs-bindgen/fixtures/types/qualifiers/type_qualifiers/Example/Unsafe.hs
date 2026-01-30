{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Word
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.PtrConst
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/qualifiers/type_qualifiers.h>"
  , "_Bool hs_bindgen_360934a08f19eaab ("
  , "  char const **arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return list_example(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_typesqualifierstype_qualifie_Example_Unsafe_list_example@
foreign import ccall unsafe "hs_bindgen_360934a08f19eaab" hs_bindgen_360934a08f19eaab_base ::
     Ptr.Ptr Void
  -> GHC.Word.Word64
  -> IO GHC.Word.Word8

-- __unique:__ @test_typesqualifierstype_qualifie_Example_Unsafe_list_example@
hs_bindgen_360934a08f19eaab ::
     Ptr.Ptr (HsBindgen.Runtime.PtrConst.PtrConst FC.CChar)
  -> HsBindgen.Runtime.LibC.CSize
  -> IO FC.CBool
hs_bindgen_360934a08f19eaab =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_360934a08f19eaab_base

{-| __C declaration:__ @list_example@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 14:6@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
list_example ::
     Ptr.Ptr (HsBindgen.Runtime.PtrConst.PtrConst FC.CChar)
     -- ^ __C declaration:__ @items@
  -> HsBindgen.Runtime.LibC.CSize
     -- ^ __C declaration:__ @count@
  -> IO FC.CBool
list_example = hs_bindgen_360934a08f19eaab
