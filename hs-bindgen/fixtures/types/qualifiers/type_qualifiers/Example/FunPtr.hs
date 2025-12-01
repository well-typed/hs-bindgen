{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/qualifiers/type_qualifiers.h>"
  , "/* test_typesqualifierstype_qualifie_Example_get_list_example_ptr */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_33c0388dc987452a (void)) ("
  , "  char const **arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &list_example;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_33c0388dc987452a" hs_bindgen_33c0388dc987452a_base ::
     HsBindgen.Runtime.HasBaseForeignType.BaseForeignType (IO (Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr FC.CChar)) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CBool)))

{-| __unique:__ @test_typesqualifierstype_qualifie_Example_get_list_example_ptr@
-}
hs_bindgen_33c0388dc987452a ::
     IO (Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr FC.CChar)) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CBool))
hs_bindgen_33c0388dc987452a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_33c0388dc987452a_base

{-# NOINLINE list_example_ptr #-}

{-| __C declaration:__ @list_example@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:14:6@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
list_example_ptr :: Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr FC.CChar)) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CBool)
list_example_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_33c0388dc987452a
