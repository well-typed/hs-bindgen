{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/qualifiers/type_qualifiers.h>"
  , "/* ExampleNothingget_list_example_ptr */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_test_typesqualifierstype_qualifie_24b25f22222ce366 (void)) ("
  , "  char const **arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &list_example;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_list_example_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typesqualifierstype_qualifie_24b25f22222ce366" hs_bindgen_test_typesqualifierstype_qualifie_24b25f22222ce366 ::
     IO (Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr FC.CChar)) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CBool))

{-# NOINLINE list_example_ptr #-}

{-| __C declaration:__ @list_example@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:14:6@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
list_example_ptr :: Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr FC.CChar)) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CBool)
list_example_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typesqualifierstype_qualifie_24b25f22222ce366
