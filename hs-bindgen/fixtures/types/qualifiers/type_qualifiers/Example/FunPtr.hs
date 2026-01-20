{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_list_example@
hs_bindgen_a19bc138e7f2759b :: IO (Ptr.FunPtr ((Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar)) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CBool))
hs_bindgen_a19bc138e7f2759b =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a19bc138e7f2759b_base

{-# NOINLINE list_example #-}
{-| __C declaration:__ @list_example@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 14:6@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
list_example :: Ptr.FunPtr ((Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar)) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CBool)
list_example =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a19bc138e7f2759b
