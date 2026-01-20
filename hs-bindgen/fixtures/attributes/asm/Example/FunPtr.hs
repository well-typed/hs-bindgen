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
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <attributes/asm.h>"
  , "/* test_attributesasm_Example_get_asm_labeled_function */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_4f7c4fceed28f171 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &asm_labeled_function;"
  , "}"
  ]))

-- __unique:__ @test_attributesasm_Example_get_asm_labeled_function@
foreign import ccall unsafe "hs_bindgen_4f7c4fceed28f171" hs_bindgen_4f7c4fceed28f171_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_attributesasm_Example_get_asm_labeled_function@
hs_bindgen_4f7c4fceed28f171 :: IO (Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt))
hs_bindgen_4f7c4fceed28f171 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4f7c4fceed28f171_base

{-# NOINLINE asm_labeled_function #-}
{-| __C declaration:__ @asm_labeled_function@

    __defined at:__ @attributes\/asm.h 4:5@

    __exported by:__ @attributes\/asm.h@
-}
asm_labeled_function :: Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)
asm_labeled_function =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4f7c4fceed28f171
