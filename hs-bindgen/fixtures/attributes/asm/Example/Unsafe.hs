{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <attributes/asm.h>"
  , "signed int hs_bindgen_3ad6c287a2386382 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return asm_labeled_function(arg1, arg2);"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3ad6c287a2386382" asm_labeled_function_base ::
     FC.CInt
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @asm_labeled_function@

    __defined at:__ @attributes\/asm.h:4:5@

    __exported by:__ @attributes\/asm.h@

    __unique:__ @test_attributesasm_Example_Unsafe_asm_labeled_function@
-}
asm_labeled_function ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> FC.CInt
     -- ^ __C declaration:__ @y@
  -> IO FC.CInt
asm_labeled_function =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType asm_labeled_function_base
