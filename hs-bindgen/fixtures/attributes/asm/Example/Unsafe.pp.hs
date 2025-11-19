{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <attributes/asm.h>"
  , "signed int hs_bindgen_test_attributesasm_289cb83358beadc0 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return asm_labeled_function(arg1, arg2);"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_attributesasm_289cb83358beadc0" asm_labeled_function_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CInt
    -> FC.CInt
    -> IO FC.CInt
    )

{-| __C declaration:__ @asm_labeled_function@

    __defined at:__ @attributes\/asm.h:4:5@

    __exported by:__ @attributes\/asm.h@
-}
asm_labeled_function ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @y@
     -}
  -> IO FC.CInt
asm_labeled_function =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType asm_labeled_function_base
