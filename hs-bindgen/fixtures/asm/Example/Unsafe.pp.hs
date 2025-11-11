{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <asm.h>"
  , "signed int hs_bindgen_test_asm_289cb83358beadc0 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return asm_labeled_function(arg1, arg2);"
  , "}"
  ]))

{-| __C declaration:__ @asm_labeled_function@

    __defined at:__ @asm.h:4:5@

    __exported by:__ @asm.h@
-}
foreign import ccall unsafe "hs_bindgen_test_asm_289cb83358beadc0" asm_labeled_function ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @y@
     -}
  -> IO FC.CInt
