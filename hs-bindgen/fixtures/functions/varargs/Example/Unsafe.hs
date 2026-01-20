{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/varargs.h>"
  , "void hs_bindgen_32ebae80cc3543e1 (void)"
  , "{"
  , "  h();"
  , "}"
  ]))

-- __unique:__ @test_functionsvarargs_Example_Unsafe_h@
foreign import ccall unsafe "hs_bindgen_32ebae80cc3543e1" hs_bindgen_32ebae80cc3543e1_base ::
     IO ()

-- __unique:__ @test_functionsvarargs_Example_Unsafe_h@
hs_bindgen_32ebae80cc3543e1 :: IO ()
hs_bindgen_32ebae80cc3543e1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_32ebae80cc3543e1_base

{-| __C declaration:__ @h@

    __defined at:__ @functions\/varargs.h 8:6@

    __exported by:__ @functions\/varargs.h@
-}
h :: IO ()
h = hs_bindgen_32ebae80cc3543e1
