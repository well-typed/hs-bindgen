{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/varargs.h>"
  , "void hs_bindgen_32ebae80cc3543e1 (void)"
  , "{"
  , "  (h)();"
  , "}"
  ]))

-- __unique:__ @test_functionsvarargs_Example_Unsafe_h@
foreign import ccall unsafe "hs_bindgen_32ebae80cc3543e1" hs_bindgen_32ebae80cc3543e1_base ::
     IO ()

-- __unique:__ @test_functionsvarargs_Example_Unsafe_h@
hs_bindgen_32ebae80cc3543e1 :: IO ()
hs_bindgen_32ebae80cc3543e1 =
  RIP.fromFFIType hs_bindgen_32ebae80cc3543e1_base

{-| __C declaration:__ @h@

    __defined at:__ @functions\/varargs.h 8:6@

    __exported by:__ @functions\/varargs.h@
-}
h :: IO ()
h = hs_bindgen_32ebae80cc3543e1
