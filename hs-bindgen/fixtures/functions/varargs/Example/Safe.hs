{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/varargs.h>"
  , "void hs_bindgen_77a4bac5bbe80f62 (void)"
  , "{"
  , "  h();"
  , "}"
  ]))

-- __unique:__ @test_functionsvarargs_Example_Safe_h@
foreign import ccall safe "hs_bindgen_77a4bac5bbe80f62" hs_bindgen_77a4bac5bbe80f62_base ::
     IO ()

-- __unique:__ @test_functionsvarargs_Example_Safe_h@
hs_bindgen_77a4bac5bbe80f62 :: IO ()
hs_bindgen_77a4bac5bbe80f62 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_77a4bac5bbe80f62_base

{-| __C declaration:__ @h@

    __defined at:__ @functions\/varargs.h 8:6@

    __exported by:__ @functions\/varargs.h@
-}
h :: IO ()
h = hs_bindgen_77a4bac5bbe80f62
