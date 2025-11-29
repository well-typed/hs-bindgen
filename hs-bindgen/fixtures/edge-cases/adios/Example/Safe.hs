{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/adios.h>"
  , "void hs_bindgen_test_edgecasesadios_d563f36e835846ef (void)"
  , "{"
  , "  \978();"
  , "}"
  , "void hs_bindgen_test_edgecasesadios_6ecd3c2c1d149f9a (void)"
  , "{"
  , "  \25308\25308();"
  , "}"
  , "void hs_bindgen_test_edgecasesadios_580d4502b4a9e30b (void)"
  , "{"
  , "  Say\25308\25308();"
  , "}"
  ]))

{-| __C declaration:__ @ϒ@

    __defined at:__ @edge-cases\/adios.h:18:6@

    __exported by:__ @edge-cases\/adios.h@

    __unique:__ @Example_Safe_ϒ@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesadios_d563f36e835846ef" cϒ ::
     IO ()

{-| __C declaration:__ @拜拜@

    __defined at:__ @edge-cases\/adios.h:27:6@

    __exported by:__ @edge-cases\/adios.h@

    __unique:__ @Example_Safe_拜拜@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesadios_6ecd3c2c1d149f9a" 拜拜 ::
     IO ()

{-| __C declaration:__ @Say拜拜@

    __defined at:__ @edge-cases\/adios.h:31:6@

    __exported by:__ @edge-cases\/adios.h@

    __unique:__ @Example_Safe_Say拜拜@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesadios_580d4502b4a9e30b" say拜拜 ::
     IO ()
