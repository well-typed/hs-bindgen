{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/adios.h>"
  , "void hs_bindgen_2010521804ef9a6e (void)"
  , "{"
  , "  \978();"
  , "}"
  , "void hs_bindgen_3bc3e53cc82c9580 (void)"
  , "{"
  , "  \25308\25308();"
  , "}"
  , "void hs_bindgen_ad8eb47027b2d49d (void)"
  , "{"
  , "  Say\25308\25308();"
  , "}"
  ]))

{-| __C declaration:__ @ϒ@

    __defined at:__ @edge-cases\/adios.h:18:6@

    __exported by:__ @edge-cases\/adios.h@

    __unique:__ @test_edgecasesadios_Example_Safe_ϒ@
-}
foreign import ccall safe "hs_bindgen_2010521804ef9a6e" cϒ ::
     IO ()

{-| __C declaration:__ @拜拜@

    __defined at:__ @edge-cases\/adios.h:27:6@

    __exported by:__ @edge-cases\/adios.h@

    __unique:__ @test_edgecasesadios_Example_Safe_拜拜@
-}
foreign import ccall safe "hs_bindgen_3bc3e53cc82c9580" 拜拜 ::
     IO ()

{-| __C declaration:__ @Say拜拜@

    __defined at:__ @edge-cases\/adios.h:31:6@

    __exported by:__ @edge-cases\/adios.h@

    __unique:__ @test_edgecasesadios_Example_Safe_Say拜拜@
-}
foreign import ccall safe "hs_bindgen_ad8eb47027b2d49d" say拜拜 ::
     IO ()
