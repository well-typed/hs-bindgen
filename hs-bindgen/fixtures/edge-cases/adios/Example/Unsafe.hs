{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/adios.h>"
  , "void hs_bindgen_test_edgecasesadios_55aa919e2d938fc8 (void)"
  , "{"
  , "  \978();"
  , "}"
  , "void hs_bindgen_test_edgecasesadios_a52cb7c7d22cbd18 (void)"
  , "{"
  , "  \25308\25308();"
  , "}"
  , "void hs_bindgen_test_edgecasesadios_ddda4c229467f4aa (void)"
  , "{"
  , "  Say\25308\25308();"
  , "}"
  ]))

{-| __C declaration:__ @ϒ@

    __defined at:__ @edge-cases\/adios.h:18:6@

    __exported by:__ @edge-cases\/adios.h@

    __unique:__ @Example_Unsafe_ϒ@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_55aa919e2d938fc8" cϒ ::
     IO ()

{-| __C declaration:__ @拜拜@

    __defined at:__ @edge-cases\/adios.h:27:6@

    __exported by:__ @edge-cases\/adios.h@

    __unique:__ @Example_Unsafe_拜拜@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_a52cb7c7d22cbd18" 拜拜 ::
     IO ()

{-| __C declaration:__ @Say拜拜@

    __defined at:__ @edge-cases\/adios.h:31:6@

    __exported by:__ @edge-cases\/adios.h@

    __unique:__ @Example_Unsafe_Say拜拜@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_ddda4c229467f4aa" say拜拜 ::
     IO ()
