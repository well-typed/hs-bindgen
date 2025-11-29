{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/adios.h>"
  , "void hs_bindgen_test_edgecasesadios_82fab26db9547005 (void)"
  , "{"
  , "  \978();"
  , "}"
  , "void hs_bindgen_test_edgecasesadios_ad1afd0d0a11937f (void)"
  , "{"
  , "  \25308\25308();"
  , "}"
  , "void hs_bindgen_test_edgecasesadios_9a2b7b543a500f7d (void)"
  , "{"
  , "  Say\25308\25308();"
  , "}"
  ]))

{-| __C declaration:__ @ϒ@

    __defined at:__ @edge-cases\/adios.h:18:6@

    __exported by:__ @edge-cases\/adios.h@

    __unique:__ @ExampleJust Unsafeϒ@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_82fab26db9547005" cϒ ::
     IO ()

{-| __C declaration:__ @拜拜@

    __defined at:__ @edge-cases\/adios.h:27:6@

    __exported by:__ @edge-cases\/adios.h@

    __unique:__ @ExampleJust Unsafe拜拜@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_ad1afd0d0a11937f" 拜拜 ::
     IO ()

{-| __C declaration:__ @Say拜拜@

    __defined at:__ @edge-cases\/adios.h:31:6@

    __exported by:__ @edge-cases\/adios.h@

    __unique:__ @ExampleJust UnsafeSay拜拜@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_9a2b7b543a500f7d" say拜拜 ::
     IO ()
