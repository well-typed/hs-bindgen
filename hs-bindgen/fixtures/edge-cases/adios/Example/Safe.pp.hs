{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/adios.h>"
  , "void hs_bindgen_test_edgecasesadios_1f928c1e5a3ea8be (void)"
  , "{"
  , "  \978();"
  , "}"
  , "void hs_bindgen_test_edgecasesadios_912e938ac6370f83 (void)"
  , "{"
  , "  \25308\25308();"
  , "}"
  , "void hs_bindgen_test_edgecasesadios_cc7cd7984d0bfaee (void)"
  , "{"
  , "  Say\25308\25308();"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesadios_1f928c1e5a3ea8be" cϒ_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @ϒ@

    __defined at:__ @edge-cases\/adios.h:18:6@

    __exported by:__ @edge-cases\/adios.h@
-}
cϒ ::
     IO ()
cϒ =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType cϒ_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesadios_912e938ac6370f83" 拜拜_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @拜拜@

    __defined at:__ @edge-cases\/adios.h:27:6@

    __exported by:__ @edge-cases\/adios.h@
-}
拜拜 ::
     IO ()
拜拜 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType 拜拜_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesadios_cc7cd7984d0bfaee" say拜拜_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @Say拜拜@

    __defined at:__ @edge-cases\/adios.h:31:6@

    __exported by:__ @edge-cases\/adios.h@
-}
say拜拜 ::
     IO ()
say拜拜 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType say拜拜_base
