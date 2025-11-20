{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Marshallable
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

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_82fab26db9547005" cϒ_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_ad1afd0d0a11937f" 拜拜_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_9a2b7b543a500f7d" say拜拜_base ::
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
