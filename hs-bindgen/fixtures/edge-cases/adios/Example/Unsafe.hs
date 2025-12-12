{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/adios.h>"
  , "void hs_bindgen_1814d14d59d9daf7 (void)"
  , "{"
  , "  \978();"
  , "}"
  , "void hs_bindgen_c1ab9527e537714b (void)"
  , "{"
  , "  \25308\25308();"
  , "}"
  , "void hs_bindgen_d532055af9051fad (void)"
  , "{"
  , "  Say\25308\25308();"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_1814d14d59d9daf7" cϒ_base ::
     IO ()

{-| __C declaration:__ @ϒ@

    __defined at:__ @edge-cases\/adios.h:18:6@

    __exported by:__ @edge-cases\/adios.h@

    __unique:__ @test_edgecasesadios_Example_Unsafe_ϒ@
-}
cϒ ::
     IO ()
cϒ =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType cϒ_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c1ab9527e537714b" 拜拜_base ::
     IO ()

{-| __C declaration:__ @拜拜@

    __defined at:__ @edge-cases\/adios.h:27:6@

    __exported by:__ @edge-cases\/adios.h@

    __unique:__ @test_edgecasesadios_Example_Unsafe_拜拜@
-}
拜拜 ::
     IO ()
拜拜 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType 拜拜_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_d532055af9051fad" say拜拜_base ::
     IO ()

{-| __C declaration:__ @Say拜拜@

    __defined at:__ @edge-cases\/adios.h:31:6@

    __exported by:__ @edge-cases\/adios.h@

    __unique:__ @test_edgecasesadios_Example_Unsafe_Say拜拜@
-}
say拜拜 ::
     IO ()
say拜拜 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType say拜拜_base
