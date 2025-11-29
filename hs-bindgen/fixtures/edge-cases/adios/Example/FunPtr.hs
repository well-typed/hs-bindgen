{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/adios.h>"
  , "/* ExampleNothingget_\978_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesadios_857cc80028e9fd4d (void)) (void)"
  , "{"
  , "  return &\978;"
  , "}"
  , "/* ExampleNothingget_\25308\25308_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesadios_8b289d4c7ae2c2a7 (void)) (void)"
  , "{"
  , "  return &\25308\25308;"
  , "}"
  , "/* ExampleNothingget_Say\25308\25308_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesadios_2879b42f75005d3b (void)) (void)"
  , "{"
  , "  return &Say\25308\25308;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_ϒ_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_857cc80028e9fd4d" hs_bindgen_test_edgecasesadios_857cc80028e9fd4d ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE cϒ_ptr #-}

{-| __C declaration:__ @ϒ@

    __defined at:__ @edge-cases\/adios.h:18:6@

    __exported by:__ @edge-cases\/adios.h@
-}
cϒ_ptr :: Ptr.FunPtr (IO ())
cϒ_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesadios_857cc80028e9fd4d

{-| __unique:__ @ExampleNothingget_拜拜_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_8b289d4c7ae2c2a7" hs_bindgen_test_edgecasesadios_8b289d4c7ae2c2a7 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE 拜拜_ptr #-}

{-| __C declaration:__ @拜拜@

    __defined at:__ @edge-cases\/adios.h:27:6@

    __exported by:__ @edge-cases\/adios.h@
-}
拜拜_ptr :: Ptr.FunPtr (IO ())
拜拜_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesadios_8b289d4c7ae2c2a7

{-| __unique:__ @ExampleNothingget_Say拜拜_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_2879b42f75005d3b" hs_bindgen_test_edgecasesadios_2879b42f75005d3b ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE say拜拜_ptr #-}

{-| __C declaration:__ @Say拜拜@

    __defined at:__ @edge-cases\/adios.h:31:6@

    __exported by:__ @edge-cases\/adios.h@
-}
say拜拜_ptr :: Ptr.FunPtr (IO ())
say拜拜_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesadios_2879b42f75005d3b
