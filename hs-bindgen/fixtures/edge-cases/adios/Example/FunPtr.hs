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
  , "/* Example_get_\978_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesadios_4ee45318a3dadc32 (void)) (void)"
  , "{"
  , "  return &\978;"
  , "}"
  , "/* Example_get_\25308\25308_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesadios_8ca938fecd1fdc3e (void)) (void)"
  , "{"
  , "  return &\25308\25308;"
  , "}"
  , "/* Example_get_Say\25308\25308_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesadios_0cd292c555830a4c (void)) (void)"
  , "{"
  , "  return &Say\25308\25308;"
  , "}"
  ]))

{-| __unique:__ @Example_get_ϒ_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_4ee45318a3dadc32" hs_bindgen_test_edgecasesadios_4ee45318a3dadc32 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE cϒ_ptr #-}

{-| __C declaration:__ @ϒ@

    __defined at:__ @edge-cases\/adios.h:18:6@

    __exported by:__ @edge-cases\/adios.h@
-}
cϒ_ptr :: Ptr.FunPtr (IO ())
cϒ_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesadios_4ee45318a3dadc32

{-| __unique:__ @Example_get_拜拜_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_8ca938fecd1fdc3e" hs_bindgen_test_edgecasesadios_8ca938fecd1fdc3e ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE 拜拜_ptr #-}

{-| __C declaration:__ @拜拜@

    __defined at:__ @edge-cases\/adios.h:27:6@

    __exported by:__ @edge-cases\/adios.h@
-}
拜拜_ptr :: Ptr.FunPtr (IO ())
拜拜_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesadios_8ca938fecd1fdc3e

{-| __unique:__ @Example_get_Say拜拜_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_0cd292c555830a4c" hs_bindgen_test_edgecasesadios_0cd292c555830a4c ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE say拜拜_ptr #-}

{-| __C declaration:__ @Say拜拜@

    __defined at:__ @edge-cases\/adios.h:31:6@

    __exported by:__ @edge-cases\/adios.h@
-}
say拜拜_ptr :: Ptr.FunPtr (IO ())
say拜拜_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesadios_0cd292c555830a4c
