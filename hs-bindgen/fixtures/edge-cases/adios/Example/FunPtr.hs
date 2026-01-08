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
  , "/* test_edgecasesadios_Example_get_\978 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0b1168f405aafe83 (void)) (void)"
  , "{"
  , "  return &\978;"
  , "}"
  , "/* test_edgecasesadios_Example_get_\25308\25308 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0a95358747546f1b (void)) (void)"
  , "{"
  , "  return &\25308\25308;"
  , "}"
  , "/* test_edgecasesadios_Example_get_Say\25308\25308 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a01e420336bfa879 (void)) (void)"
  , "{"
  , "  return &Say\25308\25308;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesadios_Example_get_ϒ@
foreign import ccall unsafe "hs_bindgen_0b1168f405aafe83" hs_bindgen_0b1168f405aafe83 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE cϒ #-}
{-| __C declaration:__ @ϒ@

    __defined at:__ @edge-cases\/adios.h 18:6@

    __exported by:__ @edge-cases\/adios.h@
-}
cϒ :: Ptr.FunPtr (IO ())
cϒ =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0b1168f405aafe83

-- __unique:__ @test_edgecasesadios_Example_get_拜拜@
foreign import ccall unsafe "hs_bindgen_0a95358747546f1b" hs_bindgen_0a95358747546f1b ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE 拜拜 #-}
{-| __C declaration:__ @拜拜@

    __defined at:__ @edge-cases\/adios.h 27:6@

    __exported by:__ @edge-cases\/adios.h@
-}
拜拜 :: Ptr.FunPtr (IO ())
拜拜 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0a95358747546f1b

-- __unique:__ @test_edgecasesadios_Example_get_Say拜拜@
foreign import ccall unsafe "hs_bindgen_a01e420336bfa879" hs_bindgen_a01e420336bfa879 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE say拜拜 #-}
{-| __C declaration:__ @Say拜拜@

    __defined at:__ @edge-cases\/adios.h 31:6@

    __exported by:__ @edge-cases\/adios.h@
-}
say拜拜 :: Ptr.FunPtr (IO ())
say拜拜 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a01e420336bfa879
