{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.HasFFIType
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <edge-cases/adios.h>"
  , "/* test_edgecasesadios_Example_get_adio\769s_fun */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_4ac23afef85d3af0 (void)) (void)"
  , "{"
  , "  return &adio\769s_fun;"
  , "}"
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

-- __unique:__ @test_edgecasesadios_Example_get_adiós_fun@
foreign import ccall unsafe "hs_bindgen_4ac23afef85d3af0" hs_bindgen_4ac23afef85d3af0_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesadios_Example_get_adiós_fun@
hs_bindgen_4ac23afef85d3af0 :: IO (Ptr.FunPtr (IO FC.CInt))
hs_bindgen_4ac23afef85d3af0 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_4ac23afef85d3af0_base

{-# NOINLINE adio'0301s_fun #-}
{-| __C declaration:__ @adiós_fun@

    __defined at:__ @edge-cases\/adios.h 12:5@

    __exported by:__ @edge-cases\/adios.h@
-}
adio'0301s_fun :: Ptr.FunPtr (IO FC.CInt)
adio'0301s_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4ac23afef85d3af0

-- __unique:__ @test_edgecasesadios_Example_get_ϒ@
foreign import ccall unsafe "hs_bindgen_0b1168f405aafe83" hs_bindgen_0b1168f405aafe83_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesadios_Example_get_ϒ@
hs_bindgen_0b1168f405aafe83 :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_0b1168f405aafe83 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_0b1168f405aafe83_base

{-# NOINLINE cϒ #-}
{-| __C declaration:__ @ϒ@

    __defined at:__ @edge-cases\/adios.h 23:6@

    __exported by:__ @edge-cases\/adios.h@
-}
cϒ :: Ptr.FunPtr (IO ())
cϒ =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0b1168f405aafe83

-- __unique:__ @test_edgecasesadios_Example_get_拜拜@
foreign import ccall unsafe "hs_bindgen_0a95358747546f1b" hs_bindgen_0a95358747546f1b_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesadios_Example_get_拜拜@
hs_bindgen_0a95358747546f1b :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_0a95358747546f1b =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_0a95358747546f1b_base

{-# NOINLINE 拜拜 #-}
{-| __C declaration:__ @拜拜@

    __defined at:__ @edge-cases\/adios.h 32:6@

    __exported by:__ @edge-cases\/adios.h@
-}
拜拜 :: Ptr.FunPtr (IO ())
拜拜 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0a95358747546f1b

-- __unique:__ @test_edgecasesadios_Example_get_Say拜拜@
foreign import ccall unsafe "hs_bindgen_a01e420336bfa879" hs_bindgen_a01e420336bfa879_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesadios_Example_get_Say拜拜@
hs_bindgen_a01e420336bfa879 :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_a01e420336bfa879 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_a01e420336bfa879_base

{-# NOINLINE say拜拜 #-}
{-| __C declaration:__ @Say拜拜@

    __defined at:__ @edge-cases\/adios.h 36:6@

    __exported by:__ @edge-cases\/adios.h@
-}
say拜拜 :: Ptr.FunPtr (IO ())
say拜拜 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a01e420336bfa879
