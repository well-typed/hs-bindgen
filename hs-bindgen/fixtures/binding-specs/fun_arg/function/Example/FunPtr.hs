{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified M
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/function.h>"
  , "/* test_bindingspecsfun_argfunction_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8a0babf894a65b5f (void)) ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argfunction_Example_get_fooA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a7d32556fb0befb2 (void)) ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_bindingspecsfun_argfunction_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_568bf4fa5a80b6d4 (void)) ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  , "/* test_bindingspecsfun_argfunction_Example_get_fooC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_79ba57311c1537e9 (void)) ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  return &fooC;"
  , "}"
  , "/* test_bindingspecsfun_argfunction_Example_get_fooD */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e9befcc69cbf152b (void)) ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  return &fooD;"
  , "}"
  , "/* test_bindingspecsfun_argfunction_Example_get_fooE */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e1c0f75003a4e64d (void)) ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  return &fooE;"
  , "}"
  , "/* test_bindingspecsfun_argfunction_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_bb54c0055348a008 (void)) ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  , "/* test_bindingspecsfun_argfunction_Example_get_barA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a80e113a9839a869 (void)) ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  return &barA;"
  , "}"
  , "/* test_bindingspecsfun_argfunction_Example_get_barB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_911053d3f3a619b7 (void)) ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  return &barB;"
  , "}"
  , "/* test_bindingspecsfun_argfunction_Example_get_barC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cdc4cc959ee149f8 (void)) ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  return &barC;"
  , "}"
  , "/* test_bindingspecsfun_argfunction_Example_get_barD */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e17be35f67e6c7ca (void)) ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  return &barD;"
  , "}"
  , "/* test_bindingspecsfun_argfunction_Example_get_barE */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_956eed2c9d6ddacf (void)) ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  return &barE;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_8a0babf894a65b5f" hs_bindgen_8a0babf894a65b5f ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO ()))

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/function.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
foo :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8a0babf894a65b5f

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_a7d32556fb0befb2" hs_bindgen_a7d32556fb0befb2 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr A) -> IO ()))

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/function.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
fooA :: Ptr.FunPtr ((Ptr.FunPtr A) -> IO ())
fooA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a7d32556fb0befb2

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_568bf4fa5a80b6d4" hs_bindgen_568bf4fa5a80b6d4 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr B) -> IO ()))

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/function.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
fooB :: Ptr.FunPtr ((Ptr.FunPtr B) -> IO ())
fooB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_568bf4fa5a80b6d4

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_fooC@
foreign import ccall unsafe "hs_bindgen_79ba57311c1537e9" hs_bindgen_79ba57311c1537e9 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr M.C) -> IO ()))

{-# NOINLINE fooC #-}
{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/function.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
fooC :: Ptr.FunPtr ((Ptr.FunPtr M.C) -> IO ())
fooC =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_79ba57311c1537e9

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_fooD@
foreign import ccall unsafe "hs_bindgen_e9befcc69cbf152b" hs_bindgen_e9befcc69cbf152b ::
     IO (Ptr.FunPtr ((Ptr.FunPtr M.D) -> IO ()))

{-# NOINLINE fooD #-}
{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/function.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
fooD :: Ptr.FunPtr ((Ptr.FunPtr M.D) -> IO ())
fooD =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e9befcc69cbf152b

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_fooE@
foreign import ccall unsafe "hs_bindgen_e1c0f75003a4e64d" hs_bindgen_e1c0f75003a4e64d ::
     IO (Ptr.FunPtr ((Ptr.FunPtr E) -> IO ()))

{-# NOINLINE fooE #-}
{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/function.h 24:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
fooE :: Ptr.FunPtr ((Ptr.FunPtr E) -> IO ())
fooE =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e1c0f75003a4e64d

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_bb54c0055348a008" hs_bindgen_bb54c0055348a008 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO ()))

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/function.h 29:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
bar :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO ())
bar =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bb54c0055348a008

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_barA@
foreign import ccall unsafe "hs_bindgen_a80e113a9839a869" hs_bindgen_a80e113a9839a869 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr A) -> IO ()))

{-# NOINLINE barA #-}
{-| __C declaration:__ @barA@

    __defined at:__ @binding-specs\/fun_arg\/function.h 31:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
barA :: Ptr.FunPtr ((Ptr.FunPtr A) -> IO ())
barA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a80e113a9839a869

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_barB@
foreign import ccall unsafe "hs_bindgen_911053d3f3a619b7" hs_bindgen_911053d3f3a619b7 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr B) -> IO ()))

{-# NOINLINE barB #-}
{-| __C declaration:__ @barB@

    __defined at:__ @binding-specs\/fun_arg\/function.h 32:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
barB :: Ptr.FunPtr ((Ptr.FunPtr B) -> IO ())
barB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_911053d3f3a619b7

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_barC@
foreign import ccall unsafe "hs_bindgen_cdc4cc959ee149f8" hs_bindgen_cdc4cc959ee149f8 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr M.C) -> IO ()))

{-# NOINLINE barC #-}
{-| __C declaration:__ @barC@

    __defined at:__ @binding-specs\/fun_arg\/function.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
barC :: Ptr.FunPtr ((Ptr.FunPtr M.C) -> IO ())
barC =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cdc4cc959ee149f8

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_barD@
foreign import ccall unsafe "hs_bindgen_e17be35f67e6c7ca" hs_bindgen_e17be35f67e6c7ca ::
     IO (Ptr.FunPtr ((Ptr.FunPtr M.D) -> IO ()))

{-# NOINLINE barD #-}
{-| __C declaration:__ @barD@

    __defined at:__ @binding-specs\/fun_arg\/function.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
barD :: Ptr.FunPtr ((Ptr.FunPtr M.D) -> IO ())
barD =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e17be35f67e6c7ca

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_barE@
foreign import ccall unsafe "hs_bindgen_956eed2c9d6ddacf" hs_bindgen_956eed2c9d6ddacf ::
     IO (Ptr.FunPtr ((Ptr.FunPtr E) -> IO ()))

{-# NOINLINE barE #-}
{-| __C declaration:__ @barE@

    __defined at:__ @binding-specs\/fun_arg\/function.h 36:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
barE :: Ptr.FunPtr ((Ptr.FunPtr E) -> IO ())
barE =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_956eed2c9d6ddacf
