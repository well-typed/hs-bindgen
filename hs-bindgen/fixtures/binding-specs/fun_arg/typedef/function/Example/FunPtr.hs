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
import qualified M
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <binding-specs/fun_arg/typedef/function.h>"
  , "/* test_bindingspecsfun_argtypedef_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2020929269c10652 (void)) ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argtypedef_Example_get_fooA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3fbd7681308a7575 (void)) ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_bindingspecsfun_argtypedef_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_118f9a485493ca2d (void)) ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  , "/* test_bindingspecsfun_argtypedef_Example_get_fooC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_dbe89d4787aae78a (void)) ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  return &fooC;"
  , "}"
  , "/* test_bindingspecsfun_argtypedef_Example_get_fooD */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cf5702eef5ab2ac4 (void)) ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  return &fooD;"
  , "}"
  , "/* test_bindingspecsfun_argtypedef_Example_get_fooE */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_dc10cdc4b07670d1 (void)) ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  return &fooE;"
  , "}"
  , "/* test_bindingspecsfun_argtypedef_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d07fcdceb06f5a2c (void)) ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  , "/* test_bindingspecsfun_argtypedef_Example_get_barA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_807763d49e9b12a2 (void)) ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  return &barA;"
  , "}"
  , "/* test_bindingspecsfun_argtypedef_Example_get_barB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_978114a8cf46bc2b (void)) ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  return &barB;"
  , "}"
  , "/* test_bindingspecsfun_argtypedef_Example_get_barC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8db1266f28c7b15e (void)) ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  return &barC;"
  , "}"
  , "/* test_bindingspecsfun_argtypedef_Example_get_barD */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_643d72e5cfddafd7 (void)) ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  return &barD;"
  , "}"
  , "/* test_bindingspecsfun_argtypedef_Example_get_barE */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_659e6f20e0c4f384 (void)) ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  return &barE;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_2020929269c10652" hs_bindgen_2020929269c10652_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_foo@
hs_bindgen_2020929269c10652 :: IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO ()))
hs_bindgen_2020929269c10652 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_2020929269c10652_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
foo :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2020929269c10652

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_3fbd7681308a7575" hs_bindgen_3fbd7681308a7575_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooA@
hs_bindgen_3fbd7681308a7575 :: IO (Ptr.FunPtr ((Ptr.FunPtr A) -> IO ()))
hs_bindgen_3fbd7681308a7575 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_3fbd7681308a7575_base

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooA :: Ptr.FunPtr ((Ptr.FunPtr A) -> IO ())
fooA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3fbd7681308a7575

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_118f9a485493ca2d" hs_bindgen_118f9a485493ca2d_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooB@
hs_bindgen_118f9a485493ca2d :: IO (Ptr.FunPtr ((Ptr.FunPtr B) -> IO ()))
hs_bindgen_118f9a485493ca2d =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_118f9a485493ca2d_base

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooB :: Ptr.FunPtr ((Ptr.FunPtr B) -> IO ())
fooB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_118f9a485493ca2d

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooC@
foreign import ccall unsafe "hs_bindgen_dbe89d4787aae78a" hs_bindgen_dbe89d4787aae78a_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooC@
hs_bindgen_dbe89d4787aae78a :: IO (Ptr.FunPtr ((Ptr.FunPtr M.C) -> IO ()))
hs_bindgen_dbe89d4787aae78a =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_dbe89d4787aae78a_base

{-# NOINLINE fooC #-}
{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooC :: Ptr.FunPtr ((Ptr.FunPtr M.C) -> IO ())
fooC =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dbe89d4787aae78a

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooD@
foreign import ccall unsafe "hs_bindgen_cf5702eef5ab2ac4" hs_bindgen_cf5702eef5ab2ac4_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooD@
hs_bindgen_cf5702eef5ab2ac4 :: IO (Ptr.FunPtr ((Ptr.FunPtr M.D) -> IO ()))
hs_bindgen_cf5702eef5ab2ac4 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_cf5702eef5ab2ac4_base

{-# NOINLINE fooD #-}
{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooD :: Ptr.FunPtr ((Ptr.FunPtr M.D) -> IO ())
fooD =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cf5702eef5ab2ac4

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooE@
foreign import ccall unsafe "hs_bindgen_dc10cdc4b07670d1" hs_bindgen_dc10cdc4b07670d1_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooE@
hs_bindgen_dc10cdc4b07670d1 :: IO (Ptr.FunPtr ((Ptr.FunPtr E) -> IO ()))
hs_bindgen_dc10cdc4b07670d1 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_dc10cdc4b07670d1_base

{-# NOINLINE fooE #-}
{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooE :: Ptr.FunPtr ((Ptr.FunPtr E) -> IO ())
fooE =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dc10cdc4b07670d1

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_d07fcdceb06f5a2c" hs_bindgen_d07fcdceb06f5a2c_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_bar@
hs_bindgen_d07fcdceb06f5a2c :: IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO ()))
hs_bindgen_d07fcdceb06f5a2c =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d07fcdceb06f5a2c_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 28:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
bar :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO ())
bar =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d07fcdceb06f5a2c

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_barA@
foreign import ccall unsafe "hs_bindgen_807763d49e9b12a2" hs_bindgen_807763d49e9b12a2_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_barA@
hs_bindgen_807763d49e9b12a2 :: IO (Ptr.FunPtr ((Ptr.FunPtr A) -> IO ()))
hs_bindgen_807763d49e9b12a2 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_807763d49e9b12a2_base

{-# NOINLINE barA #-}
{-| __C declaration:__ @barA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 30:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barA :: Ptr.FunPtr ((Ptr.FunPtr A) -> IO ())
barA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_807763d49e9b12a2

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_barB@
foreign import ccall unsafe "hs_bindgen_978114a8cf46bc2b" hs_bindgen_978114a8cf46bc2b_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_barB@
hs_bindgen_978114a8cf46bc2b :: IO (Ptr.FunPtr ((Ptr.FunPtr B) -> IO ()))
hs_bindgen_978114a8cf46bc2b =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_978114a8cf46bc2b_base

{-# NOINLINE barB #-}
{-| __C declaration:__ @barB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 31:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barB :: Ptr.FunPtr ((Ptr.FunPtr B) -> IO ())
barB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_978114a8cf46bc2b

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_barC@
foreign import ccall unsafe "hs_bindgen_8db1266f28c7b15e" hs_bindgen_8db1266f28c7b15e_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_barC@
hs_bindgen_8db1266f28c7b15e :: IO (Ptr.FunPtr ((Ptr.FunPtr M.C) -> IO ()))
hs_bindgen_8db1266f28c7b15e =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_8db1266f28c7b15e_base

{-# NOINLINE barC #-}
{-| __C declaration:__ @barC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barC :: Ptr.FunPtr ((Ptr.FunPtr M.C) -> IO ())
barC =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8db1266f28c7b15e

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_barD@
foreign import ccall unsafe "hs_bindgen_643d72e5cfddafd7" hs_bindgen_643d72e5cfddafd7_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_barD@
hs_bindgen_643d72e5cfddafd7 :: IO (Ptr.FunPtr ((Ptr.FunPtr M.D) -> IO ()))
hs_bindgen_643d72e5cfddafd7 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_643d72e5cfddafd7_base

{-# NOINLINE barD #-}
{-| __C declaration:__ @barD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barD :: Ptr.FunPtr ((Ptr.FunPtr M.D) -> IO ())
barD =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_643d72e5cfddafd7

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_barE@
foreign import ccall unsafe "hs_bindgen_659e6f20e0c4f384" hs_bindgen_659e6f20e0c4f384_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_barE@
hs_bindgen_659e6f20e0c4f384 :: IO (Ptr.FunPtr ((Ptr.FunPtr E) -> IO ()))
hs_bindgen_659e6f20e0c4f384 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_659e6f20e0c4f384_base

{-# NOINLINE barE #-}
{-| __C declaration:__ @barE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barE :: Ptr.FunPtr ((Ptr.FunPtr E) -> IO ())
barE =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_659e6f20e0c4f384
