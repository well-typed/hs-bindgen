{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    , Example.FunPtr.fooA
    , Example.FunPtr.fooB
    , Example.FunPtr.fooC
    , Example.FunPtr.fooD
    , Example.FunPtr.fooE
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/fun_arg/typedef/function_pointer.h>"
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
  , "  A arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_bindingspecsfun_argtypedef_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_118f9a485493ca2d (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  , "/* test_bindingspecsfun_argtypedef_Example_get_fooC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_dbe89d4787aae78a (void)) ("
  , "  C arg1"
  , ")"
  , "{"
  , "  return &fooC;"
  , "}"
  , "/* test_bindingspecsfun_argtypedef_Example_get_fooD */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cf5702eef5ab2ac4 (void)) ("
  , "  D arg1"
  , ")"
  , "{"
  , "  return &fooD;"
  , "}"
  , "/* test_bindingspecsfun_argtypedef_Example_get_fooE */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_dc10cdc4b07670d1 (void)) ("
  , "  E arg1"
  , ")"
  , "{"
  , "  return &fooE;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_2020929269c10652" hs_bindgen_2020929269c10652_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_foo@
hs_bindgen_2020929269c10652 :: IO (BG.FunPtr (BG.FunPtr (BG.CInt -> IO BG.CInt) -> IO ()))
hs_bindgen_2020929269c10652 =
  BG.fromFFIType hs_bindgen_2020929269c10652_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
foo :: BG.FunPtr (BG.FunPtr (BG.CInt -> IO BG.CInt) -> IO ())
foo = BG.unsafePerformIO hs_bindgen_2020929269c10652

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_3fbd7681308a7575" hs_bindgen_3fbd7681308a7575_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooA@
hs_bindgen_3fbd7681308a7575 :: IO (BG.FunPtr (A -> IO ()))
hs_bindgen_3fbd7681308a7575 =
  BG.fromFFIType hs_bindgen_3fbd7681308a7575_base

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
fooA :: BG.FunPtr (A -> IO ())
fooA = BG.unsafePerformIO hs_bindgen_3fbd7681308a7575

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_118f9a485493ca2d" hs_bindgen_118f9a485493ca2d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooB@
hs_bindgen_118f9a485493ca2d :: IO (BG.FunPtr (B -> IO ()))
hs_bindgen_118f9a485493ca2d =
  BG.fromFFIType hs_bindgen_118f9a485493ca2d_base

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
fooB :: BG.FunPtr (B -> IO ())
fooB = BG.unsafePerformIO hs_bindgen_118f9a485493ca2d

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooC@
foreign import ccall unsafe "hs_bindgen_dbe89d4787aae78a" hs_bindgen_dbe89d4787aae78a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooC@
hs_bindgen_dbe89d4787aae78a :: IO (BG.FunPtr (M.C -> IO ()))
hs_bindgen_dbe89d4787aae78a =
  BG.fromFFIType hs_bindgen_dbe89d4787aae78a_base

{-# NOINLINE fooC #-}
{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
fooC :: BG.FunPtr (M.C -> IO ())
fooC = BG.unsafePerformIO hs_bindgen_dbe89d4787aae78a

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooD@
foreign import ccall unsafe "hs_bindgen_cf5702eef5ab2ac4" hs_bindgen_cf5702eef5ab2ac4_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooD@
hs_bindgen_cf5702eef5ab2ac4 :: IO (BG.FunPtr (M.D -> IO ()))
hs_bindgen_cf5702eef5ab2ac4 =
  BG.fromFFIType hs_bindgen_cf5702eef5ab2ac4_base

{-# NOINLINE fooD #-}
{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
fooD :: BG.FunPtr (M.D -> IO ())
fooD = BG.unsafePerformIO hs_bindgen_cf5702eef5ab2ac4

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooE@
foreign import ccall unsafe "hs_bindgen_dc10cdc4b07670d1" hs_bindgen_dc10cdc4b07670d1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooE@
hs_bindgen_dc10cdc4b07670d1 :: IO (BG.FunPtr (E -> IO ()))
hs_bindgen_dc10cdc4b07670d1 =
  BG.fromFFIType hs_bindgen_dc10cdc4b07670d1_base

{-# NOINLINE fooE #-}
{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
fooE :: BG.FunPtr (E -> IO ())
fooE = BG.unsafePerformIO hs_bindgen_dc10cdc4b07670d1
