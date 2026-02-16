{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified M
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/typedef/array_known_size.h>"
  , "/* test_bindingspecsfun_argtypedef_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2020929269c10652 (void)) ("
  , "  signed int arg1[3]"
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
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_foo@
hs_bindgen_2020929269c10652 :: IO (RIP.FunPtr (((CA.ConstantArray 3) RIP.CInt) -> IO ()))
hs_bindgen_2020929269c10652 =
  RIP.fromFFIType hs_bindgen_2020929269c10652_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h@
-}
foo :: RIP.FunPtr (((CA.ConstantArray 3) RIP.CInt) -> IO ())
foo = RIP.unsafePerformIO hs_bindgen_2020929269c10652

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_3fbd7681308a7575" hs_bindgen_3fbd7681308a7575_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooA@
hs_bindgen_3fbd7681308a7575 :: IO (RIP.FunPtr (A -> IO ()))
hs_bindgen_3fbd7681308a7575 =
  RIP.fromFFIType hs_bindgen_3fbd7681308a7575_base

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h@
-}
fooA :: RIP.FunPtr (A -> IO ())
fooA =
  RIP.unsafePerformIO hs_bindgen_3fbd7681308a7575

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_118f9a485493ca2d" hs_bindgen_118f9a485493ca2d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooB@
hs_bindgen_118f9a485493ca2d :: IO (RIP.FunPtr (B -> IO ()))
hs_bindgen_118f9a485493ca2d =
  RIP.fromFFIType hs_bindgen_118f9a485493ca2d_base

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h@
-}
fooB :: RIP.FunPtr (B -> IO ())
fooB =
  RIP.unsafePerformIO hs_bindgen_118f9a485493ca2d

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooC@
foreign import ccall unsafe "hs_bindgen_dbe89d4787aae78a" hs_bindgen_dbe89d4787aae78a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooC@
hs_bindgen_dbe89d4787aae78a :: IO (RIP.FunPtr (M.C -> IO ()))
hs_bindgen_dbe89d4787aae78a =
  RIP.fromFFIType hs_bindgen_dbe89d4787aae78a_base

{-# NOINLINE fooC #-}
{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h@
-}
fooC :: RIP.FunPtr (M.C -> IO ())
fooC =
  RIP.unsafePerformIO hs_bindgen_dbe89d4787aae78a

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooD@
foreign import ccall unsafe "hs_bindgen_cf5702eef5ab2ac4" hs_bindgen_cf5702eef5ab2ac4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooD@
hs_bindgen_cf5702eef5ab2ac4 :: IO (RIP.FunPtr (M.D -> IO ()))
hs_bindgen_cf5702eef5ab2ac4 =
  RIP.fromFFIType hs_bindgen_cf5702eef5ab2ac4_base

{-# NOINLINE fooD #-}
{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h@
-}
fooD :: RIP.FunPtr (M.D -> IO ())
fooD =
  RIP.unsafePerformIO hs_bindgen_cf5702eef5ab2ac4

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooE@
foreign import ccall unsafe "hs_bindgen_dc10cdc4b07670d1" hs_bindgen_dc10cdc4b07670d1_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_fooE@
hs_bindgen_dc10cdc4b07670d1 :: IO (RIP.FunPtr (E -> IO ()))
hs_bindgen_dc10cdc4b07670d1 =
  RIP.fromFFIType hs_bindgen_dc10cdc4b07670d1_base

{-# NOINLINE fooE #-}
{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h@
-}
fooE :: RIP.FunPtr (E -> IO ())
fooE =
  RIP.unsafePerformIO hs_bindgen_dc10cdc4b07670d1
