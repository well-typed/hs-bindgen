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
  [ "#include <binding-specs/fun_arg/macro/enum.h>"
  , "/* test_bindingspecsfun_argmacroen_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_098964a440956602 (void)) ("
  , "  enum MyEnum arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argmacroen_Example_get_fooA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_86b685b9d27ce50e (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_bindingspecsfun_argmacroen_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9383ae52414c2c19 (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  , "/* test_bindingspecsfun_argmacroen_Example_get_fooC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e677debf9f8f8884 (void)) ("
  , "  C arg1"
  , ")"
  , "{"
  , "  return &fooC;"
  , "}"
  , "/* test_bindingspecsfun_argmacroen_Example_get_fooD */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7d8aecf79b372251 (void)) ("
  , "  D arg1"
  , ")"
  , "{"
  , "  return &fooD;"
  , "}"
  , "/* test_bindingspecsfun_argmacroen_Example_get_fooE */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_24fe84901d848b4c (void)) ("
  , "  E arg1"
  , ")"
  , "{"
  , "  return &fooE;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_098964a440956602" hs_bindgen_098964a440956602_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_get_foo@
hs_bindgen_098964a440956602 :: IO (BG.FunPtr (MyEnum -> IO ()))
hs_bindgen_098964a440956602 =
  BG.fromFFIType hs_bindgen_098964a440956602_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 6:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
foo :: BG.FunPtr (MyEnum -> IO ())
foo = BG.unsafePerformIO hs_bindgen_098964a440956602

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_86b685b9d27ce50e" hs_bindgen_86b685b9d27ce50e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_get_fooA@
hs_bindgen_86b685b9d27ce50e :: IO (BG.FunPtr (A -> IO ()))
hs_bindgen_86b685b9d27ce50e =
  BG.fromFFIType hs_bindgen_86b685b9d27ce50e_base

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 12:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooA :: BG.FunPtr (A -> IO ())
fooA = BG.unsafePerformIO hs_bindgen_86b685b9d27ce50e

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_9383ae52414c2c19" hs_bindgen_9383ae52414c2c19_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_get_fooB@
hs_bindgen_9383ae52414c2c19 :: IO (BG.FunPtr (B -> IO ()))
hs_bindgen_9383ae52414c2c19 =
  BG.fromFFIType hs_bindgen_9383ae52414c2c19_base

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 13:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooB :: BG.FunPtr (B -> IO ())
fooB = BG.unsafePerformIO hs_bindgen_9383ae52414c2c19

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_get_fooC@
foreign import ccall unsafe "hs_bindgen_e677debf9f8f8884" hs_bindgen_e677debf9f8f8884_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_get_fooC@
hs_bindgen_e677debf9f8f8884 :: IO (BG.FunPtr (M.C -> IO ()))
hs_bindgen_e677debf9f8f8884 =
  BG.fromFFIType hs_bindgen_e677debf9f8f8884_base

{-# NOINLINE fooC #-}
{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooC :: BG.FunPtr (M.C -> IO ())
fooC = BG.unsafePerformIO hs_bindgen_e677debf9f8f8884

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_get_fooD@
foreign import ccall unsafe "hs_bindgen_7d8aecf79b372251" hs_bindgen_7d8aecf79b372251_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_get_fooD@
hs_bindgen_7d8aecf79b372251 :: IO (BG.FunPtr (M.D -> IO ()))
hs_bindgen_7d8aecf79b372251 =
  BG.fromFFIType hs_bindgen_7d8aecf79b372251_base

{-# NOINLINE fooD #-}
{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooD :: BG.FunPtr (M.D -> IO ())
fooD = BG.unsafePerformIO hs_bindgen_7d8aecf79b372251

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_get_fooE@
foreign import ccall unsafe "hs_bindgen_24fe84901d848b4c" hs_bindgen_24fe84901d848b4c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_get_fooE@
hs_bindgen_24fe84901d848b4c :: IO (BG.FunPtr (E -> IO ()))
hs_bindgen_24fe84901d848b4c =
  BG.fromFFIType hs_bindgen_24fe84901d848b4c_base

{-# NOINLINE fooE #-}
{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooE :: BG.FunPtr (E -> IO ())
fooE = BG.unsafePerformIO hs_bindgen_24fe84901d848b4c
