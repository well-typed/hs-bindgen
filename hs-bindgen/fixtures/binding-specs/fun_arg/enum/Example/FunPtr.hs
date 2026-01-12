{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified M
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/enum.h>"
  , "/* test_bindingspecsfun_argenum_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ba3da81519e4d7fd (void)) ("
  , "  enum MyEnum arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argenum_Example_get_fooA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_083abf6b0966267d (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_bindingspecsfun_argenum_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2e34202e0d19b6a0 (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  , "/* test_bindingspecsfun_argenum_Example_get_fooC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fc13499d2a422fe0 (void)) ("
  , "  C arg1"
  , ")"
  , "{"
  , "  return &fooC;"
  , "}"
  , "/* test_bindingspecsfun_argenum_Example_get_fooD */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_22ba7e22852829ac (void)) ("
  , "  D arg1"
  , ")"
  , "{"
  , "  return &fooD;"
  , "}"
  , "/* test_bindingspecsfun_argenum_Example_get_fooE */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c8e5bd1a07bfbb09 (void)) ("
  , "  E arg1"
  , ")"
  , "{"
  , "  return &fooE;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argenum_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_ba3da81519e4d7fd" hs_bindgen_ba3da81519e4d7fd ::
     IO (Ptr.FunPtr (MyEnum -> IO ()))

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/enum.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
foo :: Ptr.FunPtr (MyEnum -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ba3da81519e4d7fd

-- __unique:__ @test_bindingspecsfun_argenum_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_083abf6b0966267d" hs_bindgen_083abf6b0966267d ::
     IO (Ptr.FunPtr (A -> IO ()))

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/enum.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
fooA :: Ptr.FunPtr (A -> IO ())
fooA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_083abf6b0966267d

-- __unique:__ @test_bindingspecsfun_argenum_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_2e34202e0d19b6a0" hs_bindgen_2e34202e0d19b6a0 ::
     IO (Ptr.FunPtr (B -> IO ()))

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/enum.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
fooB :: Ptr.FunPtr (B -> IO ())
fooB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2e34202e0d19b6a0

-- __unique:__ @test_bindingspecsfun_argenum_Example_get_fooC@
foreign import ccall unsafe "hs_bindgen_fc13499d2a422fe0" hs_bindgen_fc13499d2a422fe0 ::
     IO (Ptr.FunPtr (M.C -> IO ()))

{-# NOINLINE fooC #-}
{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/enum.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
fooC :: Ptr.FunPtr (M.C -> IO ())
fooC =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fc13499d2a422fe0

-- __unique:__ @test_bindingspecsfun_argenum_Example_get_fooD@
foreign import ccall unsafe "hs_bindgen_22ba7e22852829ac" hs_bindgen_22ba7e22852829ac ::
     IO (Ptr.FunPtr (M.D -> IO ()))

{-# NOINLINE fooD #-}
{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/enum.h 24:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
fooD :: Ptr.FunPtr (M.D -> IO ())
fooD =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_22ba7e22852829ac

-- __unique:__ @test_bindingspecsfun_argenum_Example_get_fooE@
foreign import ccall unsafe "hs_bindgen_c8e5bd1a07bfbb09" hs_bindgen_c8e5bd1a07bfbb09 ::
     IO (Ptr.FunPtr (E -> IO ()))

{-# NOINLINE fooE #-}
{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/enum.h 25:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
fooE :: Ptr.FunPtr (E -> IO ())
fooE =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c8e5bd1a07bfbb09
