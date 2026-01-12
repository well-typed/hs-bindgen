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
  [ "#include <binding-specs/fun_arg/struct.h>"
  , "/* test_bindingspecsfun_argstruct_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c2e17b622d82efca (void)) ("
  , "  struct MyStruct arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argstruct_Example_get_fooA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cf2393be43816b19 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_bindingspecsfun_argstruct_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0b4bb6e5755e229a (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  , "/* test_bindingspecsfun_argstruct_Example_get_fooC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6c700bd5ed543a0d (void)) ("
  , "  C arg1"
  , ")"
  , "{"
  , "  return &fooC;"
  , "}"
  , "/* test_bindingspecsfun_argstruct_Example_get_fooD */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_14a38376ec3576df (void)) ("
  , "  D arg1"
  , ")"
  , "{"
  , "  return &fooD;"
  , "}"
  , "/* test_bindingspecsfun_argstruct_Example_get_fooE */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_35e4a51489fdc88e (void)) ("
  , "  E arg1"
  , ")"
  , "{"
  , "  return &fooE;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argstruct_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_c2e17b622d82efca" hs_bindgen_c2e17b622d82efca ::
     IO (Ptr.FunPtr (MyStruct -> IO ()))

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
foo :: Ptr.FunPtr (MyStruct -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c2e17b622d82efca

-- __unique:__ @test_bindingspecsfun_argstruct_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_cf2393be43816b19" hs_bindgen_cf2393be43816b19 ::
     IO (Ptr.FunPtr (A -> IO ()))

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
fooA :: Ptr.FunPtr (A -> IO ())
fooA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cf2393be43816b19

-- __unique:__ @test_bindingspecsfun_argstruct_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_0b4bb6e5755e229a" hs_bindgen_0b4bb6e5755e229a ::
     IO (Ptr.FunPtr (B -> IO ()))

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
fooB :: Ptr.FunPtr (B -> IO ())
fooB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0b4bb6e5755e229a

-- __unique:__ @test_bindingspecsfun_argstruct_Example_get_fooC@
foreign import ccall unsafe "hs_bindgen_6c700bd5ed543a0d" hs_bindgen_6c700bd5ed543a0d ::
     IO (Ptr.FunPtr (M.C -> IO ()))

{-# NOINLINE fooC #-}
{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
fooC :: Ptr.FunPtr (M.C -> IO ())
fooC =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6c700bd5ed543a0d

-- __unique:__ @test_bindingspecsfun_argstruct_Example_get_fooD@
foreign import ccall unsafe "hs_bindgen_14a38376ec3576df" hs_bindgen_14a38376ec3576df ::
     IO (Ptr.FunPtr (M.D -> IO ()))

{-# NOINLINE fooD #-}
{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 24:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
fooD :: Ptr.FunPtr (M.D -> IO ())
fooD =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_14a38376ec3576df

-- __unique:__ @test_bindingspecsfun_argstruct_Example_get_fooE@
foreign import ccall unsafe "hs_bindgen_35e4a51489fdc88e" hs_bindgen_35e4a51489fdc88e ::
     IO (Ptr.FunPtr (E -> IO ()))

{-# NOINLINE fooE #-}
{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 25:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
fooE :: Ptr.FunPtr (E -> IO ())
fooE =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_35e4a51489fdc88e
