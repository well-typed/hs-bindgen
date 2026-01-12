{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Prelude
import qualified M
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/array_known_size.h>"
  , "/* test_bindingspecsfun_argarray_kn_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_630054fbd94e8bcc (void)) ("
  , "  signed int arg1[3]"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argarray_kn_Example_get_fooA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_64c558fbdff0aa6f (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_bindingspecsfun_argarray_kn_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_138f25a1c035e491 (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  , "/* test_bindingspecsfun_argarray_kn_Example_get_fooC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e44396c12491a301 (void)) ("
  , "  C arg1"
  , ")"
  , "{"
  , "  return &fooC;"
  , "}"
  , "/* test_bindingspecsfun_argarray_kn_Example_get_fooD */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_822cfa2df2dabce9 (void)) ("
  , "  D arg1"
  , ")"
  , "{"
  , "  return &fooD;"
  , "}"
  , "/* test_bindingspecsfun_argarray_kn_Example_get_fooE */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cb3b428d93762ce9 (void)) ("
  , "  E arg1"
  , ")"
  , "{"
  , "  return &fooE;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_630054fbd94e8bcc" hs_bindgen_630054fbd94e8bcc ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO ()))

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
foo :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_630054fbd94e8bcc

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_64c558fbdff0aa6f" hs_bindgen_64c558fbdff0aa6f ::
     IO (Ptr.FunPtr (A -> IO ()))

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
fooA :: Ptr.FunPtr (A -> IO ())
fooA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_64c558fbdff0aa6f

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_138f25a1c035e491" hs_bindgen_138f25a1c035e491 ::
     IO (Ptr.FunPtr (B -> IO ()))

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
fooB :: Ptr.FunPtr (B -> IO ())
fooB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_138f25a1c035e491

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_get_fooC@
foreign import ccall unsafe "hs_bindgen_e44396c12491a301" hs_bindgen_e44396c12491a301 ::
     IO (Ptr.FunPtr (M.C -> IO ()))

{-# NOINLINE fooC #-}
{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
fooC :: Ptr.FunPtr (M.C -> IO ())
fooC =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e44396c12491a301

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_get_fooD@
foreign import ccall unsafe "hs_bindgen_822cfa2df2dabce9" hs_bindgen_822cfa2df2dabce9 ::
     IO (Ptr.FunPtr (M.D -> IO ()))

{-# NOINLINE fooD #-}
{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
fooD :: Ptr.FunPtr (M.D -> IO ())
fooD =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_822cfa2df2dabce9

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_get_fooE@
foreign import ccall unsafe "hs_bindgen_cb3b428d93762ce9" hs_bindgen_cb3b428d93762ce9 ::
     IO (Ptr.FunPtr (E -> IO ()))

{-# NOINLINE fooE #-}
{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
fooE :: Ptr.FunPtr (E -> IO ())
fooE =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cb3b428d93762ce9
