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
  [ "#include <binding-specs/fun_arg/function_pointer.h>"
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
  , "  A arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_bindingspecsfun_argfunction_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_568bf4fa5a80b6d4 (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  , "/* test_bindingspecsfun_argfunction_Example_get_fooC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_79ba57311c1537e9 (void)) ("
  , "  C arg1"
  , ")"
  , "{"
  , "  return &fooC;"
  , "}"
  , "/* test_bindingspecsfun_argfunction_Example_get_fooD */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e9befcc69cbf152b (void)) ("
  , "  D arg1"
  , ")"
  , "{"
  , "  return &fooD;"
  , "}"
  , "/* test_bindingspecsfun_argfunction_Example_get_fooE */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e1c0f75003a4e64d (void)) ("
  , "  E arg1"
  , ")"
  , "{"
  , "  return &fooE;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_8a0babf894a65b5f" hs_bindgen_8a0babf894a65b5f ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO ()))

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
foo :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8a0babf894a65b5f

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_a7d32556fb0befb2" hs_bindgen_a7d32556fb0befb2 ::
     IO (Ptr.FunPtr (A -> IO ()))

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
fooA :: Ptr.FunPtr (A -> IO ())
fooA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a7d32556fb0befb2

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_568bf4fa5a80b6d4" hs_bindgen_568bf4fa5a80b6d4 ::
     IO (Ptr.FunPtr (B -> IO ()))

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
fooB :: Ptr.FunPtr (B -> IO ())
fooB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_568bf4fa5a80b6d4

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_fooC@
foreign import ccall unsafe "hs_bindgen_79ba57311c1537e9" hs_bindgen_79ba57311c1537e9 ::
     IO (Ptr.FunPtr (M.C -> IO ()))

{-# NOINLINE fooC #-}
{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
fooC :: Ptr.FunPtr (M.C -> IO ())
fooC =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_79ba57311c1537e9

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_fooD@
foreign import ccall unsafe "hs_bindgen_e9befcc69cbf152b" hs_bindgen_e9befcc69cbf152b ::
     IO (Ptr.FunPtr (M.D -> IO ()))

{-# NOINLINE fooD #-}
{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
fooD :: Ptr.FunPtr (M.D -> IO ())
fooD =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e9befcc69cbf152b

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_fooE@
foreign import ccall unsafe "hs_bindgen_e1c0f75003a4e64d" hs_bindgen_e1c0f75003a4e64d ::
     IO (Ptr.FunPtr (E -> IO ()))

{-# NOINLINE fooE #-}
{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
fooE :: Ptr.FunPtr (E -> IO ())
fooE =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e1c0f75003a4e64d
