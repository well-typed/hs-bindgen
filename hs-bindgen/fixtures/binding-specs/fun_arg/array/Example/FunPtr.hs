{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import qualified M
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/array.h>"
  , "/* test_bindingspecsfun_argarray_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_85fa838e81394ea2 (void)) ("
  , "  signed int arg1[]"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argarray_Example_get_fooA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ad9cc23fd6154f30 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_bindingspecsfun_argarray_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_00696b0042838d91 (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  , "/* test_bindingspecsfun_argarray_Example_get_fooC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f342da94cfffd62f (void)) ("
  , "  C arg1"
  , ")"
  , "{"
  , "  return &fooC;"
  , "}"
  , "/* test_bindingspecsfun_argarray_Example_get_fooD */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ec3e864fc631189f (void)) ("
  , "  D arg1"
  , ")"
  , "{"
  , "  return &fooD;"
  , "}"
  , "/* test_bindingspecsfun_argarray_Example_get_fooE */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_97aa709ccef1d1be (void)) ("
  , "  E arg1"
  , ")"
  , "{"
  , "  return &fooE;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argarray_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_85fa838e81394ea2" hs_bindgen_85fa838e81394ea2 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO ()))

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/array.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
foo :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_85fa838e81394ea2

-- __unique:__ @test_bindingspecsfun_argarray_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_ad9cc23fd6154f30" hs_bindgen_ad9cc23fd6154f30 ::
     IO (Ptr.FunPtr (A -> IO ()))

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/array.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
fooA :: Ptr.FunPtr (A -> IO ())
fooA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ad9cc23fd6154f30

-- __unique:__ @test_bindingspecsfun_argarray_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_00696b0042838d91" hs_bindgen_00696b0042838d91 ::
     IO (Ptr.FunPtr (B -> IO ()))

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/array.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
fooB :: Ptr.FunPtr (B -> IO ())
fooB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_00696b0042838d91

-- __unique:__ @test_bindingspecsfun_argarray_Example_get_fooC@
foreign import ccall unsafe "hs_bindgen_f342da94cfffd62f" hs_bindgen_f342da94cfffd62f ::
     IO (Ptr.FunPtr (M.C -> IO ()))

{-# NOINLINE fooC #-}
{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/array.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
fooC :: Ptr.FunPtr (M.C -> IO ())
fooC =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f342da94cfffd62f

-- __unique:__ @test_bindingspecsfun_argarray_Example_get_fooD@
foreign import ccall unsafe "hs_bindgen_ec3e864fc631189f" hs_bindgen_ec3e864fc631189f ::
     IO (Ptr.FunPtr (M.D -> IO ()))

{-# NOINLINE fooD #-}
{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/array.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
fooD :: Ptr.FunPtr (M.D -> IO ())
fooD =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ec3e864fc631189f

-- __unique:__ @test_bindingspecsfun_argarray_Example_get_fooE@
foreign import ccall unsafe "hs_bindgen_97aa709ccef1d1be" hs_bindgen_97aa709ccef1d1be ::
     IO (Ptr.FunPtr (E -> IO ()))

{-# NOINLINE fooE #-}
{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/array.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
fooE :: Ptr.FunPtr (E -> IO ())
fooE =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_97aa709ccef1d1be
