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
  [ "#include <binding-specs/fun_arg/union.h>"
  , "/* test_bindingspecsfun_argunion_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2ae32a336baf7eeb (void)) ("
  , "  union MyUnion arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argunion_Example_get_fooA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9b6d938881abeb97 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_bindingspecsfun_argunion_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d327728e41cdb1ae (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  , "/* test_bindingspecsfun_argunion_Example_get_fooC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fe9f976621c17dd1 (void)) ("
  , "  C arg1"
  , ")"
  , "{"
  , "  return &fooC;"
  , "}"
  , "/* test_bindingspecsfun_argunion_Example_get_fooD */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c9718a0803f9fcdb (void)) ("
  , "  D arg1"
  , ")"
  , "{"
  , "  return &fooD;"
  , "}"
  , "/* test_bindingspecsfun_argunion_Example_get_fooE */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_668fdaddd6fadfbf (void)) ("
  , "  E arg1"
  , ")"
  , "{"
  , "  return &fooE;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argunion_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_2ae32a336baf7eeb" hs_bindgen_2ae32a336baf7eeb ::
     IO (Ptr.FunPtr (MyUnion -> IO ()))

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/union.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
foo :: Ptr.FunPtr (MyUnion -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2ae32a336baf7eeb

-- __unique:__ @test_bindingspecsfun_argunion_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_9b6d938881abeb97" hs_bindgen_9b6d938881abeb97 ::
     IO (Ptr.FunPtr (A -> IO ()))

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/union.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
fooA :: Ptr.FunPtr (A -> IO ())
fooA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9b6d938881abeb97

-- __unique:__ @test_bindingspecsfun_argunion_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_d327728e41cdb1ae" hs_bindgen_d327728e41cdb1ae ::
     IO (Ptr.FunPtr (B -> IO ()))

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/union.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
fooB :: Ptr.FunPtr (B -> IO ())
fooB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d327728e41cdb1ae

-- __unique:__ @test_bindingspecsfun_argunion_Example_get_fooC@
foreign import ccall unsafe "hs_bindgen_fe9f976621c17dd1" hs_bindgen_fe9f976621c17dd1 ::
     IO (Ptr.FunPtr (M.C -> IO ()))

{-# NOINLINE fooC #-}
{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/union.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
fooC :: Ptr.FunPtr (M.C -> IO ())
fooC =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fe9f976621c17dd1

-- __unique:__ @test_bindingspecsfun_argunion_Example_get_fooD@
foreign import ccall unsafe "hs_bindgen_c9718a0803f9fcdb" hs_bindgen_c9718a0803f9fcdb ::
     IO (Ptr.FunPtr (M.D -> IO ()))

{-# NOINLINE fooD #-}
{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/union.h 24:6@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
fooD :: Ptr.FunPtr (M.D -> IO ())
fooD =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c9718a0803f9fcdb

-- __unique:__ @test_bindingspecsfun_argunion_Example_get_fooE@
foreign import ccall unsafe "hs_bindgen_668fdaddd6fadfbf" hs_bindgen_668fdaddd6fadfbf ::
     IO (Ptr.FunPtr (E -> IO ()))

{-# NOINLINE fooE #-}
{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/union.h 25:6@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
fooE :: Ptr.FunPtr (E -> IO ())
fooE =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_668fdaddd6fadfbf
