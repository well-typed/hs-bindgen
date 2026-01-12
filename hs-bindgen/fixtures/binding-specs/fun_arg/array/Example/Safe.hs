{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/array.h>"
  , "void hs_bindgen_3d4f2b2a378bc14d ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_cedf4515df8901e3 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_857a8fdee3b3fe8d ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  , "void hs_bindgen_62c2ecd0d496faab ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooC(arg1);"
  , "}"
  , "void hs_bindgen_93c6d3de2d542247 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooD(arg1);"
  , "}"
  , "void hs_bindgen_9f32a06312cb74c2 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooE(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argarray_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_3d4f2b2a378bc14d" hs_bindgen_3d4f2b2a378bc14d ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/array.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
foo ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_3d4f2b2a378bc14d

-- __unique:__ @test_bindingspecsfun_argarray_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_cedf4515df8901e3" hs_bindgen_cedf4515df8901e3 ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/array.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
fooA ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_cedf4515df8901e3

-- __unique:__ @test_bindingspecsfun_argarray_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_857a8fdee3b3fe8d" hs_bindgen_857a8fdee3b3fe8d ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/array.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
fooB ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_857a8fdee3b3fe8d

-- __unique:__ @test_bindingspecsfun_argarray_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_62c2ecd0d496faab" hs_bindgen_62c2ecd0d496faab ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/array.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
fooC ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_62c2ecd0d496faab

-- __unique:__ @test_bindingspecsfun_argarray_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_93c6d3de2d542247" hs_bindgen_93c6d3de2d542247 ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/array.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
fooD ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_93c6d3de2d542247

-- __unique:__ @test_bindingspecsfun_argarray_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_9f32a06312cb74c2" hs_bindgen_9f32a06312cb74c2 ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/array.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
fooE ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_9f32a06312cb74c2
