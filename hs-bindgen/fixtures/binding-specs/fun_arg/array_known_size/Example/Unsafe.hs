{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/array_known_size.h>"
  , "void hs_bindgen_4551d5573cd01cc1 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_869b94838f34541d ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_0d3bfff3d9aceab7 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  , "void hs_bindgen_e1295831d35f6812 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooC(arg1);"
  , "}"
  , "void hs_bindgen_a14efdef6a3a481f ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooD(arg1);"
  , "}"
  , "void hs_bindgen_3fb481fda03d5ce0 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooE(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_4551d5573cd01cc1" hs_bindgen_4551d5573cd01cc1 ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
foo ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_4551d5573cd01cc1

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_869b94838f34541d" hs_bindgen_869b94838f34541d ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
fooA ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_869b94838f34541d

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_0d3bfff3d9aceab7" hs_bindgen_0d3bfff3d9aceab7 ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
fooB ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_0d3bfff3d9aceab7

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_e1295831d35f6812" hs_bindgen_e1295831d35f6812 ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
fooC ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_e1295831d35f6812

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_Unsafe_fooD@
foreign import ccall unsafe "hs_bindgen_a14efdef6a3a481f" hs_bindgen_a14efdef6a3a481f ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
fooD ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_a14efdef6a3a481f

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_Unsafe_fooE@
foreign import ccall unsafe "hs_bindgen_3fb481fda03d5ce0" hs_bindgen_3fb481fda03d5ce0 ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
fooE ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_3fb481fda03d5ce0
