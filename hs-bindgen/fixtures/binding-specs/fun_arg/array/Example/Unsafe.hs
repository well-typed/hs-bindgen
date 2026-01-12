{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified M
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/array.h>"
  , "void hs_bindgen_0b4968dd850ae79c ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_07cc1cbdff19cd0c ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_529c1c7e31072654 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  , "void hs_bindgen_4f374937a4e7e40d ("
  , "  C arg1"
  , ")"
  , "{"
  , "  fooC(arg1);"
  , "}"
  , "void hs_bindgen_cade5cfffbebe4f2 ("
  , "  D arg1"
  , ")"
  , "{"
  , "  fooD(arg1);"
  , "}"
  , "void hs_bindgen_5733fab9a673cdd5 ("
  , "  E arg1"
  , ")"
  , "{"
  , "  fooE(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argarray_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_0b4968dd850ae79c" hs_bindgen_0b4968dd850ae79c ::
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
foo = hs_bindgen_0b4968dd850ae79c

-- __unique:__ @test_bindingspecsfun_argarray_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_07cc1cbdff19cd0c" hs_bindgen_07cc1cbdff19cd0c ::
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
fooA = hs_bindgen_07cc1cbdff19cd0c

-- __unique:__ @test_bindingspecsfun_argarray_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_529c1c7e31072654" hs_bindgen_529c1c7e31072654 ::
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
fooB = hs_bindgen_529c1c7e31072654

-- __unique:__ @test_bindingspecsfun_argarray_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_4f374937a4e7e40d" hs_bindgen_4f374937a4e7e40d ::
     M.C
  -> IO ()

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/array.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
fooC ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_4f374937a4e7e40d

-- __unique:__ @test_bindingspecsfun_argarray_Example_Unsafe_fooD@
foreign import ccall unsafe "hs_bindgen_cade5cfffbebe4f2" hs_bindgen_cade5cfffbebe4f2 ::
     M.D
  -> IO ()

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/array.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
fooD ::
     M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_cade5cfffbebe4f2

-- __unique:__ @test_bindingspecsfun_argarray_Example_Unsafe_fooE@
foreign import ccall unsafe "hs_bindgen_5733fab9a673cdd5" hs_bindgen_5733fab9a673cdd5 ::
     E
  -> IO ()

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/array.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
fooE ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_5733fab9a673cdd5
