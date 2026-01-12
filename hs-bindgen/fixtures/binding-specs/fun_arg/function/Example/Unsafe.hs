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
  [ "#include <binding-specs/fun_arg/function.h>"
  , "void hs_bindgen_bdd2915ebf9193f3 ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_f6fc7b0f46727f71 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_1666c20b0adf43e8 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  , "void hs_bindgen_eb059a00d66d3f2c ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  fooC(arg1);"
  , "}"
  , "void hs_bindgen_e4db7213fe4a0384 ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  fooD(arg1);"
  , "}"
  , "void hs_bindgen_08d10843d2358c70 ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  fooE(arg1);"
  , "}"
  , "void hs_bindgen_9f3ef1a2ad90e46e ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  bar(arg1);"
  , "}"
  , "void hs_bindgen_0cef65d4048fe005 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  barA(arg1);"
  , "}"
  , "void hs_bindgen_620e4006acfa001e ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  barB(arg1);"
  , "}"
  , "void hs_bindgen_1f329c0669b9536a ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  barC(arg1);"
  , "}"
  , "void hs_bindgen_1054ad1560b34a7d ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  barD(arg1);"
  , "}"
  , "void hs_bindgen_4a9c2d1ad62dd8eb ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  barE(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_bdd2915ebf9193f3" hs_bindgen_bdd2915ebf9193f3 ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  -> IO ()

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/function.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
foo ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_bdd2915ebf9193f3

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_f6fc7b0f46727f71" hs_bindgen_f6fc7b0f46727f71 ::
     Ptr.FunPtr A
  -> IO ()

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/function.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
fooA ::
     Ptr.FunPtr A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_f6fc7b0f46727f71

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_1666c20b0adf43e8" hs_bindgen_1666c20b0adf43e8 ::
     Ptr.FunPtr B
  -> IO ()

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/function.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
fooB ::
     Ptr.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_1666c20b0adf43e8

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_eb059a00d66d3f2c" hs_bindgen_eb059a00d66d3f2c ::
     Ptr.FunPtr M.C
  -> IO ()

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/function.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
fooC ::
     Ptr.FunPtr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_eb059a00d66d3f2c

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_fooD@
foreign import ccall unsafe "hs_bindgen_e4db7213fe4a0384" hs_bindgen_e4db7213fe4a0384 ::
     Ptr.FunPtr M.D
  -> IO ()

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/function.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
fooD ::
     Ptr.FunPtr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_e4db7213fe4a0384

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_fooE@
foreign import ccall unsafe "hs_bindgen_08d10843d2358c70" hs_bindgen_08d10843d2358c70 ::
     Ptr.FunPtr E
  -> IO ()

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/function.h 24:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
fooE ::
     Ptr.FunPtr E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_08d10843d2358c70

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_9f3ef1a2ad90e46e" hs_bindgen_9f3ef1a2ad90e46e ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  -> IO ()

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/function.h 29:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
bar ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_9f3ef1a2ad90e46e

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_barA@
foreign import ccall unsafe "hs_bindgen_0cef65d4048fe005" hs_bindgen_0cef65d4048fe005 ::
     Ptr.FunPtr A
  -> IO ()

{-| __C declaration:__ @barA@

    __defined at:__ @binding-specs\/fun_arg\/function.h 31:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
barA ::
     Ptr.FunPtr A
     -- ^ __C declaration:__ @x@
  -> IO ()
barA = hs_bindgen_0cef65d4048fe005

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_barB@
foreign import ccall unsafe "hs_bindgen_620e4006acfa001e" hs_bindgen_620e4006acfa001e ::
     Ptr.FunPtr B
  -> IO ()

{-| __C declaration:__ @barB@

    __defined at:__ @binding-specs\/fun_arg\/function.h 32:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
barB ::
     Ptr.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
barB = hs_bindgen_620e4006acfa001e

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_barC@
foreign import ccall unsafe "hs_bindgen_1f329c0669b9536a" hs_bindgen_1f329c0669b9536a ::
     Ptr.FunPtr M.C
  -> IO ()

{-| __C declaration:__ @barC@

    __defined at:__ @binding-specs\/fun_arg\/function.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
barC ::
     Ptr.FunPtr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
barC = hs_bindgen_1f329c0669b9536a

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_barD@
foreign import ccall unsafe "hs_bindgen_1054ad1560b34a7d" hs_bindgen_1054ad1560b34a7d ::
     Ptr.FunPtr M.D
  -> IO ()

{-| __C declaration:__ @barD@

    __defined at:__ @binding-specs\/fun_arg\/function.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
barD ::
     Ptr.FunPtr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
barD = hs_bindgen_1054ad1560b34a7d

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_barE@
foreign import ccall unsafe "hs_bindgen_4a9c2d1ad62dd8eb" hs_bindgen_4a9c2d1ad62dd8eb ::
     Ptr.FunPtr E
  -> IO ()

{-| __C declaration:__ @barE@

    __defined at:__ @binding-specs\/fun_arg\/function.h 36:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
barE ::
     Ptr.FunPtr E
     -- ^ __C declaration:__ @x@
  -> IO ()
barE = hs_bindgen_4a9c2d1ad62dd8eb
