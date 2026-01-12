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
  [ "#include <binding-specs/fun_arg/function_pointer.h>"
  , "void hs_bindgen_bdd2915ebf9193f3 ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_f6fc7b0f46727f71 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_1666c20b0adf43e8 ("
  , "  B arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  , "void hs_bindgen_eb059a00d66d3f2c ("
  , "  C arg1"
  , ")"
  , "{"
  , "  fooC(arg1);"
  , "}"
  , "void hs_bindgen_e4db7213fe4a0384 ("
  , "  D arg1"
  , ")"
  , "{"
  , "  fooD(arg1);"
  , "}"
  , "void hs_bindgen_08d10843d2358c70 ("
  , "  E arg1"
  , ")"
  , "{"
  , "  fooE(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_bdd2915ebf9193f3" hs_bindgen_bdd2915ebf9193f3 ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  -> IO ()

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
foo ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_bdd2915ebf9193f3

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_f6fc7b0f46727f71" hs_bindgen_f6fc7b0f46727f71 ::
     A
  -> IO ()

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_f6fc7b0f46727f71

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_1666c20b0adf43e8" hs_bindgen_1666c20b0adf43e8 ::
     B
  -> IO ()

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_1666c20b0adf43e8

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_eb059a00d66d3f2c" hs_bindgen_eb059a00d66d3f2c ::
     M.C
  -> IO ()

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
fooC ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_eb059a00d66d3f2c

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_fooD@
foreign import ccall unsafe "hs_bindgen_e4db7213fe4a0384" hs_bindgen_e4db7213fe4a0384 ::
     M.D
  -> IO ()

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
fooD ::
     M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_e4db7213fe4a0384

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Unsafe_fooE@
foreign import ccall unsafe "hs_bindgen_08d10843d2358c70" hs_bindgen_08d10843d2358c70 ::
     E
  -> IO ()

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
fooE ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_08d10843d2358c70
