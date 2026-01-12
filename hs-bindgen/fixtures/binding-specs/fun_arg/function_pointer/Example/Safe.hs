{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified M
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/function_pointer.h>"
  , "void hs_bindgen_2470a43481a1ae0c ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_82b607ea7fbbce50 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_43dd2c0c605412fc ("
  , "  B arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  , "void hs_bindgen_8891f659c6cc2bb1 ("
  , "  C arg1"
  , ")"
  , "{"
  , "  fooC(arg1);"
  , "}"
  , "void hs_bindgen_fee955032d875e99 ("
  , "  D arg1"
  , ")"
  , "{"
  , "  fooD(arg1);"
  , "}"
  , "void hs_bindgen_16d6d6df95c1f054 ("
  , "  E arg1"
  , ")"
  , "{"
  , "  fooE(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_2470a43481a1ae0c" hs_bindgen_2470a43481a1ae0c ::
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
foo = hs_bindgen_2470a43481a1ae0c

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_82b607ea7fbbce50" hs_bindgen_82b607ea7fbbce50 ::
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
fooA = hs_bindgen_82b607ea7fbbce50

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_43dd2c0c605412fc" hs_bindgen_43dd2c0c605412fc ::
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
fooB = hs_bindgen_43dd2c0c605412fc

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_8891f659c6cc2bb1" hs_bindgen_8891f659c6cc2bb1 ::
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
fooC = hs_bindgen_8891f659c6cc2bb1

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_fee955032d875e99" hs_bindgen_fee955032d875e99 ::
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
fooD = hs_bindgen_fee955032d875e99

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_16d6d6df95c1f054" hs_bindgen_16d6d6df95c1f054 ::
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
fooE = hs_bindgen_16d6d6df95c1f054
