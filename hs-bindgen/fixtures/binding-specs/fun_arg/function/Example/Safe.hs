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
  [ "#include <binding-specs/fun_arg/function.h>"
  , "void hs_bindgen_2470a43481a1ae0c ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_82b607ea7fbbce50 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_43dd2c0c605412fc ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  , "void hs_bindgen_8891f659c6cc2bb1 ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  fooC(arg1);"
  , "}"
  , "void hs_bindgen_fee955032d875e99 ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  fooD(arg1);"
  , "}"
  , "void hs_bindgen_16d6d6df95c1f054 ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  fooE(arg1);"
  , "}"
  , "void hs_bindgen_033bdfbf0dd68eb8 ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  bar(arg1);"
  , "}"
  , "void hs_bindgen_13ab619c380f478c ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  barA(arg1);"
  , "}"
  , "void hs_bindgen_97e945f9c7a17788 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  barB(arg1);"
  , "}"
  , "void hs_bindgen_60958a9bc8154f3b ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  barC(arg1);"
  , "}"
  , "void hs_bindgen_3726b64ffdeed5b5 ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  barD(arg1);"
  , "}"
  , "void hs_bindgen_8e4c333c820a545f ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  barE(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_2470a43481a1ae0c" hs_bindgen_2470a43481a1ae0c ::
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
foo = hs_bindgen_2470a43481a1ae0c

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_82b607ea7fbbce50" hs_bindgen_82b607ea7fbbce50 ::
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
fooA = hs_bindgen_82b607ea7fbbce50

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_43dd2c0c605412fc" hs_bindgen_43dd2c0c605412fc ::
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
fooB = hs_bindgen_43dd2c0c605412fc

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_8891f659c6cc2bb1" hs_bindgen_8891f659c6cc2bb1 ::
     Ptr.Ptr M.C
  -> IO ()

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/function.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
fooC ::
     Ptr.Ptr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_8891f659c6cc2bb1

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_fee955032d875e99" hs_bindgen_fee955032d875e99 ::
     Ptr.Ptr M.D
  -> IO ()

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/function.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
fooD ::
     Ptr.Ptr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_fee955032d875e99

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_16d6d6df95c1f054" hs_bindgen_16d6d6df95c1f054 ::
     Ptr.Ptr E
  -> IO ()

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/function.h 24:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
fooE ::
     Ptr.Ptr E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_16d6d6df95c1f054

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_033bdfbf0dd68eb8" hs_bindgen_033bdfbf0dd68eb8 ::
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
bar = hs_bindgen_033bdfbf0dd68eb8

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_barA@
foreign import ccall safe "hs_bindgen_13ab619c380f478c" hs_bindgen_13ab619c380f478c ::
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
barA = hs_bindgen_13ab619c380f478c

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_barB@
foreign import ccall safe "hs_bindgen_97e945f9c7a17788" hs_bindgen_97e945f9c7a17788 ::
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
barB = hs_bindgen_97e945f9c7a17788

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_barC@
foreign import ccall safe "hs_bindgen_60958a9bc8154f3b" hs_bindgen_60958a9bc8154f3b ::
     Ptr.Ptr M.C
  -> IO ()

{-| __C declaration:__ @barC@

    __defined at:__ @binding-specs\/fun_arg\/function.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
barC ::
     Ptr.Ptr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
barC = hs_bindgen_60958a9bc8154f3b

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_barD@
foreign import ccall safe "hs_bindgen_3726b64ffdeed5b5" hs_bindgen_3726b64ffdeed5b5 ::
     Ptr.Ptr M.D
  -> IO ()

{-| __C declaration:__ @barD@

    __defined at:__ @binding-specs\/fun_arg\/function.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
barD ::
     Ptr.Ptr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
barD = hs_bindgen_3726b64ffdeed5b5

-- __unique:__ @test_bindingspecsfun_argfunction_Example_Safe_barE@
foreign import ccall safe "hs_bindgen_8e4c333c820a545f" hs_bindgen_8e4c333c820a545f ::
     Ptr.Ptr E
  -> IO ()

{-| __C declaration:__ @barE@

    __defined at:__ @binding-specs\/fun_arg\/function.h 36:6@

    __exported by:__ @binding-specs\/fun_arg\/function.h@
-}
barE ::
     Ptr.Ptr E
     -- ^ __C declaration:__ @x@
  -> IO ()
barE = hs_bindgen_8e4c333c820a545f
