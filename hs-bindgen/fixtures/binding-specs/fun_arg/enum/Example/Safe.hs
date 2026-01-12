{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Prelude
import qualified M
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/enum.h>"
  , "void hs_bindgen_dc70ea2c3e0f59f7 ("
  , "  enum MyEnum arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_b2cb354892eb7f37 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_247fcd1e9502076f ("
  , "  B arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  , "void hs_bindgen_700400324966c13f ("
  , "  C arg1"
  , ")"
  , "{"
  , "  fooC(arg1);"
  , "}"
  , "void hs_bindgen_d350cebf8bce06e0 ("
  , "  D arg1"
  , ")"
  , "{"
  , "  fooD(arg1);"
  , "}"
  , "void hs_bindgen_5a900ea11300707d ("
  , "  E arg1"
  , ")"
  , "{"
  , "  fooE(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argenum_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_dc70ea2c3e0f59f7" hs_bindgen_dc70ea2c3e0f59f7 ::
     MyEnum
  -> IO ()

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/enum.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
foo ::
     MyEnum
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_dc70ea2c3e0f59f7

-- __unique:__ @test_bindingspecsfun_argenum_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_b2cb354892eb7f37" hs_bindgen_b2cb354892eb7f37 ::
     A
  -> IO ()

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/enum.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_b2cb354892eb7f37

-- __unique:__ @test_bindingspecsfun_argenum_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_247fcd1e9502076f" hs_bindgen_247fcd1e9502076f ::
     B
  -> IO ()

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/enum.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_247fcd1e9502076f

-- __unique:__ @test_bindingspecsfun_argenum_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_700400324966c13f" hs_bindgen_700400324966c13f ::
     M.C
  -> IO ()

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/enum.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
fooC ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_700400324966c13f

-- __unique:__ @test_bindingspecsfun_argenum_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_d350cebf8bce06e0" hs_bindgen_d350cebf8bce06e0 ::
     M.D
  -> IO ()

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/enum.h 24:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
fooD ::
     M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_d350cebf8bce06e0

-- __unique:__ @test_bindingspecsfun_argenum_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_5a900ea11300707d" hs_bindgen_5a900ea11300707d ::
     E
  -> IO ()

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/enum.h 25:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
fooE ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_5a900ea11300707d
