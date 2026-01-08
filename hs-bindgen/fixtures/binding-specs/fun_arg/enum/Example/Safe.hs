{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Prelude
import qualified M1
import qualified M2
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/enum.h>"
  , "void hs_bindgen_dc70ea2c3e0f59f7 ("
  , "  enum E arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_70cdbbc732919eae ("
  , "  A arg1"
  , ")"
  , "{"
  , "  bar(arg1);"
  , "}"
  , "void hs_bindgen_898db6718e124dae ("
  , "  B arg1"
  , ")"
  , "{"
  , "  baz(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argenum_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_dc70ea2c3e0f59f7" hs_bindgen_dc70ea2c3e0f59f7 ::
     E
  -> IO ()

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/enum.h:4:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
foo ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_dc70ea2c3e0f59f7

-- __unique:__ @test_bindingspecsfun_argenum_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_70cdbbc732919eae" hs_bindgen_70cdbbc732919eae ::
     M1.A
  -> IO ()

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/enum.h:5:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
bar ::
     M1.A
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_70cdbbc732919eae

-- __unique:__ @test_bindingspecsfun_argenum_Example_Safe_baz@
foreign import ccall safe "hs_bindgen_898db6718e124dae" hs_bindgen_898db6718e124dae ::
     M2.B
  -> IO ()

{-| __C declaration:__ @baz@

    __defined at:__ @binding-specs\/fun_arg\/enum.h:6:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
baz ::
     M2.B
     -- ^ __C declaration:__ @x@
  -> IO ()
baz = hs_bindgen_898db6718e124dae
