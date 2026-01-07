{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Prelude
import qualified M1
import qualified M2
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/enum.h>"
  , "void hs_bindgen_7a447ec2e0ff6918 ("
  , "  enum E arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_c771c0041909477f ("
  , "  A arg1"
  , ")"
  , "{"
  , "  bar(arg1);"
  , "}"
  , "void hs_bindgen_88fe2033f9ef492d ("
  , "  B arg1"
  , ")"
  , "{"
  , "  baz(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argenum_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_7a447ec2e0ff6918" hs_bindgen_7a447ec2e0ff6918 ::
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
foo = hs_bindgen_7a447ec2e0ff6918

-- __unique:__ @test_bindingspecsfun_argenum_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_c771c0041909477f" hs_bindgen_c771c0041909477f ::
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
bar = hs_bindgen_c771c0041909477f

-- __unique:__ @test_bindingspecsfun_argenum_Example_Unsafe_baz@
foreign import ccall unsafe "hs_bindgen_88fe2033f9ef492d" hs_bindgen_88fe2033f9ef492d ::
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
baz = hs_bindgen_88fe2033f9ef492d
