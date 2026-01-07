{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified M1
import qualified M2
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/array_known_size.h>"
  , "void hs_bindgen_a17ea38966af88ee ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_deddac2fc3684c04 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  bar(arg1);"
  , "}"
  , "void hs_bindgen_045f19f2132ae701 ("
  , "  B arg1"
  , ")"
  , "{"
  , "  baz(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_a17ea38966af88ee" hs_bindgen_a17ea38966af88ee ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h:3:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
foo ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_a17ea38966af88ee

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_deddac2fc3684c04" hs_bindgen_deddac2fc3684c04 ::
     M1.A
  -> IO ()

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h:4:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
bar ::
     M1.A
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_deddac2fc3684c04

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_Safe_baz@
foreign import ccall safe "hs_bindgen_045f19f2132ae701" hs_bindgen_045f19f2132ae701 ::
     M2.B
  -> IO ()

{-| __C declaration:__ @baz@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h:5:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
baz ::
     M2.B
     -- ^ __C declaration:__ @x@
  -> IO ()
baz = hs_bindgen_045f19f2132ae701
