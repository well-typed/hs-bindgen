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
  , "void hs_bindgen_5a35d0277bd58a8a ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  bar(arg1);"
  , "}"
  , "void hs_bindgen_00363fb8a2e5b7c8 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  baz(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_4551d5573cd01cc1" hs_bindgen_4551d5573cd01cc1 ::
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
foo = hs_bindgen_4551d5573cd01cc1

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_5a35d0277bd58a8a" hs_bindgen_5a35d0277bd58a8a ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h:4:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
bar ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_5a35d0277bd58a8a

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_Unsafe_baz@
foreign import ccall unsafe "hs_bindgen_00363fb8a2e5b7c8" hs_bindgen_00363fb8a2e5b7c8 ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @baz@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h:5:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
baz ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
baz = hs_bindgen_00363fb8a2e5b7c8
