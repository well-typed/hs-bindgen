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
  [ "#include <binding-specs/fun_arg/array.h>"
  , "void hs_bindgen_0b4968dd850ae79c ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_940fcc94bfe556a6 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  bar(arg1);"
  , "}"
  , "void hs_bindgen_47293ea604abc73d ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  baz(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argarray_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_0b4968dd850ae79c" hs_bindgen_0b4968dd850ae79c ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/array.h:3:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
foo ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_0b4968dd850ae79c

-- __unique:__ @test_bindingspecsfun_argarray_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_940fcc94bfe556a6" hs_bindgen_940fcc94bfe556a6 ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/array.h:4:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
bar ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_940fcc94bfe556a6

-- __unique:__ @test_bindingspecsfun_argarray_Example_Unsafe_baz@
foreign import ccall unsafe "hs_bindgen_47293ea604abc73d" hs_bindgen_47293ea604abc73d ::
     Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @baz@

    __defined at:__ @binding-specs\/fun_arg\/array.h:5:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
baz ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
baz = hs_bindgen_47293ea604abc73d
