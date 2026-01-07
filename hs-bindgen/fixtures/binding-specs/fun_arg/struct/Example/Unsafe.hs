{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified M1
import qualified M2
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/struct.h>"
  , "void hs_bindgen_b39843a9b8c0691a ("
  , "  struct S *arg1"
  , ")"
  , "{"
  , "  foo(*arg1);"
  , "}"
  , "void hs_bindgen_5eeddd0730c60b39 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  bar(arg1);"
  , "}"
  , "void hs_bindgen_7da87742468e14c2 ("
  , "  B arg1"
  , ")"
  , "{"
  , "  baz(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argstruct_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_b39843a9b8c0691a" hs_bindgen_b39843a9b8c0691a ::
     Ptr.Ptr S
  -> IO ()

{-| Pointer-based API for 'foo'
-}
foo_wrapper ::
     Ptr.Ptr S
     -- ^ __C declaration:__ @x@
  -> IO ()
foo_wrapper = hs_bindgen_b39843a9b8c0691a

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/struct.h:4:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
foo ::
     S
     -- ^ __C declaration:__ @x@
  -> IO ()
foo =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_b39843a9b8c0691a y1)

-- __unique:__ @test_bindingspecsfun_argstruct_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_5eeddd0730c60b39" hs_bindgen_5eeddd0730c60b39 ::
     M1.A
  -> IO ()

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/struct.h:5:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
bar ::
     M1.A
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_5eeddd0730c60b39

-- __unique:__ @test_bindingspecsfun_argstruct_Example_Unsafe_baz@
foreign import ccall unsafe "hs_bindgen_7da87742468e14c2" hs_bindgen_7da87742468e14c2 ::
     M2.B
  -> IO ()

{-| __C declaration:__ @baz@

    __defined at:__ @binding-specs\/fun_arg\/struct.h:6:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
baz ::
     M2.B
     -- ^ __C declaration:__ @x@
  -> IO ()
baz = hs_bindgen_7da87742468e14c2
