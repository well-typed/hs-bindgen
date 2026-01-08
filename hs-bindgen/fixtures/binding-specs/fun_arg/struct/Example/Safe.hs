{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified M1
import qualified M2
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/struct.h>"
  , "void hs_bindgen_164f74ca36f10edc ("
  , "  struct S *arg1"
  , ")"
  , "{"
  , "  foo(*arg1);"
  , "}"
  , "void hs_bindgen_96ba6c9a11edd4bf ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  bar(*arg1);"
  , "}"
  , "void hs_bindgen_5d08101ec0998a69 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  baz(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argstruct_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_164f74ca36f10edc" hs_bindgen_164f74ca36f10edc ::
     Ptr.Ptr S
  -> IO ()

{-| Pointer-based API for 'foo'
-}
foo_wrapper ::
     Ptr.Ptr S
     -- ^ __C declaration:__ @x@
  -> IO ()
foo_wrapper = hs_bindgen_164f74ca36f10edc

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
    F.with x0 (\y1 -> hs_bindgen_164f74ca36f10edc y1)

-- __unique:__ @test_bindingspecsfun_argstruct_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_96ba6c9a11edd4bf" hs_bindgen_96ba6c9a11edd4bf ::
     Ptr.Ptr M1.A
  -> IO ()

{-| Pointer-based API for 'bar'
-}
bar_wrapper ::
     Ptr.Ptr M1.A
     -- ^ __C declaration:__ @x@
  -> IO ()
bar_wrapper = hs_bindgen_96ba6c9a11edd4bf

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/struct.h:5:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
bar ::
     M1.A
     -- ^ __C declaration:__ @x@
  -> IO ()
bar =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_96ba6c9a11edd4bf y1)

-- __unique:__ @test_bindingspecsfun_argstruct_Example_Safe_baz@
foreign import ccall safe "hs_bindgen_5d08101ec0998a69" hs_bindgen_5d08101ec0998a69 ::
     Ptr.Ptr M2.B
  -> IO ()

{-| Pointer-based API for 'baz'
-}
baz_wrapper ::
     Ptr.Ptr M2.B
     -- ^ __C declaration:__ @x@
  -> IO ()
baz_wrapper = hs_bindgen_5d08101ec0998a69

{-| __C declaration:__ @baz@

    __defined at:__ @binding-specs\/fun_arg\/struct.h:6:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
baz ::
     M2.B
     -- ^ __C declaration:__ @x@
  -> IO ()
baz =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_5d08101ec0998a69 y1)
