{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/union.h>"
  , "void hs_bindgen_f784f3292d76f05c ("
  , "  union MyUnion *arg1"
  , ")"
  , "{"
  , "  (foo)(*arg1);"
  , "}"
  , "void hs_bindgen_317131bf91a541b2 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (fooA)(*arg1);"
  , "}"
  , "void hs_bindgen_d2ce062db2e5b039 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  (fooB)(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_f784f3292d76f05c" hs_bindgen_f784f3292d76f05c_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_foo@
hs_bindgen_f784f3292d76f05c ::
     RIP.Ptr MyUnion
  -> IO ()
hs_bindgen_f784f3292d76f05c =
  RIP.fromFFIType hs_bindgen_f784f3292d76f05c_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
foo ::
     MyUnion
     -- ^ __C declaration:__ @x@
  -> IO ()
foo =
  \x0 ->
    RIP.with x0 (\x1 -> hs_bindgen_f784f3292d76f05c x1)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_317131bf91a541b2" hs_bindgen_317131bf91a541b2_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_fooA@
hs_bindgen_317131bf91a541b2 ::
     RIP.Ptr A
  -> IO ()
hs_bindgen_317131bf91a541b2 =
  RIP.fromFFIType hs_bindgen_317131bf91a541b2_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA =
  \x0 ->
    RIP.with x0 (\x1 -> hs_bindgen_317131bf91a541b2 x1)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_d2ce062db2e5b039" hs_bindgen_d2ce062db2e5b039_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_fooB@
hs_bindgen_d2ce062db2e5b039 ::
     RIP.Ptr B
  -> IO ()
hs_bindgen_d2ce062db2e5b039 =
  RIP.fromFFIType hs_bindgen_d2ce062db2e5b039_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB =
  \x0 ->
    RIP.with x0 (\x1 -> hs_bindgen_d2ce062db2e5b039 x1)
