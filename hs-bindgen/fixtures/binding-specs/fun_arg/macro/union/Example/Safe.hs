{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/union.h>"
  , "void hs_bindgen_5da9ad143faecbca ("
  , "  union MyUnion *arg1"
  , ")"
  , "{"
  , "  (foo)(*arg1);"
  , "}"
  , "void hs_bindgen_f70ba8b74da026b3 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (fooA)(*arg1);"
  , "}"
  , "void hs_bindgen_89e946b10b5189a6 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  (fooB)(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_5da9ad143faecbca" hs_bindgen_5da9ad143faecbca_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Safe_foo@
hs_bindgen_5da9ad143faecbca ::
     RIP.Ptr MyUnion
  -> IO ()
hs_bindgen_5da9ad143faecbca =
  RIP.fromFFIType hs_bindgen_5da9ad143faecbca_base

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
    RIP.with x0 (\x1 -> hs_bindgen_5da9ad143faecbca x1)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_f70ba8b74da026b3" hs_bindgen_f70ba8b74da026b3_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Safe_fooA@
hs_bindgen_f70ba8b74da026b3 ::
     RIP.Ptr A
  -> IO ()
hs_bindgen_f70ba8b74da026b3 =
  RIP.fromFFIType hs_bindgen_f70ba8b74da026b3_base

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
    RIP.with x0 (\x1 -> hs_bindgen_f70ba8b74da026b3 x1)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_89e946b10b5189a6" hs_bindgen_89e946b10b5189a6_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Safe_fooB@
hs_bindgen_89e946b10b5189a6 ::
     RIP.Ptr B
  -> IO ()
hs_bindgen_89e946b10b5189a6 =
  RIP.fromFFIType hs_bindgen_89e946b10b5189a6_base

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
    RIP.with x0 (\x1 -> hs_bindgen_89e946b10b5189a6 x1)
