{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    , Example.Safe.fooA
    , Example.Safe.fooB
    , Example.Safe.fooC
    , Example.Safe.fooD
    , Example.Safe.fooE
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified M
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
  , "void hs_bindgen_c82bf4a34bf547de ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  (fooC)(*arg1);"
  , "}"
  , "void hs_bindgen_25033ba8f1b4731d ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  (fooD)(*arg1);"
  , "}"
  , "void hs_bindgen_0cf5debdefedd88c ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  (fooE)(*arg1);"
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

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 6:6@

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

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 12:6@

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

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 13:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB =
  \x0 ->
    RIP.with x0 (\x1 -> hs_bindgen_89e946b10b5189a6 x1)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_c82bf4a34bf547de" hs_bindgen_c82bf4a34bf547de_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Safe_fooC@
hs_bindgen_c82bf4a34bf547de ::
     RIP.Ptr M.C
  -> IO ()
hs_bindgen_c82bf4a34bf547de =
  RIP.fromFFIType hs_bindgen_c82bf4a34bf547de_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
fooC ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC =
  \x0 ->
    RIP.with x0 (\x1 -> hs_bindgen_c82bf4a34bf547de x1)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_25033ba8f1b4731d" hs_bindgen_25033ba8f1b4731d_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Safe_fooD@
hs_bindgen_25033ba8f1b4731d ::
     RIP.Ptr M.D
  -> IO ()
hs_bindgen_25033ba8f1b4731d =
  RIP.fromFFIType hs_bindgen_25033ba8f1b4731d_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
fooD ::
     M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD =
  \x0 ->
    RIP.with x0 (\x1 -> hs_bindgen_25033ba8f1b4731d x1)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_0cf5debdefedd88c" hs_bindgen_0cf5debdefedd88c_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Safe_fooE@
hs_bindgen_0cf5debdefedd88c ::
     RIP.Ptr E
  -> IO ()
hs_bindgen_0cf5debdefedd88c =
  RIP.fromFFIType hs_bindgen_0cf5debdefedd88c_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
fooE ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE =
  \x0 ->
    RIP.with x0 (\x1 -> hs_bindgen_0cf5debdefedd88c x1)
