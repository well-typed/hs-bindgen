{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo
    , Example.Unsafe.fooA
    , Example.Unsafe.fooB
    , Example.Unsafe.fooC
    , Example.Unsafe.fooD
    , Example.Unsafe.fooE
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
  , "void hs_bindgen_6984f6c3dda0005c ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  (fooC)(*arg1);"
  , "}"
  , "void hs_bindgen_5431738e4ccf9ce9 ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  (fooD)(*arg1);"
  , "}"
  , "void hs_bindgen_1032f2cabeb8faaa ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  (fooE)(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_f784f3292d76f05c" hs_bindgen_f784f3292d76f05c_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_foo@
hs_bindgen_f784f3292d76f05c ::
     BG.Ptr MyUnion
  -> IO ()
hs_bindgen_f784f3292d76f05c =
  BG.fromFFIType hs_bindgen_f784f3292d76f05c_base

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
    BG.with x0 (\x1 -> hs_bindgen_f784f3292d76f05c x1)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_317131bf91a541b2" hs_bindgen_317131bf91a541b2_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_fooA@
hs_bindgen_317131bf91a541b2 ::
     BG.Ptr A
  -> IO ()
hs_bindgen_317131bf91a541b2 =
  BG.fromFFIType hs_bindgen_317131bf91a541b2_base

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
    BG.with x0 (\x1 -> hs_bindgen_317131bf91a541b2 x1)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_d2ce062db2e5b039" hs_bindgen_d2ce062db2e5b039_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_fooB@
hs_bindgen_d2ce062db2e5b039 ::
     BG.Ptr B
  -> IO ()
hs_bindgen_d2ce062db2e5b039 =
  BG.fromFFIType hs_bindgen_d2ce062db2e5b039_base

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
    BG.with x0 (\x1 -> hs_bindgen_d2ce062db2e5b039 x1)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_6984f6c3dda0005c" hs_bindgen_6984f6c3dda0005c_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_fooC@
hs_bindgen_6984f6c3dda0005c ::
     BG.Ptr M.C
  -> IO ()
hs_bindgen_6984f6c3dda0005c =
  BG.fromFFIType hs_bindgen_6984f6c3dda0005c_base

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
    BG.with x0 (\x1 -> hs_bindgen_6984f6c3dda0005c x1)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_fooD@
foreign import ccall unsafe "hs_bindgen_5431738e4ccf9ce9" hs_bindgen_5431738e4ccf9ce9_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_fooD@
hs_bindgen_5431738e4ccf9ce9 ::
     BG.Ptr M.D
  -> IO ()
hs_bindgen_5431738e4ccf9ce9 =
  BG.fromFFIType hs_bindgen_5431738e4ccf9ce9_base

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
    BG.with x0 (\x1 -> hs_bindgen_5431738e4ccf9ce9 x1)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_fooE@
foreign import ccall unsafe "hs_bindgen_1032f2cabeb8faaa" hs_bindgen_1032f2cabeb8faaa_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_fooE@
hs_bindgen_1032f2cabeb8faaa ::
     BG.Ptr E
  -> IO ()
hs_bindgen_1032f2cabeb8faaa =
  BG.fromFFIType hs_bindgen_1032f2cabeb8faaa_base

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
    BG.with x0 (\x1 -> hs_bindgen_1032f2cabeb8faaa x1)
