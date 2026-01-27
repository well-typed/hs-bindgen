{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/macro/union.h>"
  , "void hs_bindgen_f784f3292d76f05c ("
  , "  union MyUnion *arg1"
  , ")"
  , "{"
  , "  foo(*arg1);"
  , "}"
  , "void hs_bindgen_317131bf91a541b2 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  fooA(*arg1);"
  , "}"
  , "void hs_bindgen_d2ce062db2e5b039 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  fooB(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_f784f3292d76f05c" hs_bindgen_f784f3292d76f05c_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_foo@
hs_bindgen_f784f3292d76f05c ::
     Ptr.Ptr MyUnion
  -> IO ()
hs_bindgen_f784f3292d76f05c =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_f784f3292d76f05c_base

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
    F.with x0 (\x1 -> hs_bindgen_f784f3292d76f05c x1)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_317131bf91a541b2" hs_bindgen_317131bf91a541b2_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_fooA@
hs_bindgen_317131bf91a541b2 ::
     Ptr.Ptr A
  -> IO ()
hs_bindgen_317131bf91a541b2 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_317131bf91a541b2_base

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
    F.with x0 (\x1 -> hs_bindgen_317131bf91a541b2 x1)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_d2ce062db2e5b039" hs_bindgen_d2ce062db2e5b039_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_Unsafe_fooB@
hs_bindgen_d2ce062db2e5b039 ::
     Ptr.Ptr B
  -> IO ()
hs_bindgen_d2ce062db2e5b039 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d2ce062db2e5b039_base

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
    F.with x0 (\x1 -> hs_bindgen_d2ce062db2e5b039 x1)
