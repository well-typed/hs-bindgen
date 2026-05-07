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

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified M
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/enum.h>"
  , "void hs_bindgen_0e6b98e93cad73ef ("
  , "  enum MyEnum arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_1c6de1b89014dc52 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  (fooA)(arg1);"
  , "}"
  , "void hs_bindgen_a8e579f3b5035c03 ("
  , "  B arg1"
  , ")"
  , "{"
  , "  (fooB)(arg1);"
  , "}"
  , "void hs_bindgen_00ad8122da6609fc ("
  , "  C arg1"
  , ")"
  , "{"
  , "  (fooC)(arg1);"
  , "}"
  , "void hs_bindgen_2be41098f80a9019 ("
  , "  D arg1"
  , ")"
  , "{"
  , "  (fooD)(arg1);"
  , "}"
  , "void hs_bindgen_07241b4b6cbf2991 ("
  , "  E arg1"
  , ")"
  , "{"
  , "  (fooE)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_0e6b98e93cad73ef" hs_bindgen_0e6b98e93cad73ef_base ::
     RIP.Word32
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Unsafe_foo@
hs_bindgen_0e6b98e93cad73ef ::
     MyEnum
  -> IO ()
hs_bindgen_0e6b98e93cad73ef =
  RIP.fromFFIType hs_bindgen_0e6b98e93cad73ef_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 6:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
foo ::
     MyEnum
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_0e6b98e93cad73ef

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_1c6de1b89014dc52" hs_bindgen_1c6de1b89014dc52_base ::
     RIP.Word32
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Unsafe_fooA@
hs_bindgen_1c6de1b89014dc52 ::
     A
  -> IO ()
hs_bindgen_1c6de1b89014dc52 =
  RIP.fromFFIType hs_bindgen_1c6de1b89014dc52_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 12:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_1c6de1b89014dc52

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_a8e579f3b5035c03" hs_bindgen_a8e579f3b5035c03_base ::
     RIP.Word32
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Unsafe_fooB@
hs_bindgen_a8e579f3b5035c03 ::
     B
  -> IO ()
hs_bindgen_a8e579f3b5035c03 =
  RIP.fromFFIType hs_bindgen_a8e579f3b5035c03_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 13:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_a8e579f3b5035c03

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_00ad8122da6609fc" hs_bindgen_00ad8122da6609fc_base ::
     RIP.Word32
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Unsafe_fooC@
hs_bindgen_00ad8122da6609fc ::
     M.C
  -> IO ()
hs_bindgen_00ad8122da6609fc =
  RIP.fromFFIType hs_bindgen_00ad8122da6609fc_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooC ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_00ad8122da6609fc

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Unsafe_fooD@
foreign import ccall unsafe "hs_bindgen_2be41098f80a9019" hs_bindgen_2be41098f80a9019_base ::
     RIP.Word32
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Unsafe_fooD@
hs_bindgen_2be41098f80a9019 ::
     M.D
  -> IO ()
hs_bindgen_2be41098f80a9019 =
  RIP.fromFFIType hs_bindgen_2be41098f80a9019_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooD ::
     M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_2be41098f80a9019

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Unsafe_fooE@
foreign import ccall unsafe "hs_bindgen_07241b4b6cbf2991" hs_bindgen_07241b4b6cbf2991_base ::
     RIP.Word32
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Unsafe_fooE@
hs_bindgen_07241b4b6cbf2991 ::
     E
  -> IO ()
hs_bindgen_07241b4b6cbf2991 =
  RIP.fromFFIType hs_bindgen_07241b4b6cbf2991_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooE ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_07241b4b6cbf2991
