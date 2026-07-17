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

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/enum.h>"
  , "void hs_bindgen_d49a011eb7da5969 ("
  , "  enum MyEnum arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_32c6f6f9bb440690 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  (fooA)(arg1);"
  , "}"
  , "void hs_bindgen_16e7856908d06f71 ("
  , "  B arg1"
  , ")"
  , "{"
  , "  (fooB)(arg1);"
  , "}"
  , "void hs_bindgen_2ebd95eb71d88a64 ("
  , "  C arg1"
  , ")"
  , "{"
  , "  (fooC)(arg1);"
  , "}"
  , "void hs_bindgen_deddaf3108b305bb ("
  , "  D arg1"
  , ")"
  , "{"
  , "  (fooD)(arg1);"
  , "}"
  , "void hs_bindgen_9ed085d5f2e5837e ("
  , "  E arg1"
  , ")"
  , "{"
  , "  (fooE)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_d49a011eb7da5969" hs_bindgen_d49a011eb7da5969_base ::
     BG.Word32
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Safe_foo@
hs_bindgen_d49a011eb7da5969 ::
     MyEnum
  -> IO ()
hs_bindgen_d49a011eb7da5969 =
  BG.fromFFIType hs_bindgen_d49a011eb7da5969_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 6:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
foo ::
     MyEnum
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_d49a011eb7da5969

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_32c6f6f9bb440690" hs_bindgen_32c6f6f9bb440690_base ::
     BG.Word32
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Safe_fooA@
hs_bindgen_32c6f6f9bb440690 ::
     A
  -> IO ()
hs_bindgen_32c6f6f9bb440690 =
  BG.fromFFIType hs_bindgen_32c6f6f9bb440690_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 12:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_32c6f6f9bb440690

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_16e7856908d06f71" hs_bindgen_16e7856908d06f71_base ::
     BG.Word32
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Safe_fooB@
hs_bindgen_16e7856908d06f71 ::
     B
  -> IO ()
hs_bindgen_16e7856908d06f71 =
  BG.fromFFIType hs_bindgen_16e7856908d06f71_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 13:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_16e7856908d06f71

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_2ebd95eb71d88a64" hs_bindgen_2ebd95eb71d88a64_base ::
     BG.Word32
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Safe_fooC@
hs_bindgen_2ebd95eb71d88a64 ::
     M.C
  -> IO ()
hs_bindgen_2ebd95eb71d88a64 =
  BG.fromFFIType hs_bindgen_2ebd95eb71d88a64_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooC ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_2ebd95eb71d88a64

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_deddaf3108b305bb" hs_bindgen_deddaf3108b305bb_base ::
     BG.Word32
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Safe_fooD@
hs_bindgen_deddaf3108b305bb ::
     M.D
  -> IO ()
hs_bindgen_deddaf3108b305bb =
  BG.fromFFIType hs_bindgen_deddaf3108b305bb_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooD ::
     M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_deddaf3108b305bb

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_9ed085d5f2e5837e" hs_bindgen_9ed085d5f2e5837e_base ::
     BG.Word32
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Safe_fooE@
hs_bindgen_9ed085d5f2e5837e ::
     E
  -> IO ()
hs_bindgen_9ed085d5f2e5837e =
  BG.fromFFIType hs_bindgen_9ed085d5f2e5837e_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooE ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_9ed085d5f2e5837e
