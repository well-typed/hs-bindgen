{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
  ]))

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_d49a011eb7da5969" hs_bindgen_d49a011eb7da5969_base ::
     RIP.Word32
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Safe_foo@
hs_bindgen_d49a011eb7da5969 ::
     MyEnum
  -> IO ()
hs_bindgen_d49a011eb7da5969 =
  RIP.fromFFIType hs_bindgen_d49a011eb7da5969_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
foo ::
     MyEnum
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_d49a011eb7da5969

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_32c6f6f9bb440690" hs_bindgen_32c6f6f9bb440690_base ::
     RIP.Word32
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Safe_fooA@
hs_bindgen_32c6f6f9bb440690 ::
     A
  -> IO ()
hs_bindgen_32c6f6f9bb440690 =
  RIP.fromFFIType hs_bindgen_32c6f6f9bb440690_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_32c6f6f9bb440690

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_16e7856908d06f71" hs_bindgen_16e7856908d06f71_base ::
     RIP.Word32
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Safe_fooB@
hs_bindgen_16e7856908d06f71 ::
     B
  -> IO ()
hs_bindgen_16e7856908d06f71 =
  RIP.fromFFIType hs_bindgen_16e7856908d06f71_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_16e7856908d06f71
