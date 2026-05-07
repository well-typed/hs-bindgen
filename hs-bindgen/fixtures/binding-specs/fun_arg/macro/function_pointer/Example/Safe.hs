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
  [ "#include <binding-specs/fun_arg/macro/function_pointer.h>"
  , "void hs_bindgen_40e15e86e5db36ce ("
  , "  MyFunctionPointer arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_e13a57fd1d27f6e6 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  (fooA)(arg1);"
  , "}"
  , "void hs_bindgen_fd490df5087893ae ("
  , "  B arg1"
  , ")"
  , "{"
  , "  (fooB)(arg1);"
  , "}"
  , "void hs_bindgen_c552a862db9ba6eb ("
  , "  C arg1"
  , ")"
  , "{"
  , "  (fooC)(arg1);"
  , "}"
  , "void hs_bindgen_b08ba864cf83a382 ("
  , "  D arg1"
  , ")"
  , "{"
  , "  (fooD)(arg1);"
  , "}"
  , "void hs_bindgen_ca717b3afd1e43f5 ("
  , "  E arg1"
  , ")"
  , "{"
  , "  (fooE)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_40e15e86e5db36ce" hs_bindgen_40e15e86e5db36ce_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_foo@
hs_bindgen_40e15e86e5db36ce ::
     MyFunctionPointer
  -> IO ()
hs_bindgen_40e15e86e5db36ce =
  RIP.fromFFIType hs_bindgen_40e15e86e5db36ce_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 6:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
foo ::
     MyFunctionPointer
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_40e15e86e5db36ce

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_e13a57fd1d27f6e6" hs_bindgen_e13a57fd1d27f6e6_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooA@
hs_bindgen_e13a57fd1d27f6e6 ::
     A
  -> IO ()
hs_bindgen_e13a57fd1d27f6e6 =
  RIP.fromFFIType hs_bindgen_e13a57fd1d27f6e6_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 12:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_e13a57fd1d27f6e6

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_fd490df5087893ae" hs_bindgen_fd490df5087893ae_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooB@
hs_bindgen_fd490df5087893ae ::
     B
  -> IO ()
hs_bindgen_fd490df5087893ae =
  RIP.fromFFIType hs_bindgen_fd490df5087893ae_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 13:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_fd490df5087893ae

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_c552a862db9ba6eb" hs_bindgen_c552a862db9ba6eb_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooC@
hs_bindgen_c552a862db9ba6eb ::
     M.C
  -> IO ()
hs_bindgen_c552a862db9ba6eb =
  RIP.fromFFIType hs_bindgen_c552a862db9ba6eb_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooC ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_c552a862db9ba6eb

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_b08ba864cf83a382" hs_bindgen_b08ba864cf83a382_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooD@
hs_bindgen_b08ba864cf83a382 ::
     M.D
  -> IO ()
hs_bindgen_b08ba864cf83a382 =
  RIP.fromFFIType hs_bindgen_b08ba864cf83a382_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooD ::
     M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_b08ba864cf83a382

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_ca717b3afd1e43f5" hs_bindgen_ca717b3afd1e43f5_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooE@
hs_bindgen_ca717b3afd1e43f5 ::
     E
  -> IO ()
hs_bindgen_ca717b3afd1e43f5 =
  RIP.fromFFIType hs_bindgen_ca717b3afd1e43f5_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooE ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_ca717b3afd1e43f5
