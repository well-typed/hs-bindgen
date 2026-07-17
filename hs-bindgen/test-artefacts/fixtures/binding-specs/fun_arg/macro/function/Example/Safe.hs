{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    , Example.Safe.fooA
    , Example.Safe.fooB
    , Example.Safe.fooC
    , Example.Safe.fooD
    , Example.Safe.fooE
    , Example.Safe.bar
    , Example.Safe.barA
    , Example.Safe.barB
    , Example.Safe.barC
    , Example.Safe.barD
    , Example.Safe.barE
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/function.h>"
  , "void hs_bindgen_40e15e86e5db36ce ("
  , "  MyFunction *arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_e13a57fd1d27f6e6 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (fooA)(arg1);"
  , "}"
  , "void hs_bindgen_fd490df5087893ae ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  (fooB)(arg1);"
  , "}"
  , "void hs_bindgen_c552a862db9ba6eb ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  (fooC)(arg1);"
  , "}"
  , "void hs_bindgen_b08ba864cf83a382 ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  (fooD)(arg1);"
  , "}"
  , "void hs_bindgen_ca717b3afd1e43f5 ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  (fooE)(arg1);"
  , "}"
  , "void hs_bindgen_a708e95f35bff290 ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  , "void hs_bindgen_2991dd76f4337b78 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (barA)(arg1);"
  , "}"
  , "void hs_bindgen_104de3f97206bd1d ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  (barB)(arg1);"
  , "}"
  , "void hs_bindgen_963bcd9e5467fd8f ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  (barC)(arg1);"
  , "}"
  , "void hs_bindgen_57f4aab65f2a1232 ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  (barD)(arg1);"
  , "}"
  , "void hs_bindgen_6479d953f4c6848f ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  (barE)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_40e15e86e5db36ce" hs_bindgen_40e15e86e5db36ce_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_foo@
hs_bindgen_40e15e86e5db36ce ::
     BG.FunPtr MyFunction
  -> IO ()
hs_bindgen_40e15e86e5db36ce =
  BG.fromFFIType hs_bindgen_40e15e86e5db36ce_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 6:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
foo ::
     BG.FunPtr MyFunction
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_40e15e86e5db36ce

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_e13a57fd1d27f6e6" hs_bindgen_e13a57fd1d27f6e6_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooA@
hs_bindgen_e13a57fd1d27f6e6 ::
     BG.FunPtr A
  -> IO ()
hs_bindgen_e13a57fd1d27f6e6 =
  BG.fromFFIType hs_bindgen_e13a57fd1d27f6e6_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 12:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooA ::
     BG.FunPtr A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_e13a57fd1d27f6e6

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_fd490df5087893ae" hs_bindgen_fd490df5087893ae_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooB@
hs_bindgen_fd490df5087893ae ::
     BG.FunPtr B
  -> IO ()
hs_bindgen_fd490df5087893ae =
  BG.fromFFIType hs_bindgen_fd490df5087893ae_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 13:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooB ::
     BG.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_fd490df5087893ae

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_c552a862db9ba6eb" hs_bindgen_c552a862db9ba6eb_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooC@
hs_bindgen_c552a862db9ba6eb ::
     BG.FunPtr M.C
  -> IO ()
hs_bindgen_c552a862db9ba6eb =
  BG.fromFFIType hs_bindgen_c552a862db9ba6eb_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooC ::
     BG.FunPtr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_c552a862db9ba6eb

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_b08ba864cf83a382" hs_bindgen_b08ba864cf83a382_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooD@
hs_bindgen_b08ba864cf83a382 ::
     BG.FunPtr M.D
  -> IO ()
hs_bindgen_b08ba864cf83a382 =
  BG.fromFFIType hs_bindgen_b08ba864cf83a382_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooD ::
     BG.FunPtr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_b08ba864cf83a382

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_ca717b3afd1e43f5" hs_bindgen_ca717b3afd1e43f5_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooE@
hs_bindgen_ca717b3afd1e43f5 ::
     BG.FunPtr E
  -> IO ()
hs_bindgen_ca717b3afd1e43f5 =
  BG.fromFFIType hs_bindgen_ca717b3afd1e43f5_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooE ::
     BG.FunPtr E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_ca717b3afd1e43f5

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_a708e95f35bff290" hs_bindgen_a708e95f35bff290_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_bar@
hs_bindgen_a708e95f35bff290 ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO ()
hs_bindgen_a708e95f35bff290 =
  BG.fromFFIType hs_bindgen_a708e95f35bff290_base

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 40:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
bar ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_a708e95f35bff290

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_barA@
foreign import ccall safe "hs_bindgen_2991dd76f4337b78" hs_bindgen_2991dd76f4337b78_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_barA@
hs_bindgen_2991dd76f4337b78 ::
     BG.FunPtr A
  -> IO ()
hs_bindgen_2991dd76f4337b78 =
  BG.fromFFIType hs_bindgen_2991dd76f4337b78_base

{-| __C declaration:__ @barA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 42:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barA ::
     BG.FunPtr A
     -- ^ __C declaration:__ @x@
  -> IO ()
barA = hs_bindgen_2991dd76f4337b78

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_barB@
foreign import ccall safe "hs_bindgen_104de3f97206bd1d" hs_bindgen_104de3f97206bd1d_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_barB@
hs_bindgen_104de3f97206bd1d ::
     BG.FunPtr B
  -> IO ()
hs_bindgen_104de3f97206bd1d =
  BG.fromFFIType hs_bindgen_104de3f97206bd1d_base

{-| __C declaration:__ @barB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 43:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barB ::
     BG.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
barB = hs_bindgen_104de3f97206bd1d

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_barC@
foreign import ccall safe "hs_bindgen_963bcd9e5467fd8f" hs_bindgen_963bcd9e5467fd8f_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_barC@
hs_bindgen_963bcd9e5467fd8f ::
     BG.FunPtr M.C
  -> IO ()
hs_bindgen_963bcd9e5467fd8f =
  BG.fromFFIType hs_bindgen_963bcd9e5467fd8f_base

{-| __C declaration:__ @barC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 45:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barC ::
     BG.FunPtr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
barC = hs_bindgen_963bcd9e5467fd8f

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_barD@
foreign import ccall safe "hs_bindgen_57f4aab65f2a1232" hs_bindgen_57f4aab65f2a1232_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_barD@
hs_bindgen_57f4aab65f2a1232 ::
     BG.FunPtr M.D
  -> IO ()
hs_bindgen_57f4aab65f2a1232 =
  BG.fromFFIType hs_bindgen_57f4aab65f2a1232_base

{-| __C declaration:__ @barD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 46:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barD ::
     BG.FunPtr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
barD = hs_bindgen_57f4aab65f2a1232

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_barE@
foreign import ccall safe "hs_bindgen_6479d953f4c6848f" hs_bindgen_6479d953f4c6848f_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_barE@
hs_bindgen_6479d953f4c6848f ::
     BG.FunPtr E
  -> IO ()
hs_bindgen_6479d953f4c6848f =
  BG.fromFFIType hs_bindgen_6479d953f4c6848f_base

{-| __C declaration:__ @barE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 47:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barE ::
     BG.FunPtr E
     -- ^ __C declaration:__ @x@
  -> IO ()
barE = hs_bindgen_6479d953f4c6848f
