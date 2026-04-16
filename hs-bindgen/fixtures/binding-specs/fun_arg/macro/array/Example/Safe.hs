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
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified M
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/array.h>"
  , "void hs_bindgen_2a6ef3a515232132 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_4449a68917cbc499 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (fooA)(arg1);"
  , "}"
  , "void hs_bindgen_05766199d1b077bb ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (fooB)(arg1);"
  , "}"
  , "void hs_bindgen_adeb3a183d6676ee ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (fooC)(arg1);"
  , "}"
  , "void hs_bindgen_e9321a5f27f4381e ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (fooD)(arg1);"
  , "}"
  , "void hs_bindgen_257d56ad8cb31970 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (fooE)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_2a6ef3a515232132" hs_bindgen_2a6ef3a515232132_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Safe_foo@
hs_bindgen_2a6ef3a515232132 ::
     RIP.Ptr (IsA.Elem MyArray)
  -> IO ()
hs_bindgen_2a6ef3a515232132 =
  RIP.fromFFIType hs_bindgen_2a6ef3a515232132_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 6:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
foo ::
     RIP.Ptr (IsA.Elem MyArray)
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_2a6ef3a515232132

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_4449a68917cbc499" hs_bindgen_4449a68917cbc499_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Safe_fooA@
hs_bindgen_4449a68917cbc499 ::
     RIP.Ptr (IsA.Elem A)
  -> IO ()
hs_bindgen_4449a68917cbc499 =
  RIP.fromFFIType hs_bindgen_4449a68917cbc499_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 12:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
fooA ::
     RIP.Ptr (IsA.Elem A)
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_4449a68917cbc499

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_05766199d1b077bb" hs_bindgen_05766199d1b077bb_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Safe_fooB@
hs_bindgen_05766199d1b077bb ::
     RIP.Ptr (IsA.Elem B)
  -> IO ()
hs_bindgen_05766199d1b077bb =
  RIP.fromFFIType hs_bindgen_05766199d1b077bb_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 13:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
fooB ::
     RIP.Ptr (IsA.Elem B)
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_05766199d1b077bb

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_adeb3a183d6676ee" hs_bindgen_adeb3a183d6676ee_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Safe_fooC@
hs_bindgen_adeb3a183d6676ee ::
     RIP.Ptr (IsA.Elem M.C)
  -> IO ()
hs_bindgen_adeb3a183d6676ee =
  RIP.fromFFIType hs_bindgen_adeb3a183d6676ee_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
fooC ::
     RIP.Ptr (IsA.Elem M.C)
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_adeb3a183d6676ee

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_e9321a5f27f4381e" hs_bindgen_e9321a5f27f4381e_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Safe_fooD@
hs_bindgen_e9321a5f27f4381e ::
     RIP.Ptr (IsA.Elem M.D)
  -> IO ()
hs_bindgen_e9321a5f27f4381e =
  RIP.fromFFIType hs_bindgen_e9321a5f27f4381e_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
fooD ::
     RIP.Ptr (IsA.Elem M.D)
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_e9321a5f27f4381e

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_257d56ad8cb31970" hs_bindgen_257d56ad8cb31970_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Safe_fooE@
hs_bindgen_257d56ad8cb31970 ::
     RIP.Ptr (IsA.Elem E)
  -> IO ()
hs_bindgen_257d56ad8cb31970 =
  RIP.fromFFIType hs_bindgen_257d56ad8cb31970_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
fooE ::
     RIP.Ptr (IsA.Elem E)
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_257d56ad8cb31970
