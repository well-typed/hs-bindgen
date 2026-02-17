{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
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

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 5:6@

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

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 10:6@

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

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
fooB ::
     RIP.Ptr (IsA.Elem B)
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_05766199d1b077bb
