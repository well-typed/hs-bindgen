{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/array.h>"
  , "void hs_bindgen_969f916bf9590709 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_fb89f30695d9a112 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (fooA)(arg1);"
  , "}"
  , "void hs_bindgen_6882cd6e82766148 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (fooB)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_969f916bf9590709" hs_bindgen_969f916bf9590709_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Unsafe_foo@
hs_bindgen_969f916bf9590709 ::
     RIP.Ptr (IsA.Elem MyArray)
  -> IO ()
hs_bindgen_969f916bf9590709 =
  RIP.fromFFIType hs_bindgen_969f916bf9590709_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
foo ::
     RIP.Ptr (IsA.Elem MyArray)
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_969f916bf9590709

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_fb89f30695d9a112" hs_bindgen_fb89f30695d9a112_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Unsafe_fooA@
hs_bindgen_fb89f30695d9a112 ::
     RIP.Ptr (IsA.Elem A)
  -> IO ()
hs_bindgen_fb89f30695d9a112 =
  RIP.fromFFIType hs_bindgen_fb89f30695d9a112_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
fooA ::
     RIP.Ptr (IsA.Elem A)
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_fb89f30695d9a112

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_6882cd6e82766148" hs_bindgen_6882cd6e82766148_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Unsafe_fooB@
hs_bindgen_6882cd6e82766148 ::
     RIP.Ptr (IsA.Elem B)
  -> IO ()
hs_bindgen_6882cd6e82766148 =
  RIP.fromFFIType hs_bindgen_6882cd6e82766148_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
fooB ::
     RIP.Ptr (IsA.Elem B)
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_6882cd6e82766148
