{-# LANGUAGE CApiFFI #-}
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
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified M
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/array_known_size.h>"
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
  , "void hs_bindgen_97086d4192532ee4 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (fooC)(arg1);"
  , "}"
  , "void hs_bindgen_ca769b1357fca61c ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (fooD)(arg1);"
  , "}"
  , "void hs_bindgen_eea5d25f5323ea60 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (fooE)(arg1);"
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

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 6:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
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

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 12:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
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

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 13:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
fooB ::
     RIP.Ptr (IsA.Elem B)
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_6882cd6e82766148

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_97086d4192532ee4" hs_bindgen_97086d4192532ee4_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Unsafe_fooC@
hs_bindgen_97086d4192532ee4 ::
     RIP.Ptr (IsA.Elem M.C)
  -> IO ()
hs_bindgen_97086d4192532ee4 =
  RIP.fromFFIType hs_bindgen_97086d4192532ee4_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
fooC ::
     RIP.Ptr (IsA.Elem M.C)
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_97086d4192532ee4

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Unsafe_fooD@
foreign import ccall unsafe "hs_bindgen_ca769b1357fca61c" hs_bindgen_ca769b1357fca61c_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Unsafe_fooD@
hs_bindgen_ca769b1357fca61c ::
     RIP.Ptr (IsA.Elem M.D)
  -> IO ()
hs_bindgen_ca769b1357fca61c =
  RIP.fromFFIType hs_bindgen_ca769b1357fca61c_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
fooD ::
     RIP.Ptr (IsA.Elem M.D)
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_ca769b1357fca61c

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Unsafe_fooE@
foreign import ccall unsafe "hs_bindgen_eea5d25f5323ea60" hs_bindgen_eea5d25f5323ea60_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Unsafe_fooE@
hs_bindgen_eea5d25f5323ea60 ::
     RIP.Ptr (IsA.Elem E)
  -> IO ()
hs_bindgen_eea5d25f5323ea60 =
  RIP.fromFFIType hs_bindgen_eea5d25f5323ea60_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
fooE ::
     RIP.Ptr (IsA.Elem E)
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_eea5d25f5323ea60
