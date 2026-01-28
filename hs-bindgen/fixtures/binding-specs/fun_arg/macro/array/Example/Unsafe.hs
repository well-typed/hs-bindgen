{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.HasFFIType
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/array.h>"
  , "void hs_bindgen_969f916bf9590709 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_fb89f30695d9a112 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_6882cd6e82766148 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_969f916bf9590709" hs_bindgen_969f916bf9590709_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Unsafe_foo@
hs_bindgen_969f916bf9590709 ::
     Ptr.Ptr FC.CInt
  -> IO ()
hs_bindgen_969f916bf9590709 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_969f916bf9590709_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
foo ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_969f916bf9590709

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_fb89f30695d9a112" hs_bindgen_fb89f30695d9a112_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Unsafe_fooA@
hs_bindgen_fb89f30695d9a112 ::
     Ptr.Ptr FC.CInt
  -> IO ()
hs_bindgen_fb89f30695d9a112 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_fb89f30695d9a112_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
fooA ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_fb89f30695d9a112

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_6882cd6e82766148" hs_bindgen_6882cd6e82766148_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_Unsafe_fooB@
hs_bindgen_6882cd6e82766148 ::
     Ptr.Ptr FC.CInt
  -> IO ()
hs_bindgen_6882cd6e82766148 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_6882cd6e82766148_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
fooB ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_6882cd6e82766148
