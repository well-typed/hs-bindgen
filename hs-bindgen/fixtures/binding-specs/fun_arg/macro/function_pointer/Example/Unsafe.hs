{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Internal.CAPI
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/function_pointer.h>"
  , "void hs_bindgen_fbc2ec26cd297034 ("
  , "  MyFunctionPointer arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_cf67e2fc00fd28d8 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_269a46f9680e33ed ("
  , "  B arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_fbc2ec26cd297034" hs_bindgen_fbc2ec26cd297034_base ::
     Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_foo@
hs_bindgen_fbc2ec26cd297034 ::
     MyFunctionPointer
  -> IO ()
hs_bindgen_fbc2ec26cd297034 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_fbc2ec26cd297034_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
foo ::
     MyFunctionPointer
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_fbc2ec26cd297034

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_cf67e2fc00fd28d8" hs_bindgen_cf67e2fc00fd28d8_base ::
     Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooA@
hs_bindgen_cf67e2fc00fd28d8 ::
     A
  -> IO ()
hs_bindgen_cf67e2fc00fd28d8 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_cf67e2fc00fd28d8_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_cf67e2fc00fd28d8

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_269a46f9680e33ed" hs_bindgen_269a46f9680e33ed_base ::
     Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Unsafe_fooB@
hs_bindgen_269a46f9680e33ed ::
     B
  -> IO ()
hs_bindgen_269a46f9680e33ed =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_269a46f9680e33ed_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_269a46f9680e33ed
