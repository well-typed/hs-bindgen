{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/function.h>"
  , "void hs_bindgen_40e15e86e5db36ce ("
  , "  MyFunction *arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_e13a57fd1d27f6e6 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_fd490df5087893ae ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  , "void hs_bindgen_a708e95f35bff290 ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  bar(arg1);"
  , "}"
  , "void hs_bindgen_2991dd76f4337b78 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  barA(arg1);"
  , "}"
  , "void hs_bindgen_104de3f97206bd1d ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  barB(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_40e15e86e5db36ce" hs_bindgen_40e15e86e5db36ce_base ::
     Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_foo@
hs_bindgen_40e15e86e5db36ce ::
     Ptr.FunPtr MyFunction
  -> IO ()
hs_bindgen_40e15e86e5db36ce =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_40e15e86e5db36ce_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
foo ::
     Ptr.FunPtr MyFunction
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_40e15e86e5db36ce

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_e13a57fd1d27f6e6" hs_bindgen_e13a57fd1d27f6e6_base ::
     Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooA@
hs_bindgen_e13a57fd1d27f6e6 ::
     Ptr.FunPtr A
  -> IO ()
hs_bindgen_e13a57fd1d27f6e6 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_e13a57fd1d27f6e6_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooA ::
     Ptr.FunPtr A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_e13a57fd1d27f6e6

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_fd490df5087893ae" hs_bindgen_fd490df5087893ae_base ::
     Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_fooB@
hs_bindgen_fd490df5087893ae ::
     Ptr.FunPtr B
  -> IO ()
hs_bindgen_fd490df5087893ae =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_fd490df5087893ae_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooB ::
     Ptr.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_fd490df5087893ae

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_a708e95f35bff290" hs_bindgen_a708e95f35bff290_base ::
     Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_bar@
hs_bindgen_a708e95f35bff290 ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  -> IO ()
hs_bindgen_a708e95f35bff290 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_a708e95f35bff290_base

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
bar ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_a708e95f35bff290

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_barA@
foreign import ccall safe "hs_bindgen_2991dd76f4337b78" hs_bindgen_2991dd76f4337b78_base ::
     Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_barA@
hs_bindgen_2991dd76f4337b78 ::
     Ptr.FunPtr A
  -> IO ()
hs_bindgen_2991dd76f4337b78 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_2991dd76f4337b78_base

{-| __C declaration:__ @barA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 36:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barA ::
     Ptr.FunPtr A
     -- ^ __C declaration:__ @x@
  -> IO ()
barA = hs_bindgen_2991dd76f4337b78

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_barB@
foreign import ccall safe "hs_bindgen_104de3f97206bd1d" hs_bindgen_104de3f97206bd1d_base ::
     Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_Safe_barB@
hs_bindgen_104de3f97206bd1d ::
     Ptr.FunPtr B
  -> IO ()
hs_bindgen_104de3f97206bd1d =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_104de3f97206bd1d_base

{-| __C declaration:__ @barB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 37:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barB ::
     Ptr.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
barB = hs_bindgen_104de3f97206bd1d
