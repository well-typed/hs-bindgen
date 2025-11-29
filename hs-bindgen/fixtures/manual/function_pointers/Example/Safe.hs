{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <manual/function_pointers.h>"
  , "signed int hs_bindgen_test_manualfunction_pointers_55e5eb89e54abf83 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square(arg1);"
  , "}"
  , "signed int hs_bindgen_test_manualfunction_pointers_680daf766a044980 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return plus(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_manualfunction_pointers_abcb860034253564 ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return apply1(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_manualfunction_pointers_1ad13c166a710f40 ("
  , "  signed int (*arg1) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , "),"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return apply2(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_test_manualfunction_pointers_a8ef4d9e6ce68f54 ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return apply1_pointer_arg(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_manualfunction_pointers_7dc4caa1f7f0caf0 ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return apply1_nopointer_arg(arg1, arg2);"
  , "}"
  , "signed int (*const hs_bindgen_test_manualfunction_pointers_3612aa0d10e36d5b (void)) ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return apply1_nopointer_res();"
  , "}"
  ]))

{-| __C declaration:__ @square@

    __defined at:__ @manual\/function_pointers.h:5:12@

    __exported by:__ @manual\/function_pointers.h@

    __unique:__ @ExampleJust Safesquare@
-}
foreign import ccall safe "hs_bindgen_test_manualfunction_pointers_55e5eb89e54abf83" square ::
     FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @plus@

    __defined at:__ @manual\/function_pointers.h:7:12@

    __exported by:__ @manual\/function_pointers.h@

    __unique:__ @ExampleJust Safeplus@
-}
foreign import ccall safe "hs_bindgen_test_manualfunction_pointers_680daf766a044980" plus ::
     FC.CInt
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @apply1@

    __defined at:__ @manual\/function_pointers.h:9:12@

    __exported by:__ @manual\/function_pointers.h@

    __unique:__ @ExampleJust Safeapply1@
-}
foreign import ccall safe "hs_bindgen_test_manualfunction_pointers_abcb860034253564" apply1 ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     {- ^ __C declaration:__ @f@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @apply2@

    __defined at:__ @manual\/function_pointers.h:11:12@

    __exported by:__ @manual\/function_pointers.h@

    __unique:__ @ExampleJust Safeapply2@
-}
foreign import ccall safe "hs_bindgen_test_manualfunction_pointers_1ad13c166a710f40" apply2 ::
     Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)
     {- ^ __C declaration:__ @f@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @y@
     -}
  -> IO FC.CInt

{-| Basically the same as apply1(), but here for illustratory purposes.

__C declaration:__ @apply1_pointer_arg@

__defined at:__ @manual\/function_pointers.h:22:12@

__exported by:__ @manual\/function_pointers.h@

__unique:__ @ExampleJust Safeapply1_pointer_arg@
-}
foreign import ccall safe "hs_bindgen_test_manualfunction_pointers_a8ef4d9e6ce68f54" apply1_pointer_arg ::
     Ptr.FunPtr Int2int
  -> FC.CInt
  -> IO FC.CInt

{-| A version of apply1_pointer_arg() that declares to take a argument of function type, rather than a pointer-to-function type.

__C declaration:__ @apply1_nopointer_arg@

__defined at:__ @manual\/function_pointers.h:26:12@

__exported by:__ @manual\/function_pointers.h@

__unique:__ @ExampleJust Safeapply1_nopointer_arg@
-}
foreign import ccall safe "hs_bindgen_test_manualfunction_pointers_7dc4caa1f7f0caf0" apply1_nopointer_arg ::
     Ptr.FunPtr Int2int
  -> FC.CInt
  -> IO FC.CInt

{-| A function returning a pointer to a function like apply1_nopointer().

__C declaration:__ @apply1_nopointer_res@

__defined at:__ @manual\/function_pointers.h:31:21@

__exported by:__ @manual\/function_pointers.h@

__unique:__ @ExampleJust Safeapply1_nopointer_res@
-}
foreign import ccall safe "hs_bindgen_test_manualfunction_pointers_3612aa0d10e36d5b" apply1_nopointer_res ::
     IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))
