{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <manual/function_pointers.h>"
  , "signed int hs_bindgen_db669c022bc12e81 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square(arg1);"
  , "}"
  , "signed int hs_bindgen_0bb46b9dde136391 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return plus(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_3ff551d60859d359 ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return apply1(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_4c92d113161d27cf ("
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
  , "signed int hs_bindgen_2f904bf3ce7a5f06 ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return apply1_pointer_arg(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_966092b638965558 ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return apply1_nopointer_arg(arg1, arg2);"
  , "}"
  , "signed int (*const hs_bindgen_99a8340e6e6029c5 (void)) ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return apply1_nopointer_res();"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_db669c022bc12e81" square_base ::
     FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @square@

    __defined at:__ @manual\/function_pointers.h:5:12@

    __exported by:__ @manual\/function_pointers.h@

    __unique:__ @test_manualfunction_pointers_Example_Unsafe_square@
-}
square ::
     FC.CInt
  -> IO FC.CInt
square =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType square_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_0bb46b9dde136391" plus_base ::
     FC.CInt
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @plus@

    __defined at:__ @manual\/function_pointers.h:7:12@

    __exported by:__ @manual\/function_pointers.h@

    __unique:__ @test_manualfunction_pointers_Example_Unsafe_plus@
-}
plus ::
     FC.CInt
  -> FC.CInt
  -> IO FC.CInt
plus =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType plus_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3ff551d60859d359" apply1_base ::
     Ptr.FunPtr Void
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @apply1@

    __defined at:__ @manual\/function_pointers.h:9:12@

    __exported by:__ @manual\/function_pointers.h@

    __unique:__ @test_manualfunction_pointers_Example_Unsafe_apply1@
-}
apply1 ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @f@
  -> FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO FC.CInt
apply1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType apply1_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_4c92d113161d27cf" apply2_base ::
     Ptr.FunPtr Void
  -> FC.CInt
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @apply2@

    __defined at:__ @manual\/function_pointers.h:11:12@

    __exported by:__ @manual\/function_pointers.h@

    __unique:__ @test_manualfunction_pointers_Example_Unsafe_apply2@
-}
apply2 ::
     Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @f@
  -> FC.CInt
     -- ^ __C declaration:__ @x@
  -> FC.CInt
     -- ^ __C declaration:__ @y@
  -> IO FC.CInt
apply2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType apply2_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2f904bf3ce7a5f06" apply1_pointer_arg_base ::
     Ptr.FunPtr Void
  -> FC.CInt
  -> IO FC.CInt

{-| Basically the same as apply1(), but here for illustratory purposes.

__C declaration:__ @apply1_pointer_arg@

__defined at:__ @manual\/function_pointers.h:22:12@

__exported by:__ @manual\/function_pointers.h@

__unique:__ @test_manualfunction_pointers_Example_Unsafe_apply1_pointer_arg@
-}
apply1_pointer_arg ::
     Ptr.FunPtr Int2int
  -> FC.CInt
  -> IO FC.CInt
apply1_pointer_arg =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType apply1_pointer_arg_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_966092b638965558" apply1_nopointer_arg_base ::
     Ptr.FunPtr Void
  -> FC.CInt
  -> IO FC.CInt

{-| A version of apply1_pointer_arg() that declares to take a argument of function type, rather than a pointer-to-function type.

__C declaration:__ @apply1_nopointer_arg@

__defined at:__ @manual\/function_pointers.h:26:12@

__exported by:__ @manual\/function_pointers.h@

__unique:__ @test_manualfunction_pointers_Example_Unsafe_apply1_nopointer_arg@
-}
apply1_nopointer_arg ::
     Ptr.FunPtr Int2int
  -> FC.CInt
  -> IO FC.CInt
apply1_nopointer_arg =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType apply1_nopointer_arg_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_99a8340e6e6029c5" apply1_nopointer_res_base ::
     IO (Ptr.FunPtr Void)

{-| A function returning a pointer to a function like apply1_nopointer().

__C declaration:__ @apply1_nopointer_res@

__defined at:__ @manual\/function_pointers.h:31:21@

__exported by:__ @manual\/function_pointers.h@

__unique:__ @test_manualfunction_pointers_Example_Unsafe_apply1_nopointer_res@
-}
apply1_nopointer_res ::
     IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))
apply1_nopointer_res =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType apply1_nopointer_res_base
