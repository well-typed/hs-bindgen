{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <manual/function_pointers.h>"
  , "/* test_manualfunction_pointers_Example_get_square_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_1820f088ae031e75 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square;"
  , "}"
  , "/* test_manualfunction_pointers_Example_get_plus_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_bbf0ff803f52cc45 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &plus;"
  , "}"
  , "/* test_manualfunction_pointers_Example_get_apply1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_9515a684c6197849 (void)) ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &apply1;"
  , "}"
  , "/* test_manualfunction_pointers_Example_get_apply2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_1bf5d46d7038cf34 (void)) ("
  , "  signed int (*arg1) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , "),"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &apply2;"
  , "}"
  , "/* test_manualfunction_pointers_Example_get_apply1_pointer_arg_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_95e560983f86bd1d (void)) ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &apply1_pointer_arg;"
  , "}"
  , "/* test_manualfunction_pointers_Example_get_apply1_nopointer_arg_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_9738799dd67af2e9 (void)) ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &apply1_nopointer_arg;"
  , "}"
  , "/* test_manualfunction_pointers_Example_get_apply1_nopointer_res_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*const (*hs_bindgen_c9d47a6b0f980aa5 (void)) (void)) ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &apply1_nopointer_res;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_1820f088ae031e75" hs_bindgen_1820f088ae031e75_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_manualfunction_pointers_Example_get_square_ptr@
hs_bindgen_1820f088ae031e75 ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
hs_bindgen_1820f088ae031e75 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_1820f088ae031e75_base

{-# NOINLINE square_ptr #-}

{-| __C declaration:__ @square@

    __defined at:__ @manual\/function_pointers.h:5:12@

    __exported by:__ @manual\/function_pointers.h@
-}
square_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1820f088ae031e75

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_bbf0ff803f52cc45" hs_bindgen_bbf0ff803f52cc45_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_manualfunction_pointers_Example_get_plus_ptr@
hs_bindgen_bbf0ff803f52cc45 ::
     IO (Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt))
hs_bindgen_bbf0ff803f52cc45 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_bbf0ff803f52cc45_base

{-# NOINLINE plus_ptr #-}

{-| __C declaration:__ @plus@

    __defined at:__ @manual\/function_pointers.h:7:12@

    __exported by:__ @manual\/function_pointers.h@
-}
plus_ptr :: Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)
plus_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bbf0ff803f52cc45

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_9515a684c6197849" hs_bindgen_9515a684c6197849_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_manualfunction_pointers_Example_get_apply1_ptr@
hs_bindgen_9515a684c6197849 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> FC.CInt -> IO FC.CInt))
hs_bindgen_9515a684c6197849 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_9515a684c6197849_base

{-# NOINLINE apply1_ptr #-}

{-| __C declaration:__ @apply1@

    __defined at:__ @manual\/function_pointers.h:9:12@

    __exported by:__ @manual\/function_pointers.h@
-}
apply1_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> FC.CInt -> IO FC.CInt)
apply1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9515a684c6197849

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_1bf5d46d7038cf34" hs_bindgen_1bf5d46d7038cf34_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_manualfunction_pointers_Example_get_apply2_ptr@
hs_bindgen_1bf5d46d7038cf34 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)) -> FC.CInt -> FC.CInt -> IO FC.CInt))
hs_bindgen_1bf5d46d7038cf34 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_1bf5d46d7038cf34_base

{-# NOINLINE apply2_ptr #-}

{-| __C declaration:__ @apply2@

    __defined at:__ @manual\/function_pointers.h:11:12@

    __exported by:__ @manual\/function_pointers.h@
-}
apply2_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)) -> FC.CInt -> FC.CInt -> IO FC.CInt)
apply2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1bf5d46d7038cf34

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_95e560983f86bd1d" hs_bindgen_95e560983f86bd1d_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_manualfunction_pointers_Example_get_apply1_pointer_arg_ptr@
hs_bindgen_95e560983f86bd1d ::
     IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))
hs_bindgen_95e560983f86bd1d =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_95e560983f86bd1d_base

{-# NOINLINE apply1_pointer_arg_ptr #-}

{-| Basically the same as apply1(), but here for illustratory purposes.

__C declaration:__ @apply1_pointer_arg@

__defined at:__ @manual\/function_pointers.h:22:12@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_pointer_arg_ptr :: Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
apply1_pointer_arg_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_95e560983f86bd1d

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_9738799dd67af2e9" hs_bindgen_9738799dd67af2e9_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_manualfunction_pointers_Example_get_apply1_nopointer_arg_ptr@
hs_bindgen_9738799dd67af2e9 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))
hs_bindgen_9738799dd67af2e9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_9738799dd67af2e9_base

{-# NOINLINE apply1_nopointer_arg_ptr #-}

{-| A version of apply1_pointer_arg() that declares to take a argument of function type, rather than a pointer-to-function type.

__C declaration:__ @apply1_nopointer_arg@

__defined at:__ @manual\/function_pointers.h:26:12@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_nopointer_arg_ptr :: Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
apply1_nopointer_arg_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9738799dd67af2e9

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c9d47a6b0f980aa5" hs_bindgen_c9d47a6b0f980aa5_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_manualfunction_pointers_Example_get_apply1_nopointer_res_ptr@
hs_bindgen_c9d47a6b0f980aa5 ::
     IO (Ptr.FunPtr (IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))))
hs_bindgen_c9d47a6b0f980aa5 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_c9d47a6b0f980aa5_base

{-# NOINLINE apply1_nopointer_res_ptr #-}

{-| A function returning a pointer to a function like apply1_nopointer().

__C declaration:__ @apply1_nopointer_res@

__defined at:__ @manual\/function_pointers.h:31:21@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_nopointer_res_ptr :: Ptr.FunPtr (IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)))
apply1_nopointer_res_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c9d47a6b0f980aa5
