{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <manual/function_pointers.h>"
  , "/* test_manualfunction_pointers_Example_get_apply1_nopointer_var_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*const *hs_bindgen_ab897ed3a6702a69 (void)) ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &apply1_nopointer_var;"
  , "}"
  , "/* test_manualfunction_pointers_Example_get_apply1_struct_ptr */"
  , "__attribute__ ((const))"
  , "struct Apply1Struct const *hs_bindgen_448bdc6115d5924e (void)"
  , "{"
  , "  return &apply1_struct;"
  , "}"
  , "/* test_manualfunction_pointers_Example_get_apply1_union_ptr */"
  , "__attribute__ ((const))"
  , "union Apply1Union const *hs_bindgen_3f47656e6bb54e94 (void)"
  , "{"
  , "  return &apply1_union;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_ab897ed3a6702a69" hs_bindgen_ab897ed3a6702a69_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_manualfunction_pointers_Example_get_apply1_nopointer_var_ptr@
hs_bindgen_ab897ed3a6702a69 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)))
hs_bindgen_ab897ed3a6702a69 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_ab897ed3a6702a69_base

{-# NOINLINE apply1_nopointer_var_ptr #-}

{-| A global variable pointing to a function like apply1_nopointer().

__C declaration:__ @apply1_nopointer_var@

__defined at:__ @manual\/function_pointers.h:34:21@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_nopointer_var_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))
apply1_nopointer_var_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ab897ed3a6702a69

{-# NOINLINE apply1_nopointer_var #-}

apply1_nopointer_var :: Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
apply1_nopointer_var =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr apply1_nopointer_var_ptr))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_448bdc6115d5924e" hs_bindgen_448bdc6115d5924e_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_manualfunction_pointers_Example_get_apply1_struct_ptr@
hs_bindgen_448bdc6115d5924e ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Apply1Struct)
hs_bindgen_448bdc6115d5924e =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_448bdc6115d5924e_base

{-# NOINLINE apply1_struct_ptr #-}

{-| __C declaration:__ @apply1_struct@

    __defined at:__ @manual\/function_pointers.h:40:34@

    __exported by:__ @manual\/function_pointers.h@
-}
apply1_struct_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr Apply1Struct
apply1_struct_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_448bdc6115d5924e

{-# NOINLINE apply1_struct #-}

apply1_struct :: Apply1Struct
apply1_struct =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr apply1_struct_ptr))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3f47656e6bb54e94" hs_bindgen_3f47656e6bb54e94_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_manualfunction_pointers_Example_get_apply1_union_ptr@
hs_bindgen_3f47656e6bb54e94 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Apply1Union)
hs_bindgen_3f47656e6bb54e94 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_3f47656e6bb54e94_base

{-# NOINLINE apply1_union_ptr #-}

{-| __C declaration:__ @apply1_union@

    __defined at:__ @manual\/function_pointers.h:46:32@

    __exported by:__ @manual\/function_pointers.h@
-}
apply1_union_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr Apply1Union
apply1_union_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3f47656e6bb54e94

{-# NOINLINE apply1_union #-}

apply1_union :: Apply1Union
apply1_union =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr apply1_union_ptr))
