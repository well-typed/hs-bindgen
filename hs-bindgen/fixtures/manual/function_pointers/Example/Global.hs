{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.PtrConst
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <manual/function_pointers.h>"
  , "/* test_manualfunction_pointers_Example_get_apply1_nopointer_var */"
  , "__attribute__ ((const))"
  , "signed int (*const *hs_bindgen_8ca6ae5d7bf88149 (void)) ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &apply1_nopointer_var;"
  , "}"
  , "/* test_manualfunction_pointers_Example_get_apply1_struct */"
  , "__attribute__ ((const))"
  , "struct Apply1Struct const *hs_bindgen_33a4962ebf2e9daf (void)"
  , "{"
  , "  return &apply1_struct;"
  , "}"
  , "/* test_manualfunction_pointers_Example_get_apply1_union */"
  , "__attribute__ ((const))"
  , "union Apply1Union const *hs_bindgen_d1ddd3b607c95874 (void)"
  , "{"
  , "  return &apply1_union;"
  , "}"
  ]))

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1_nopointer_var@
foreign import ccall unsafe "hs_bindgen_8ca6ae5d7bf88149" hs_bindgen_8ca6ae5d7bf88149_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1_nopointer_var@
hs_bindgen_8ca6ae5d7bf88149 :: IO (HsBindgen.Runtime.PtrConst.PtrConst (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)))
hs_bindgen_8ca6ae5d7bf88149 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_8ca6ae5d7bf88149_base

{-# NOINLINE hs_bindgen_505081298c324425 #-}
{-| A global variable pointing to a function like apply1_nopointer().

__C declaration:__ @apply1_nopointer_var@

__defined at:__ @manual\/function_pointers.h 34:21@

__exported by:__ @manual\/function_pointers.h@

__unique:__ @test_manualfunction_pointers_Example_apply1_nopointer_var@
-}
hs_bindgen_505081298c324425 :: HsBindgen.Runtime.PtrConst.PtrConst (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))
hs_bindgen_505081298c324425 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8ca6ae5d7bf88149

{-# NOINLINE apply1_nopointer_var #-}
apply1_nopointer_var :: Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
apply1_nopointer_var =
  GHC.IO.Unsafe.unsafePerformIO (HsBindgen.Runtime.PtrConst.peek hs_bindgen_505081298c324425)

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1_struct@
foreign import ccall unsafe "hs_bindgen_33a4962ebf2e9daf" hs_bindgen_33a4962ebf2e9daf_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1_struct@
hs_bindgen_33a4962ebf2e9daf :: IO (HsBindgen.Runtime.PtrConst.PtrConst Apply1Struct)
hs_bindgen_33a4962ebf2e9daf =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_33a4962ebf2e9daf_base

{-# NOINLINE hs_bindgen_96ffcf57d318ddc0 #-}
{-| __C declaration:__ @apply1_struct@

    __defined at:__ @manual\/function_pointers.h 40:34@

    __exported by:__ @manual\/function_pointers.h@

    __unique:__ @test_manualfunction_pointers_Example_apply1_struct@
-}
hs_bindgen_96ffcf57d318ddc0 :: HsBindgen.Runtime.PtrConst.PtrConst Apply1Struct
hs_bindgen_96ffcf57d318ddc0 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_33a4962ebf2e9daf

{-# NOINLINE apply1_struct #-}
apply1_struct :: Apply1Struct
apply1_struct =
  GHC.IO.Unsafe.unsafePerformIO (HsBindgen.Runtime.PtrConst.peek hs_bindgen_96ffcf57d318ddc0)

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1_union@
foreign import ccall unsafe "hs_bindgen_d1ddd3b607c95874" hs_bindgen_d1ddd3b607c95874_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1_union@
hs_bindgen_d1ddd3b607c95874 :: IO (HsBindgen.Runtime.PtrConst.PtrConst Apply1Union)
hs_bindgen_d1ddd3b607c95874 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d1ddd3b607c95874_base

{-# NOINLINE hs_bindgen_8368fc70f5e0aec5 #-}
{-| __C declaration:__ @apply1_union@

    __defined at:__ @manual\/function_pointers.h 46:32@

    __exported by:__ @manual\/function_pointers.h@

    __unique:__ @test_manualfunction_pointers_Example_apply1_union@
-}
hs_bindgen_8368fc70f5e0aec5 :: HsBindgen.Runtime.PtrConst.PtrConst Apply1Union
hs_bindgen_8368fc70f5e0aec5 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d1ddd3b607c95874

{-# NOINLINE apply1_union #-}
apply1_union :: Apply1Union
apply1_union =
  GHC.IO.Unsafe.unsafePerformIO (HsBindgen.Runtime.PtrConst.peek hs_bindgen_8368fc70f5e0aec5)
