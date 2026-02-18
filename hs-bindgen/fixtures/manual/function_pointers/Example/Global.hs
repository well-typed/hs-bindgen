{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1_nopointer_var@
hs_bindgen_8ca6ae5d7bf88149 :: IO (PtrConst.PtrConst (RIP.FunPtr ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt)))
hs_bindgen_8ca6ae5d7bf88149 =
  RIP.fromFFIType hs_bindgen_8ca6ae5d7bf88149_base

{-# NOINLINE hs_bindgen_505081298c324425 #-}
{-| A global variable pointing to a function like apply1_nopointer().

__C declaration:__ @apply1_nopointer_var@

__defined at:__ @manual\/function_pointers.h 34:21@

__exported by:__ @manual\/function_pointers.h@

__unique:__ @test_manualfunction_pointers_Example_apply1_nopointer_var@
-}
hs_bindgen_505081298c324425 :: PtrConst.PtrConst (RIP.FunPtr ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt))
hs_bindgen_505081298c324425 =
  RIP.unsafePerformIO hs_bindgen_8ca6ae5d7bf88149

{-# NOINLINE apply1_nopointer_var #-}
apply1_nopointer_var :: RIP.FunPtr ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt)
apply1_nopointer_var =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_505081298c324425)

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1_struct@
foreign import ccall unsafe "hs_bindgen_33a4962ebf2e9daf" hs_bindgen_33a4962ebf2e9daf_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1_struct@
hs_bindgen_33a4962ebf2e9daf :: IO (PtrConst.PtrConst Apply1Struct)
hs_bindgen_33a4962ebf2e9daf =
  RIP.fromFFIType hs_bindgen_33a4962ebf2e9daf_base

{-# NOINLINE hs_bindgen_96ffcf57d318ddc0 #-}
{-| __C declaration:__ @apply1_struct@

    __defined at:__ @manual\/function_pointers.h 40:34@

    __exported by:__ @manual\/function_pointers.h@

    __unique:__ @test_manualfunction_pointers_Example_apply1_struct@
-}
hs_bindgen_96ffcf57d318ddc0 :: PtrConst.PtrConst Apply1Struct
hs_bindgen_96ffcf57d318ddc0 =
  RIP.unsafePerformIO hs_bindgen_33a4962ebf2e9daf

{-# NOINLINE apply1_struct #-}
apply1_struct :: Apply1Struct
apply1_struct =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_96ffcf57d318ddc0)

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1_union@
foreign import ccall unsafe "hs_bindgen_d1ddd3b607c95874" hs_bindgen_d1ddd3b607c95874_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1_union@
hs_bindgen_d1ddd3b607c95874 :: IO (PtrConst.PtrConst Apply1Union)
hs_bindgen_d1ddd3b607c95874 =
  RIP.fromFFIType hs_bindgen_d1ddd3b607c95874_base

{-# NOINLINE hs_bindgen_8368fc70f5e0aec5 #-}
{-| __C declaration:__ @apply1_union@

    __defined at:__ @manual\/function_pointers.h 46:32@

    __exported by:__ @manual\/function_pointers.h@

    __unique:__ @test_manualfunction_pointers_Example_apply1_union@
-}
hs_bindgen_8368fc70f5e0aec5 :: PtrConst.PtrConst Apply1Union
hs_bindgen_8368fc70f5e0aec5 =
  RIP.unsafePerformIO hs_bindgen_d1ddd3b607c95874

{-# NOINLINE apply1_union #-}
apply1_union :: Apply1Union
apply1_union =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_8368fc70f5e0aec5)
