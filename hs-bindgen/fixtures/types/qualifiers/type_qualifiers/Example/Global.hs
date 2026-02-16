{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/qualifiers/type_qualifiers.h>"
  , "/* test_typesqualifierstype_qualifie_Example_get_a */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_b9f6c3995e03a64f (void)"
  , "{"
  , "  return &a;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_b */"
  , "__attribute__ ((const))"
  , "signed int const **hs_bindgen_2a09e45ccb76e4da (void)"
  , "{"
  , "  return &b;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_c */"
  , "__attribute__ ((const))"
  , "signed int *const *hs_bindgen_9cef5885d51a5077 (void)"
  , "{"
  , "  return &c;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_d */"
  , "__attribute__ ((const))"
  , "signed int const *const *hs_bindgen_039fc3584df4cf95 (void)"
  , "{"
  , "  return &d;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_e */"
  , "__attribute__ ((const))"
  , "signed int const *const **hs_bindgen_cdf2cbe09fc53047 (void)"
  , "{"
  , "  return &e;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_f */"
  , "__attribute__ ((const))"
  , "signed int const *const ***hs_bindgen_93ddb0dbf8706068 (void)"
  , "{"
  , "  return &f;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_g */"
  , "__attribute__ ((const))"
  , "signed int const **const **hs_bindgen_e6c23ab326a206a8 (void)"
  , "{"
  , "  return &g;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_h */"
  , "__attribute__ ((const))"
  , "signed int const *const *const *const *hs_bindgen_f23087f72d34068b (void)"
  , "{"
  , "  return &h;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_i */"
  , "__attribute__ ((const))"
  , "signed int *const **const **hs_bindgen_ed73ead5c58fe94a (void)"
  , "{"
  , "  return &i;"
  , "}"
  , "/* test_typesqualifierstype_qualifie_Example_get_j */"
  , "__attribute__ ((const))"
  , "signed int const *const *const ***hs_bindgen_92d2adc70d4b87ee (void)"
  , "{"
  , "  return &j;"
  , "}"
  ]))

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_a@
foreign import ccall unsafe "hs_bindgen_b9f6c3995e03a64f" hs_bindgen_b9f6c3995e03a64f_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_a@
hs_bindgen_b9f6c3995e03a64f :: IO (PtrConst.PtrConst RIP.CInt)
hs_bindgen_b9f6c3995e03a64f =
  RIP.fromFFIType hs_bindgen_b9f6c3995e03a64f_base

{-# NOINLINE hs_bindgen_568187d7e5f899e0 #-}
{-| __C declaration:__ @a@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 5:18@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@

    __unique:__ @test_typesqualifierstype_qualifie_Example_a@
-}
hs_bindgen_568187d7e5f899e0 :: PtrConst.PtrConst RIP.CInt
hs_bindgen_568187d7e5f899e0 =
  RIP.unsafePerformIO hs_bindgen_b9f6c3995e03a64f

{-# NOINLINE a #-}
a :: RIP.CInt
a =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_568187d7e5f899e0)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_b@
foreign import ccall unsafe "hs_bindgen_2a09e45ccb76e4da" hs_bindgen_2a09e45ccb76e4da_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_b@
hs_bindgen_2a09e45ccb76e4da :: IO (RIP.Ptr (PtrConst.PtrConst RIP.CInt))
hs_bindgen_2a09e45ccb76e4da =
  RIP.fromFFIType hs_bindgen_2a09e45ccb76e4da_base

{-# NOINLINE b #-}
{-| __C declaration:__ @b@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 7:19@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
b :: RIP.Ptr (PtrConst.PtrConst RIP.CInt)
b = RIP.unsafePerformIO hs_bindgen_2a09e45ccb76e4da

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_c@
foreign import ccall unsafe "hs_bindgen_9cef5885d51a5077" hs_bindgen_9cef5885d51a5077_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_c@
hs_bindgen_9cef5885d51a5077 :: IO (PtrConst.PtrConst (RIP.Ptr RIP.CInt))
hs_bindgen_9cef5885d51a5077 =
  RIP.fromFFIType hs_bindgen_9cef5885d51a5077_base

{-# NOINLINE hs_bindgen_86ab1f1ce8db256e #-}
{-| __C declaration:__ @c@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 9:19@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@

    __unique:__ @test_typesqualifierstype_qualifie_Example_c@
-}
hs_bindgen_86ab1f1ce8db256e :: PtrConst.PtrConst (RIP.Ptr RIP.CInt)
hs_bindgen_86ab1f1ce8db256e =
  RIP.unsafePerformIO hs_bindgen_9cef5885d51a5077

{-# NOINLINE c #-}
c :: RIP.Ptr RIP.CInt
c =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_86ab1f1ce8db256e)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_d@
foreign import ccall unsafe "hs_bindgen_039fc3584df4cf95" hs_bindgen_039fc3584df4cf95_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_d@
hs_bindgen_039fc3584df4cf95 :: IO (PtrConst.PtrConst (PtrConst.PtrConst RIP.CInt))
hs_bindgen_039fc3584df4cf95 =
  RIP.fromFFIType hs_bindgen_039fc3584df4cf95_base

{-# NOINLINE hs_bindgen_f5892dac29d00ba8 #-}
{-| __C declaration:__ @d@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 11:25@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@

    __unique:__ @test_typesqualifierstype_qualifie_Example_d@
-}
hs_bindgen_f5892dac29d00ba8 :: PtrConst.PtrConst (PtrConst.PtrConst RIP.CInt)
hs_bindgen_f5892dac29d00ba8 =
  RIP.unsafePerformIO hs_bindgen_039fc3584df4cf95

{-# NOINLINE d #-}
d :: PtrConst.PtrConst RIP.CInt
d =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_f5892dac29d00ba8)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_e@
foreign import ccall unsafe "hs_bindgen_cdf2cbe09fc53047" hs_bindgen_cdf2cbe09fc53047_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_e@
hs_bindgen_cdf2cbe09fc53047 :: IO (RIP.Ptr (PtrConst.PtrConst (PtrConst.PtrConst RIP.CInt)))
hs_bindgen_cdf2cbe09fc53047 =
  RIP.fromFFIType hs_bindgen_cdf2cbe09fc53047_base

{-# NOINLINE e #-}
{-| __C declaration:__ @e@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 18:26@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
e :: RIP.Ptr (PtrConst.PtrConst (PtrConst.PtrConst RIP.CInt))
e = RIP.unsafePerformIO hs_bindgen_cdf2cbe09fc53047

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_f@
foreign import ccall unsafe "hs_bindgen_93ddb0dbf8706068" hs_bindgen_93ddb0dbf8706068_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_f@
hs_bindgen_93ddb0dbf8706068 :: IO (RIP.Ptr (RIP.Ptr (PtrConst.PtrConst (PtrConst.PtrConst RIP.CInt))))
hs_bindgen_93ddb0dbf8706068 =
  RIP.fromFFIType hs_bindgen_93ddb0dbf8706068_base

{-# NOINLINE f #-}
{-| __C declaration:__ @f@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 20:27@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
f :: RIP.Ptr (RIP.Ptr (PtrConst.PtrConst (PtrConst.PtrConst RIP.CInt)))
f = RIP.unsafePerformIO hs_bindgen_93ddb0dbf8706068

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_g@
foreign import ccall unsafe "hs_bindgen_e6c23ab326a206a8" hs_bindgen_e6c23ab326a206a8_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_g@
hs_bindgen_e6c23ab326a206a8 :: IO (RIP.Ptr (PtrConst.PtrConst (RIP.Ptr (PtrConst.PtrConst RIP.CInt))))
hs_bindgen_e6c23ab326a206a8 =
  RIP.fromFFIType hs_bindgen_e6c23ab326a206a8_base

{-# NOINLINE g #-}
{-| __C declaration:__ @g@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 22:27@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
g :: RIP.Ptr (PtrConst.PtrConst (RIP.Ptr (PtrConst.PtrConst RIP.CInt)))
g = RIP.unsafePerformIO hs_bindgen_e6c23ab326a206a8

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_h@
foreign import ccall unsafe "hs_bindgen_f23087f72d34068b" hs_bindgen_f23087f72d34068b_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_h@
hs_bindgen_f23087f72d34068b :: IO (PtrConst.PtrConst (PtrConst.PtrConst (PtrConst.PtrConst (PtrConst.PtrConst RIP.CInt))))
hs_bindgen_f23087f72d34068b =
  RIP.fromFFIType hs_bindgen_f23087f72d34068b_base

{-# NOINLINE hs_bindgen_2461912a034924cf #-}
{-| __C declaration:__ @h@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 24:39@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@

    __unique:__ @test_typesqualifierstype_qualifie_Example_h@
-}
hs_bindgen_2461912a034924cf :: PtrConst.PtrConst (PtrConst.PtrConst (PtrConst.PtrConst (PtrConst.PtrConst RIP.CInt)))
hs_bindgen_2461912a034924cf =
  RIP.unsafePerformIO hs_bindgen_f23087f72d34068b

{-# NOINLINE h #-}
h :: PtrConst.PtrConst (PtrConst.PtrConst (PtrConst.PtrConst RIP.CInt))
h =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_2461912a034924cf)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_i@
foreign import ccall unsafe "hs_bindgen_ed73ead5c58fe94a" hs_bindgen_ed73ead5c58fe94a_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_i@
hs_bindgen_ed73ead5c58fe94a :: IO (RIP.Ptr (PtrConst.PtrConst (RIP.Ptr (PtrConst.PtrConst (RIP.Ptr RIP.CInt)))))
hs_bindgen_ed73ead5c58fe94a =
  RIP.fromFFIType hs_bindgen_ed73ead5c58fe94a_base

{-# NOINLINE i #-}
{-| __C declaration:__ @i@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 26:28@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
i :: RIP.Ptr (PtrConst.PtrConst (RIP.Ptr (PtrConst.PtrConst (RIP.Ptr RIP.CInt))))
i = RIP.unsafePerformIO hs_bindgen_ed73ead5c58fe94a

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_j@
foreign import ccall unsafe "hs_bindgen_92d2adc70d4b87ee" hs_bindgen_92d2adc70d4b87ee_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_j@
hs_bindgen_92d2adc70d4b87ee :: IO (RIP.Ptr (RIP.Ptr (PtrConst.PtrConst (PtrConst.PtrConst (PtrConst.PtrConst RIP.CInt)))))
hs_bindgen_92d2adc70d4b87ee =
  RIP.fromFFIType hs_bindgen_92d2adc70d4b87ee_base

{-# NOINLINE j #-}
{-| __C declaration:__ @j@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 28:34@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
j :: RIP.Ptr (RIP.Ptr (PtrConst.PtrConst (PtrConst.PtrConst (PtrConst.PtrConst RIP.CInt))))
j = RIP.unsafePerformIO hs_bindgen_92d2adc70d4b87ee
