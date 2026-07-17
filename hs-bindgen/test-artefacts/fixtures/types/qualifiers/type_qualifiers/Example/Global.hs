{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.a
    , Example.Global.b
    , Example.Global.c
    , Example.Global.d
    , Example.Global.e
    , Example.Global.f
    , Example.Global.g
    , Example.Global.h
    , Example.Global.i
    , Example.Global.j
    )
  where

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_a@
hs_bindgen_b9f6c3995e03a64f :: IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_b9f6c3995e03a64f =
  BG.fromFFIType hs_bindgen_b9f6c3995e03a64f_base

{-# NOINLINE hs_bindgen_568187d7e5f899e0 #-}
{-| __C declaration:__ @a@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 5:18@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@

    __unique:__ @test_typesqualifierstype_qualifie_Example_a@
-}
hs_bindgen_568187d7e5f899e0 :: PtrConst.PtrConst BG.CInt
hs_bindgen_568187d7e5f899e0 =
  BG.unsafePerformIO hs_bindgen_b9f6c3995e03a64f

{-# NOINLINE a #-}
a :: BG.CInt
a =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_568187d7e5f899e0)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_b@
foreign import ccall unsafe "hs_bindgen_2a09e45ccb76e4da" hs_bindgen_2a09e45ccb76e4da_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_b@
hs_bindgen_2a09e45ccb76e4da :: IO (BG.Ptr (PtrConst.PtrConst BG.CInt))
hs_bindgen_2a09e45ccb76e4da =
  BG.fromFFIType hs_bindgen_2a09e45ccb76e4da_base

{-# NOINLINE b #-}
{-| __C declaration:__ @b@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 7:19@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
b :: BG.Ptr (PtrConst.PtrConst BG.CInt)
b = BG.unsafePerformIO hs_bindgen_2a09e45ccb76e4da

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_c@
foreign import ccall unsafe "hs_bindgen_9cef5885d51a5077" hs_bindgen_9cef5885d51a5077_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_c@
hs_bindgen_9cef5885d51a5077 :: IO (PtrConst.PtrConst (BG.Ptr BG.CInt))
hs_bindgen_9cef5885d51a5077 =
  BG.fromFFIType hs_bindgen_9cef5885d51a5077_base

{-# NOINLINE hs_bindgen_86ab1f1ce8db256e #-}
{-| __C declaration:__ @c@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 9:19@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@

    __unique:__ @test_typesqualifierstype_qualifie_Example_c@
-}
hs_bindgen_86ab1f1ce8db256e :: PtrConst.PtrConst (BG.Ptr BG.CInt)
hs_bindgen_86ab1f1ce8db256e =
  BG.unsafePerformIO hs_bindgen_9cef5885d51a5077

{-# NOINLINE c #-}
c :: BG.Ptr BG.CInt
c =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_86ab1f1ce8db256e)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_d@
foreign import ccall unsafe "hs_bindgen_039fc3584df4cf95" hs_bindgen_039fc3584df4cf95_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_d@
hs_bindgen_039fc3584df4cf95 :: IO (PtrConst.PtrConst (PtrConst.PtrConst BG.CInt))
hs_bindgen_039fc3584df4cf95 =
  BG.fromFFIType hs_bindgen_039fc3584df4cf95_base

{-# NOINLINE hs_bindgen_f5892dac29d00ba8 #-}
{-| __C declaration:__ @d@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 11:25@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@

    __unique:__ @test_typesqualifierstype_qualifie_Example_d@
-}
hs_bindgen_f5892dac29d00ba8 :: PtrConst.PtrConst (PtrConst.PtrConst BG.CInt)
hs_bindgen_f5892dac29d00ba8 =
  BG.unsafePerformIO hs_bindgen_039fc3584df4cf95

{-# NOINLINE d #-}
d :: PtrConst.PtrConst BG.CInt
d =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_f5892dac29d00ba8)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_e@
foreign import ccall unsafe "hs_bindgen_cdf2cbe09fc53047" hs_bindgen_cdf2cbe09fc53047_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_e@
hs_bindgen_cdf2cbe09fc53047 :: IO (BG.Ptr (PtrConst.PtrConst (PtrConst.PtrConst BG.CInt)))
hs_bindgen_cdf2cbe09fc53047 =
  BG.fromFFIType hs_bindgen_cdf2cbe09fc53047_base

{-# NOINLINE e #-}
{-| __C declaration:__ @e@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 18:26@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
e :: BG.Ptr (PtrConst.PtrConst (PtrConst.PtrConst BG.CInt))
e = BG.unsafePerformIO hs_bindgen_cdf2cbe09fc53047

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_f@
foreign import ccall unsafe "hs_bindgen_93ddb0dbf8706068" hs_bindgen_93ddb0dbf8706068_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_f@
hs_bindgen_93ddb0dbf8706068 :: IO (BG.Ptr (BG.Ptr (PtrConst.PtrConst (PtrConst.PtrConst BG.CInt))))
hs_bindgen_93ddb0dbf8706068 =
  BG.fromFFIType hs_bindgen_93ddb0dbf8706068_base

{-# NOINLINE f #-}
{-| __C declaration:__ @f@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 20:27@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
f :: BG.Ptr (BG.Ptr (PtrConst.PtrConst (PtrConst.PtrConst BG.CInt)))
f = BG.unsafePerformIO hs_bindgen_93ddb0dbf8706068

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_g@
foreign import ccall unsafe "hs_bindgen_e6c23ab326a206a8" hs_bindgen_e6c23ab326a206a8_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_g@
hs_bindgen_e6c23ab326a206a8 :: IO (BG.Ptr (PtrConst.PtrConst (BG.Ptr (PtrConst.PtrConst BG.CInt))))
hs_bindgen_e6c23ab326a206a8 =
  BG.fromFFIType hs_bindgen_e6c23ab326a206a8_base

{-# NOINLINE g #-}
{-| __C declaration:__ @g@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 22:27@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
g :: BG.Ptr (PtrConst.PtrConst (BG.Ptr (PtrConst.PtrConst BG.CInt)))
g = BG.unsafePerformIO hs_bindgen_e6c23ab326a206a8

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_h@
foreign import ccall unsafe "hs_bindgen_f23087f72d34068b" hs_bindgen_f23087f72d34068b_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_h@
hs_bindgen_f23087f72d34068b :: IO (PtrConst.PtrConst (PtrConst.PtrConst (PtrConst.PtrConst (PtrConst.PtrConst BG.CInt))))
hs_bindgen_f23087f72d34068b =
  BG.fromFFIType hs_bindgen_f23087f72d34068b_base

{-# NOINLINE hs_bindgen_2461912a034924cf #-}
{-| __C declaration:__ @h@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 24:39@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@

    __unique:__ @test_typesqualifierstype_qualifie_Example_h@
-}
hs_bindgen_2461912a034924cf :: PtrConst.PtrConst (PtrConst.PtrConst (PtrConst.PtrConst (PtrConst.PtrConst BG.CInt)))
hs_bindgen_2461912a034924cf =
  BG.unsafePerformIO hs_bindgen_f23087f72d34068b

{-# NOINLINE h #-}
h :: PtrConst.PtrConst (PtrConst.PtrConst (PtrConst.PtrConst BG.CInt))
h =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_2461912a034924cf)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_i@
foreign import ccall unsafe "hs_bindgen_ed73ead5c58fe94a" hs_bindgen_ed73ead5c58fe94a_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_i@
hs_bindgen_ed73ead5c58fe94a :: IO (BG.Ptr (PtrConst.PtrConst (BG.Ptr (PtrConst.PtrConst (BG.Ptr BG.CInt)))))
hs_bindgen_ed73ead5c58fe94a =
  BG.fromFFIType hs_bindgen_ed73ead5c58fe94a_base

{-# NOINLINE i #-}
{-| __C declaration:__ @i@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 26:28@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
i :: BG.Ptr (PtrConst.PtrConst (BG.Ptr (PtrConst.PtrConst (BG.Ptr BG.CInt))))
i = BG.unsafePerformIO hs_bindgen_ed73ead5c58fe94a

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_j@
foreign import ccall unsafe "hs_bindgen_92d2adc70d4b87ee" hs_bindgen_92d2adc70d4b87ee_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typesqualifierstype_qualifie_Example_get_j@
hs_bindgen_92d2adc70d4b87ee :: IO (BG.Ptr (BG.Ptr (PtrConst.PtrConst (PtrConst.PtrConst (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_92d2adc70d4b87ee =
  BG.fromFFIType hs_bindgen_92d2adc70d4b87ee_base

{-# NOINLINE j #-}
{-| __C declaration:__ @j@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h 28:34@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
j :: BG.Ptr (BG.Ptr (PtrConst.PtrConst (PtrConst.PtrConst (PtrConst.PtrConst BG.CInt))))
j = BG.unsafePerformIO hs_bindgen_92d2adc70d4b87ee
