{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign as F
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.HasFFIType
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <types/qualifiers/const_typedefs.h>"
  , "/* test_typesqualifiersconst_typedef_Example_get_i */"
  , "__attribute__ ((const))"
  , "I const *hs_bindgen_bb40525ba9109d7a (void)"
  , "{"
  , "  return &i;"
  , "}"
  , "/* test_typesqualifiersconst_typedef_Example_get_s */"
  , "__attribute__ ((const))"
  , "struct S const *hs_bindgen_e02c667254de325e (void)"
  , "{"
  , "  return &s;"
  , "}"
  , "/* test_typesqualifiersconst_typedef_Example_get_u */"
  , "__attribute__ ((const))"
  , "union U const *hs_bindgen_db442d4a677f346c (void)"
  , "{"
  , "  return &u;"
  , "}"
  , "/* test_typesqualifiersconst_typedef_Example_get_e */"
  , "__attribute__ ((const))"
  , "enum E const *hs_bindgen_f8e0ac7d2c94db95 (void)"
  , "{"
  , "  return &e;"
  , "}"
  , "/* test_typesqualifiersconst_typedef_Example_get_ti */"
  , "__attribute__ ((const))"
  , "TI *hs_bindgen_da8e11a66011da3a (void)"
  , "{"
  , "  return &ti;"
  , "}"
  , "/* test_typesqualifiersconst_typedef_Example_get_ts */"
  , "__attribute__ ((const))"
  , "TS *hs_bindgen_10a3457a0f6b2036 (void)"
  , "{"
  , "  return &ts;"
  , "}"
  , "/* test_typesqualifiersconst_typedef_Example_get_tu */"
  , "__attribute__ ((const))"
  , "TU *hs_bindgen_ded2bbcd5f7b0513 (void)"
  , "{"
  , "  return &tu;"
  , "}"
  , "/* test_typesqualifiersconst_typedef_Example_get_te */"
  , "__attribute__ ((const))"
  , "TE *hs_bindgen_1b4b0fa82bacf9cc (void)"
  , "{"
  , "  return &te;"
  , "}"
  , "/* test_typesqualifiersconst_typedef_Example_get_tti */"
  , "__attribute__ ((const))"
  , "TTI *hs_bindgen_b8067bbfe8dc188e (void)"
  , "{"
  , "  return &tti;"
  , "}"
  , "/* test_typesqualifiersconst_typedef_Example_get_tts */"
  , "__attribute__ ((const))"
  , "TTS *hs_bindgen_6ce6ca09ed64a420 (void)"
  , "{"
  , "  return &tts;"
  , "}"
  , "/* test_typesqualifiersconst_typedef_Example_get_ttu */"
  , "__attribute__ ((const))"
  , "TTU *hs_bindgen_5d4569e6c5a642c0 (void)"
  , "{"
  , "  return &ttu;"
  , "}"
  , "/* test_typesqualifiersconst_typedef_Example_get_tte */"
  , "__attribute__ ((const))"
  , "TTE *hs_bindgen_cb44d6db796abc21 (void)"
  , "{"
  , "  return &tte;"
  , "}"
  ]))

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_i@
foreign import ccall unsafe "hs_bindgen_bb40525ba9109d7a" hs_bindgen_bb40525ba9109d7a_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_i@
hs_bindgen_bb40525ba9109d7a :: IO (HsBindgen.Runtime.ConstPtr.ConstPtr I)
hs_bindgen_bb40525ba9109d7a =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_bb40525ba9109d7a_base

{-# NOINLINE hs_bindgen_8bf2d89ca41997fb #-}
{-| __C declaration:__ @i@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 15:16@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@

    __unique:__ @test_typesqualifiersconst_typedef_Example_i@
-}
hs_bindgen_8bf2d89ca41997fb :: HsBindgen.Runtime.ConstPtr.ConstPtr I
hs_bindgen_8bf2d89ca41997fb =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bb40525ba9109d7a

{-# NOINLINE i #-}
i :: I
i =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_8bf2d89ca41997fb))

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_s@
foreign import ccall unsafe "hs_bindgen_e02c667254de325e" hs_bindgen_e02c667254de325e_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_s@
hs_bindgen_e02c667254de325e :: IO (HsBindgen.Runtime.ConstPtr.ConstPtr S)
hs_bindgen_e02c667254de325e =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_e02c667254de325e_base

{-# NOINLINE hs_bindgen_0e876abf0fe9e3b2 #-}
{-| __C declaration:__ @s@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 16:23@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@

    __unique:__ @test_typesqualifiersconst_typedef_Example_s@
-}
hs_bindgen_0e876abf0fe9e3b2 :: HsBindgen.Runtime.ConstPtr.ConstPtr S
hs_bindgen_0e876abf0fe9e3b2 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e02c667254de325e

{-# NOINLINE s #-}
s :: S
s =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_0e876abf0fe9e3b2))

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_u@
foreign import ccall unsafe "hs_bindgen_db442d4a677f346c" hs_bindgen_db442d4a677f346c_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_u@
hs_bindgen_db442d4a677f346c :: IO (HsBindgen.Runtime.ConstPtr.ConstPtr U)
hs_bindgen_db442d4a677f346c =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_db442d4a677f346c_base

{-# NOINLINE hs_bindgen_b7bdf87ddb6b9a5b #-}
{-| __C declaration:__ @u@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 17:22@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@

    __unique:__ @test_typesqualifiersconst_typedef_Example_u@
-}
hs_bindgen_b7bdf87ddb6b9a5b :: HsBindgen.Runtime.ConstPtr.ConstPtr U
hs_bindgen_b7bdf87ddb6b9a5b =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_db442d4a677f346c

{-# NOINLINE u #-}
u :: U
u =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_b7bdf87ddb6b9a5b))

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_e@
foreign import ccall unsafe "hs_bindgen_f8e0ac7d2c94db95" hs_bindgen_f8e0ac7d2c94db95_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_e@
hs_bindgen_f8e0ac7d2c94db95 :: IO (HsBindgen.Runtime.ConstPtr.ConstPtr E)
hs_bindgen_f8e0ac7d2c94db95 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_f8e0ac7d2c94db95_base

{-# NOINLINE hs_bindgen_fa2424d094d91a33 #-}
{-| __C declaration:__ @e@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 18:21@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@

    __unique:__ @test_typesqualifiersconst_typedef_Example_e@
-}
hs_bindgen_fa2424d094d91a33 :: HsBindgen.Runtime.ConstPtr.ConstPtr E
hs_bindgen_fa2424d094d91a33 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f8e0ac7d2c94db95

{-# NOINLINE e #-}
e :: E
e =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_fa2424d094d91a33))

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_ti@
foreign import ccall unsafe "hs_bindgen_da8e11a66011da3a" hs_bindgen_da8e11a66011da3a_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_ti@
hs_bindgen_da8e11a66011da3a :: IO (HsBindgen.Runtime.ConstPtr.ConstPtr TI)
hs_bindgen_da8e11a66011da3a =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_da8e11a66011da3a_base

{-# NOINLINE hs_bindgen_d7dd4362406f73a6 #-}
{-| __C declaration:__ @ti@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 25:11@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@

    __unique:__ @test_typesqualifiersconst_typedef_Example_ti@
-}
hs_bindgen_d7dd4362406f73a6 :: HsBindgen.Runtime.ConstPtr.ConstPtr TI
hs_bindgen_d7dd4362406f73a6 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_da8e11a66011da3a

{-# NOINLINE ti #-}
ti :: TI
ti =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_d7dd4362406f73a6))

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_ts@
foreign import ccall unsafe "hs_bindgen_10a3457a0f6b2036" hs_bindgen_10a3457a0f6b2036_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_ts@
hs_bindgen_10a3457a0f6b2036 :: IO (HsBindgen.Runtime.ConstPtr.ConstPtr TS)
hs_bindgen_10a3457a0f6b2036 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_10a3457a0f6b2036_base

{-# NOINLINE hs_bindgen_cd3e2d14ae82b6fc #-}
{-| __C declaration:__ @ts@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 26:11@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@

    __unique:__ @test_typesqualifiersconst_typedef_Example_ts@
-}
hs_bindgen_cd3e2d14ae82b6fc :: HsBindgen.Runtime.ConstPtr.ConstPtr TS
hs_bindgen_cd3e2d14ae82b6fc =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_10a3457a0f6b2036

{-# NOINLINE ts #-}
ts :: TS
ts =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_cd3e2d14ae82b6fc))

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_tu@
foreign import ccall unsafe "hs_bindgen_ded2bbcd5f7b0513" hs_bindgen_ded2bbcd5f7b0513_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_tu@
hs_bindgen_ded2bbcd5f7b0513 :: IO (HsBindgen.Runtime.ConstPtr.ConstPtr TU)
hs_bindgen_ded2bbcd5f7b0513 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_ded2bbcd5f7b0513_base

{-# NOINLINE hs_bindgen_2d9684fc851b0766 #-}
{-| __C declaration:__ @tu@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 27:11@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@

    __unique:__ @test_typesqualifiersconst_typedef_Example_tu@
-}
hs_bindgen_2d9684fc851b0766 :: HsBindgen.Runtime.ConstPtr.ConstPtr TU
hs_bindgen_2d9684fc851b0766 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ded2bbcd5f7b0513

{-# NOINLINE tu #-}
tu :: TU
tu =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_2d9684fc851b0766))

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_te@
foreign import ccall unsafe "hs_bindgen_1b4b0fa82bacf9cc" hs_bindgen_1b4b0fa82bacf9cc_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_te@
hs_bindgen_1b4b0fa82bacf9cc :: IO (HsBindgen.Runtime.ConstPtr.ConstPtr TE)
hs_bindgen_1b4b0fa82bacf9cc =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_1b4b0fa82bacf9cc_base

{-# NOINLINE hs_bindgen_200eaed7c3fb420d #-}
{-| __C declaration:__ @te@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 28:11@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@

    __unique:__ @test_typesqualifiersconst_typedef_Example_te@
-}
hs_bindgen_200eaed7c3fb420d :: HsBindgen.Runtime.ConstPtr.ConstPtr TE
hs_bindgen_200eaed7c3fb420d =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1b4b0fa82bacf9cc

{-# NOINLINE te #-}
te :: TE
te =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_200eaed7c3fb420d))

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_tti@
foreign import ccall unsafe "hs_bindgen_b8067bbfe8dc188e" hs_bindgen_b8067bbfe8dc188e_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_tti@
hs_bindgen_b8067bbfe8dc188e :: IO (HsBindgen.Runtime.ConstPtr.ConstPtr TTI)
hs_bindgen_b8067bbfe8dc188e =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_b8067bbfe8dc188e_base

{-# NOINLINE hs_bindgen_1fbea465b2793d07 #-}
{-| __C declaration:__ @tti@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 35:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@

    __unique:__ @test_typesqualifiersconst_typedef_Example_tti@
-}
hs_bindgen_1fbea465b2793d07 :: HsBindgen.Runtime.ConstPtr.ConstPtr TTI
hs_bindgen_1fbea465b2793d07 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b8067bbfe8dc188e

{-# NOINLINE tti #-}
tti :: TTI
tti =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_1fbea465b2793d07))

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_tts@
foreign import ccall unsafe "hs_bindgen_6ce6ca09ed64a420" hs_bindgen_6ce6ca09ed64a420_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_tts@
hs_bindgen_6ce6ca09ed64a420 :: IO (HsBindgen.Runtime.ConstPtr.ConstPtr TTS)
hs_bindgen_6ce6ca09ed64a420 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_6ce6ca09ed64a420_base

{-# NOINLINE hs_bindgen_f23f17df17b3fe36 #-}
{-| __C declaration:__ @tts@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 36:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@

    __unique:__ @test_typesqualifiersconst_typedef_Example_tts@
-}
hs_bindgen_f23f17df17b3fe36 :: HsBindgen.Runtime.ConstPtr.ConstPtr TTS
hs_bindgen_f23f17df17b3fe36 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6ce6ca09ed64a420

{-# NOINLINE tts #-}
tts :: TTS
tts =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_f23f17df17b3fe36))

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_ttu@
foreign import ccall unsafe "hs_bindgen_5d4569e6c5a642c0" hs_bindgen_5d4569e6c5a642c0_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_ttu@
hs_bindgen_5d4569e6c5a642c0 :: IO (HsBindgen.Runtime.ConstPtr.ConstPtr TTU)
hs_bindgen_5d4569e6c5a642c0 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_5d4569e6c5a642c0_base

{-# NOINLINE hs_bindgen_45ab2258853f641d #-}
{-| __C declaration:__ @ttu@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 37:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@

    __unique:__ @test_typesqualifiersconst_typedef_Example_ttu@
-}
hs_bindgen_45ab2258853f641d :: HsBindgen.Runtime.ConstPtr.ConstPtr TTU
hs_bindgen_45ab2258853f641d =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5d4569e6c5a642c0

{-# NOINLINE ttu #-}
ttu :: TTU
ttu =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_45ab2258853f641d))

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_tte@
foreign import ccall unsafe "hs_bindgen_cb44d6db796abc21" hs_bindgen_cb44d6db796abc21_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_tte@
hs_bindgen_cb44d6db796abc21 :: IO (HsBindgen.Runtime.ConstPtr.ConstPtr TTE)
hs_bindgen_cb44d6db796abc21 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_cb44d6db796abc21_base

{-# NOINLINE hs_bindgen_0956779e634e3dd5 #-}
{-| __C declaration:__ @tte@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 38:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@

    __unique:__ @test_typesqualifiersconst_typedef_Example_tte@
-}
hs_bindgen_0956779e634e3dd5 :: HsBindgen.Runtime.ConstPtr.ConstPtr TTE
hs_bindgen_0956779e634e3dd5 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cb44d6db796abc21

{-# NOINLINE tte #-}
tte :: TTE
tte =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_0956779e634e3dd5))
