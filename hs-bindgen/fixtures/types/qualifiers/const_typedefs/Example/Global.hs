{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign as F
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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
foreign import ccall unsafe "hs_bindgen_bb40525ba9109d7a" hs_bindgen_bb40525ba9109d7a ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr I)

{-# NOINLINE hs_bindgen_8bf2d89ca41997fb #-}
{-| __C declaration:__ @i@

    __defined at:__ @types\/qualifiers\/const_typedefs.h:15:16@

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
foreign import ccall unsafe "hs_bindgen_e02c667254de325e" hs_bindgen_e02c667254de325e ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr S)

{-# NOINLINE hs_bindgen_0e876abf0fe9e3b2 #-}
{-| __C declaration:__ @s@

    __defined at:__ @types\/qualifiers\/const_typedefs.h:16:23@

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
foreign import ccall unsafe "hs_bindgen_db442d4a677f346c" hs_bindgen_db442d4a677f346c ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr U)

{-# NOINLINE hs_bindgen_b7bdf87ddb6b9a5b #-}
{-| __C declaration:__ @u@

    __defined at:__ @types\/qualifiers\/const_typedefs.h:17:22@

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
foreign import ccall unsafe "hs_bindgen_f8e0ac7d2c94db95" hs_bindgen_f8e0ac7d2c94db95 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr E)

{-# NOINLINE hs_bindgen_fa2424d094d91a33 #-}
{-| __C declaration:__ @e@

    __defined at:__ @types\/qualifiers\/const_typedefs.h:18:21@

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
foreign import ccall unsafe "hs_bindgen_da8e11a66011da3a" hs_bindgen_da8e11a66011da3a ::
     IO (Ptr.Ptr TI)

{-# NOINLINE ti #-}
{-| __C declaration:__ @ti@

    __defined at:__ @types\/qualifiers\/const_typedefs.h:25:11@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
ti :: Ptr.Ptr TI
ti =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_da8e11a66011da3a

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_ts@
foreign import ccall unsafe "hs_bindgen_10a3457a0f6b2036" hs_bindgen_10a3457a0f6b2036 ::
     IO (Ptr.Ptr TS)

{-# NOINLINE ts #-}
{-| __C declaration:__ @ts@

    __defined at:__ @types\/qualifiers\/const_typedefs.h:26:11@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
ts :: Ptr.Ptr TS
ts =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_10a3457a0f6b2036

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_tu@
foreign import ccall unsafe "hs_bindgen_ded2bbcd5f7b0513" hs_bindgen_ded2bbcd5f7b0513 ::
     IO (Ptr.Ptr TU)

{-# NOINLINE tu #-}
{-| __C declaration:__ @tu@

    __defined at:__ @types\/qualifiers\/const_typedefs.h:27:11@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
tu :: Ptr.Ptr TU
tu =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ded2bbcd5f7b0513

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_te@
foreign import ccall unsafe "hs_bindgen_1b4b0fa82bacf9cc" hs_bindgen_1b4b0fa82bacf9cc ::
     IO (Ptr.Ptr TE)

{-# NOINLINE te #-}
{-| __C declaration:__ @te@

    __defined at:__ @types\/qualifiers\/const_typedefs.h:28:11@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
te :: Ptr.Ptr TE
te =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1b4b0fa82bacf9cc

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_tti@
foreign import ccall unsafe "hs_bindgen_b8067bbfe8dc188e" hs_bindgen_b8067bbfe8dc188e ::
     IO (Ptr.Ptr TTI)

{-# NOINLINE tti #-}
{-| __C declaration:__ @tti@

    __defined at:__ @types\/qualifiers\/const_typedefs.h:35:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
tti :: Ptr.Ptr TTI
tti =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b8067bbfe8dc188e

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_tts@
foreign import ccall unsafe "hs_bindgen_6ce6ca09ed64a420" hs_bindgen_6ce6ca09ed64a420 ::
     IO (Ptr.Ptr TTS)

{-# NOINLINE tts #-}
{-| __C declaration:__ @tts@

    __defined at:__ @types\/qualifiers\/const_typedefs.h:36:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
tts :: Ptr.Ptr TTS
tts =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6ce6ca09ed64a420

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_ttu@
foreign import ccall unsafe "hs_bindgen_5d4569e6c5a642c0" hs_bindgen_5d4569e6c5a642c0 ::
     IO (Ptr.Ptr TTU)

{-# NOINLINE ttu #-}
{-| __C declaration:__ @ttu@

    __defined at:__ @types\/qualifiers\/const_typedefs.h:37:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
ttu :: Ptr.Ptr TTU
ttu =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5d4569e6c5a642c0

-- __unique:__ @test_typesqualifiersconst_typedef_Example_get_tte@
foreign import ccall unsafe "hs_bindgen_cb44d6db796abc21" hs_bindgen_cb44d6db796abc21 ::
     IO (Ptr.Ptr TTE)

{-# NOINLINE tte #-}
{-| __C declaration:__ @tte@

    __defined at:__ @types\/qualifiers\/const_typedefs.h:38:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
tte :: Ptr.Ptr TTE
tte =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cb44d6db796abc21
