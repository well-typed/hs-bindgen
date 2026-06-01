{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.anonPoint
    , Example.Global.anonPair
    , Example.Global.anonEnum
    , Example.Global.anonEnumCoords
    , Example.Global.a
    , Example.Global.b
    , Example.Global.c
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <globals/untagged.h>"
  , "/* test_globalsuntagged_Example_get_anonPoint */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_6629eeb3c2ffd60b (void)"
  , "{"
  , "  return &anonPoint;"
  , "}"
  , "/* test_globalsuntagged_Example_get_anonPair */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_c6a1653d90a26590 (void)"
  , "{"
  , "  return &anonPair;"
  , "}"
  , "/* test_globalsuntagged_Example_get_anonEnum */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_ebe8734607d1730b (void)"
  , "{"
  , "  return &anonEnum;"
  , "}"
  , "/* test_globalsuntagged_Example_get_anonEnumCoords */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_21250a483c9f203d (void)"
  , "{"
  , "  return &anonEnumCoords;"
  , "}"
  , "/* test_globalsuntagged_Example_get_A */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_9d7d1fc84d6966f1 (void)"
  , "{"
  , "  return &A;"
  , "}"
  , "/* test_globalsuntagged_Example_get_B */"
  , "__attribute__ ((const))"
  , "void const *hs_bindgen_38f1407628b3330b (void)"
  , "{"
  , "  return &B;"
  , "}"
  , "/* test_globalsuntagged_Example_get_C */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_492f8d9f3437df29 (void)"
  , "{"
  , "  return &C;"
  , "}"
  ]))

-- __unique:__ @test_globalsuntagged_Example_get_anonPoint@
foreign import ccall unsafe "hs_bindgen_6629eeb3c2ffd60b" hs_bindgen_6629eeb3c2ffd60b_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsuntagged_Example_get_anonPoint@
hs_bindgen_6629eeb3c2ffd60b :: IO (RIP.Ptr AnonPoint)
hs_bindgen_6629eeb3c2ffd60b =
  RIP.fromFFIType hs_bindgen_6629eeb3c2ffd60b_base

{-# NOINLINE anonPoint #-}
{-| __C declaration:__ @anonPoint@

    __defined at:__ @globals\/untagged.h 12:26@

    __exported by:__ @globals\/untagged.h@
-}
anonPoint :: RIP.Ptr AnonPoint
anonPoint =
  RIP.unsafePerformIO hs_bindgen_6629eeb3c2ffd60b

-- __unique:__ @test_globalsuntagged_Example_get_anonPair@
foreign import ccall unsafe "hs_bindgen_c6a1653d90a26590" hs_bindgen_c6a1653d90a26590_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsuntagged_Example_get_anonPair@
hs_bindgen_c6a1653d90a26590 :: IO (RIP.Ptr AnonPair)
hs_bindgen_c6a1653d90a26590 =
  RIP.fromFFIType hs_bindgen_c6a1653d90a26590_base

{-# NOINLINE anonPair #-}
{-| __C declaration:__ @anonPair@

    __defined at:__ @globals\/untagged.h 14:26@

    __exported by:__ @globals\/untagged.h@
-}
anonPair :: RIP.Ptr AnonPair
anonPair =
  RIP.unsafePerformIO hs_bindgen_c6a1653d90a26590

-- __unique:__ @test_globalsuntagged_Example_get_anonEnum@
foreign import ccall unsafe "hs_bindgen_ebe8734607d1730b" hs_bindgen_ebe8734607d1730b_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsuntagged_Example_get_anonEnum@
hs_bindgen_ebe8734607d1730b :: IO (RIP.Ptr AnonEnum)
hs_bindgen_ebe8734607d1730b =
  RIP.fromFFIType hs_bindgen_ebe8734607d1730b_base

{-# NOINLINE anonEnum #-}
{-| __C declaration:__ @anonEnum@

    __defined at:__ @globals\/untagged.h 16:31@

    __exported by:__ @globals\/untagged.h@
-}
anonEnum :: RIP.Ptr AnonEnum
anonEnum =
  RIP.unsafePerformIO hs_bindgen_ebe8734607d1730b

-- __unique:__ @test_globalsuntagged_Example_get_anonEnumCoords@
foreign import ccall unsafe "hs_bindgen_21250a483c9f203d" hs_bindgen_21250a483c9f203d_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsuntagged_Example_get_anonEnumCoords@
hs_bindgen_21250a483c9f203d :: IO (RIP.Ptr AnonEnumCoords)
hs_bindgen_21250a483c9f203d =
  RIP.fromFFIType hs_bindgen_21250a483c9f203d_base

{-# NOINLINE anonEnumCoords #-}
{-| __C declaration:__ @anonEnumCoords@

    __defined at:__ @globals\/untagged.h 18:33@

    __exported by:__ @globals\/untagged.h@
-}
anonEnumCoords :: RIP.Ptr AnonEnumCoords
anonEnumCoords =
  RIP.unsafePerformIO hs_bindgen_21250a483c9f203d

-- __unique:__ @test_globalsuntagged_Example_get_A@
foreign import ccall unsafe "hs_bindgen_9d7d1fc84d6966f1" hs_bindgen_9d7d1fc84d6966f1_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsuntagged_Example_get_A@
hs_bindgen_9d7d1fc84d6966f1 :: IO (RIP.Ptr (CA.ConstantArray 17 A))
hs_bindgen_9d7d1fc84d6966f1 =
  RIP.fromFFIType hs_bindgen_9d7d1fc84d6966f1_base

{-# NOINLINE a #-}
{-| __C declaration:__ @A@

    __defined at:__ @globals\/untagged.h 24:13@

    __exported by:__ @globals\/untagged.h@
-}
a :: RIP.Ptr (CA.ConstantArray 17 A)
a = RIP.unsafePerformIO hs_bindgen_9d7d1fc84d6966f1

-- __unique:__ @test_globalsuntagged_Example_get_B@
foreign import ccall unsafe "hs_bindgen_38f1407628b3330b" hs_bindgen_38f1407628b3330b_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsuntagged_Example_get_B@
hs_bindgen_38f1407628b3330b :: IO (PtrConst.PtrConst B)
hs_bindgen_38f1407628b3330b =
  RIP.fromFFIType hs_bindgen_38f1407628b3330b_base

{-# NOINLINE hs_bindgen_c84e7bea962cf7bc #-}
{-| __C declaration:__ @B@

    __defined at:__ @globals\/untagged.h 27:24@

    __exported by:__ @globals\/untagged.h@

    __unique:__ @test_globalsuntagged_Example_b@
-}
hs_bindgen_c84e7bea962cf7bc :: PtrConst.PtrConst B
hs_bindgen_c84e7bea962cf7bc =
  RIP.unsafePerformIO hs_bindgen_38f1407628b3330b

{-# NOINLINE b #-}
b :: B
b =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_c84e7bea962cf7bc)

-- __unique:__ @test_globalsuntagged_Example_get_C@
foreign import ccall unsafe "hs_bindgen_492f8d9f3437df29" hs_bindgen_492f8d9f3437df29_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsuntagged_Example_get_C@
hs_bindgen_492f8d9f3437df29 :: IO (RIP.Ptr (PtrConst.PtrConst (IA.IncompleteArray (RIP.Ptr C))))
hs_bindgen_492f8d9f3437df29 =
  RIP.fromFFIType hs_bindgen_492f8d9f3437df29_base

{-# NOINLINE c #-}
{-| __C declaration:__ @C@

    __defined at:__ @globals\/untagged.h 30:29@

    __exported by:__ @globals\/untagged.h@
-}
c :: RIP.Ptr (PtrConst.PtrConst (IA.IncompleteArray (RIP.Ptr C)))
c = RIP.unsafePerformIO hs_bindgen_492f8d9f3437df29
