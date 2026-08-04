{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.untaggedPoint
    , Example.Global.untaggedPair
    , Example.Global.untaggedEnum
    , Example.Global.untaggedEnumCoords
    , Example.Global.a
    , Example.Global.b
    , Example.Global.c
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <globals/untagged.h>"
  , "/* test_globalsuntagged_Example_get_untaggedPoint */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_d77490b2fc5f4d07 (void)"
  , "{"
  , "  return &untaggedPoint;"
  , "}"
  , "/* test_globalsuntagged_Example_get_untaggedPair */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_0c5d4010212a5c60 (void)"
  , "{"
  , "  return &untaggedPair;"
  , "}"
  , "/* test_globalsuntagged_Example_get_untaggedEnum */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_6913d0b9652bbf6f (void)"
  , "{"
  , "  return &untaggedEnum;"
  , "}"
  , "/* test_globalsuntagged_Example_get_untaggedEnumCoords */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_d7232446972b888a (void)"
  , "{"
  , "  return &untaggedEnumCoords;"
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

-- __unique:__ @test_globalsuntagged_Example_get_untaggedPoint@
foreign import ccall unsafe "hs_bindgen_d77490b2fc5f4d07" hs_bindgen_d77490b2fc5f4d07_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsuntagged_Example_get_untaggedPoint@
hs_bindgen_d77490b2fc5f4d07 :: IO (BG.Ptr UntaggedPoint)
hs_bindgen_d77490b2fc5f4d07 =
  BG.fromFFIType hs_bindgen_d77490b2fc5f4d07_base

{-# NOINLINE untaggedPoint #-}
{-| __C declaration:__ @untaggedPoint@

    __defined at:__ @globals\/untagged.h 12:26@

    __exported by:__ @globals\/untagged.h@
-}
untaggedPoint :: BG.Ptr UntaggedPoint
untaggedPoint =
  BG.unsafePerformIO hs_bindgen_d77490b2fc5f4d07

-- __unique:__ @test_globalsuntagged_Example_get_untaggedPair@
foreign import ccall unsafe "hs_bindgen_0c5d4010212a5c60" hs_bindgen_0c5d4010212a5c60_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsuntagged_Example_get_untaggedPair@
hs_bindgen_0c5d4010212a5c60 :: IO (BG.Ptr UntaggedPair)
hs_bindgen_0c5d4010212a5c60 =
  BG.fromFFIType hs_bindgen_0c5d4010212a5c60_base

{-# NOINLINE untaggedPair #-}
{-| __C declaration:__ @untaggedPair@

    __defined at:__ @globals\/untagged.h 14:26@

    __exported by:__ @globals\/untagged.h@
-}
untaggedPair :: BG.Ptr UntaggedPair
untaggedPair =
  BG.unsafePerformIO hs_bindgen_0c5d4010212a5c60

-- __unique:__ @test_globalsuntagged_Example_get_untaggedEnum@
foreign import ccall unsafe "hs_bindgen_6913d0b9652bbf6f" hs_bindgen_6913d0b9652bbf6f_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsuntagged_Example_get_untaggedEnum@
hs_bindgen_6913d0b9652bbf6f :: IO (BG.Ptr UntaggedEnum)
hs_bindgen_6913d0b9652bbf6f =
  BG.fromFFIType hs_bindgen_6913d0b9652bbf6f_base

{-# NOINLINE untaggedEnum #-}
{-| __C declaration:__ @untaggedEnum@

    __defined at:__ @globals\/untagged.h 16:31@

    __exported by:__ @globals\/untagged.h@
-}
untaggedEnum :: BG.Ptr UntaggedEnum
untaggedEnum =
  BG.unsafePerformIO hs_bindgen_6913d0b9652bbf6f

-- __unique:__ @test_globalsuntagged_Example_get_untaggedEnumCoords@
foreign import ccall unsafe "hs_bindgen_d7232446972b888a" hs_bindgen_d7232446972b888a_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsuntagged_Example_get_untaggedEnumCoords@
hs_bindgen_d7232446972b888a :: IO (BG.Ptr UntaggedEnumCoords)
hs_bindgen_d7232446972b888a =
  BG.fromFFIType hs_bindgen_d7232446972b888a_base

{-# NOINLINE untaggedEnumCoords #-}
{-| __C declaration:__ @untaggedEnumCoords@

    __defined at:__ @globals\/untagged.h 18:33@

    __exported by:__ @globals\/untagged.h@
-}
untaggedEnumCoords :: BG.Ptr UntaggedEnumCoords
untaggedEnumCoords =
  BG.unsafePerformIO hs_bindgen_d7232446972b888a

-- __unique:__ @test_globalsuntagged_Example_get_A@
foreign import ccall unsafe "hs_bindgen_9d7d1fc84d6966f1" hs_bindgen_9d7d1fc84d6966f1_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsuntagged_Example_get_A@
hs_bindgen_9d7d1fc84d6966f1 :: IO (BG.Ptr (CA.ConstantArray 17 A))
hs_bindgen_9d7d1fc84d6966f1 =
  BG.fromFFIType hs_bindgen_9d7d1fc84d6966f1_base

{-# NOINLINE a #-}
{-| __C declaration:__ @A@

    __defined at:__ @globals\/untagged.h 24:13@

    __exported by:__ @globals\/untagged.h@
-}
a :: BG.Ptr (CA.ConstantArray 17 A)
a = BG.unsafePerformIO hs_bindgen_9d7d1fc84d6966f1

-- __unique:__ @test_globalsuntagged_Example_get_B@
foreign import ccall unsafe "hs_bindgen_38f1407628b3330b" hs_bindgen_38f1407628b3330b_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsuntagged_Example_get_B@
hs_bindgen_38f1407628b3330b :: IO (PtrConst.PtrConst B)
hs_bindgen_38f1407628b3330b =
  BG.fromFFIType hs_bindgen_38f1407628b3330b_base

{-# NOINLINE hs_bindgen_c84e7bea962cf7bc #-}
{-| __C declaration:__ @B@

    __defined at:__ @globals\/untagged.h 27:24@

    __exported by:__ @globals\/untagged.h@

    __unique:__ @test_globalsuntagged_Example_b@
-}
hs_bindgen_c84e7bea962cf7bc :: PtrConst.PtrConst B
hs_bindgen_c84e7bea962cf7bc =
  BG.unsafePerformIO hs_bindgen_38f1407628b3330b

{-# NOINLINE b #-}
b :: B
b =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_c84e7bea962cf7bc)

-- __unique:__ @test_globalsuntagged_Example_get_C@
foreign import ccall unsafe "hs_bindgen_492f8d9f3437df29" hs_bindgen_492f8d9f3437df29_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsuntagged_Example_get_C@
hs_bindgen_492f8d9f3437df29 :: IO (BG.Ptr (PtrConst.PtrConst (IA.IncompleteArray (BG.Ptr C))))
hs_bindgen_492f8d9f3437df29 =
  BG.fromFFIType hs_bindgen_492f8d9f3437df29_base

{-# NOINLINE c #-}
{-| __C declaration:__ @C@

    __defined at:__ @globals\/untagged.h 30:29@

    __exported by:__ @globals\/untagged.h@
-}
c :: BG.Ptr (PtrConst.PtrConst (IA.IncompleteArray (BG.Ptr C)))
c = BG.unsafePerformIO hs_bindgen_492f8d9f3437df29
