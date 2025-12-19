{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <attributes/visibility_attributes.h>"
  , "/* test_attributesvisibility_attribut_Example_get_f0 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f786cd0ff4765546 (void)) (void)"
  , "{"
  , "  return &f0;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e52f358f2c001dee (void)) (void)"
  , "{"
  , "  return &f1;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_56d04acba6cd0980 (void)) (void)"
  , "{"
  , "  return &f2;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f31a184d805499cf (void)) (void)"
  , "{"
  , "  return &f3;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5f7194191ffe8481 (void)) (void)"
  , "{"
  , "  return &f4;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_85bc4e11ab8b8648 (void)) (void)"
  , "{"
  , "  return &f5;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f6 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9996f6b2a5f115e0 (void)) (void)"
  , "{"
  , "  return &f6;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f7 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e492e14719d8bce9 (void)) (void)"
  , "{"
  , "  return &f7;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f8 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_dc46d9b5b58c3cec (void)) (void)"
  , "{"
  , "  return &f8;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f9 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d447735bb0d90bf0 (void)) (void)"
  , "{"
  , "  return &f9;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f10 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_038431a45380fdcb (void)) (void)"
  , "{"
  , "  return &f10;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f11 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7e591cbd13064a7e (void)) (void)"
  , "{"
  , "  return &f11;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f12 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b5295bfee1c2918d (void)) (void)"
  , "{"
  , "  return &f12;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f13 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_64f5e3ad765f7112 (void)) (void)"
  , "{"
  , "  return &f13;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f14 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6b5ac9f59213d283 (void)) (void)"
  , "{"
  , "  return &f14;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f15 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_41661225569065f2 (void)) (void)"
  , "{"
  , "  return &f15;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f16 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cb3fb392b3793519 (void)) (void)"
  , "{"
  , "  return &f16;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f17 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7ec364e12fc11702 (void)) (void)"
  , "{"
  , "  return &f17;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f18 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_026a0814b5f6b1e9 (void)) (void)"
  , "{"
  , "  return &f18;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f19 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_505d79f059ad6189 (void)) (void)"
  , "{"
  , "  return &f19;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f20 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a939d9a54db20e26 (void)) (void)"
  , "{"
  , "  return &f20;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f21 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_970806e1a858d637 (void)) (void)"
  , "{"
  , "  return &f21;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f22 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_da0c0173778e44e3 (void)) (void)"
  , "{"
  , "  return &f22;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f23 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ebb72e65cbbf7f2c (void)) (void)"
  , "{"
  , "  return &f23;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f24 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8615cd6d2c1b5c26 (void)) (void)"
  , "{"
  , "  return &f24;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f25 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3546f015679f9021 (void)) (void)"
  , "{"
  , "  return &f25;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f26 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_98a7f744d6dfd7ed (void)) (void)"
  , "{"
  , "  return &f26;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f27 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b5c5959f7fb7f0ce (void)) (void)"
  , "{"
  , "  return &f27;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f28 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_19fb69eb83bce767 (void)) (void)"
  , "{"
  , "  return &f28;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f29 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b10ef2e4d50e78fe (void)) (void)"
  , "{"
  , "  return &f29;"
  , "}"
  ]))

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f0@
foreign import ccall unsafe "hs_bindgen_f786cd0ff4765546" hs_bindgen_f786cd0ff4765546 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f0 #-}

{-| __C declaration:__ @f0@

    __defined at:__ @attributes\/visibility_attributes.h:17:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f0 :: Ptr.FunPtr (IO ())
f0 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f786cd0ff4765546

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f1@
foreign import ccall unsafe "hs_bindgen_e52f358f2c001dee" hs_bindgen_e52f358f2c001dee ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f1 #-}

{-| __C declaration:__ @f1@

    __defined at:__ @attributes\/visibility_attributes.h:18:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f1 :: Ptr.FunPtr (IO ())
f1 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e52f358f2c001dee

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f2@
foreign import ccall unsafe "hs_bindgen_56d04acba6cd0980" hs_bindgen_56d04acba6cd0980 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f2 #-}

{-| __C declaration:__ @f2@

    __defined at:__ @attributes\/visibility_attributes.h:19:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f2 :: Ptr.FunPtr (IO ())
f2 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_56d04acba6cd0980

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f3@
foreign import ccall unsafe "hs_bindgen_f31a184d805499cf" hs_bindgen_f31a184d805499cf ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f3 #-}

{-| __C declaration:__ @f3@

    __defined at:__ @attributes\/visibility_attributes.h:20:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f3 :: Ptr.FunPtr (IO ())
f3 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f31a184d805499cf

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f4@
foreign import ccall unsafe "hs_bindgen_5f7194191ffe8481" hs_bindgen_5f7194191ffe8481 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f4 #-}

{-| __C declaration:__ @f4@

    __defined at:__ @attributes\/visibility_attributes.h:21:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f4 :: Ptr.FunPtr (IO ())
f4 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5f7194191ffe8481

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f5@
foreign import ccall unsafe "hs_bindgen_85bc4e11ab8b8648" hs_bindgen_85bc4e11ab8b8648 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f5 #-}

{-| __C declaration:__ @f5@

    __defined at:__ @attributes\/visibility_attributes.h:24:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f5 :: Ptr.FunPtr (IO ())
f5 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_85bc4e11ab8b8648

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f6@
foreign import ccall unsafe "hs_bindgen_9996f6b2a5f115e0" hs_bindgen_9996f6b2a5f115e0 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f6 #-}

{-| __C declaration:__ @f6@

    __defined at:__ @attributes\/visibility_attributes.h:25:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f6 :: Ptr.FunPtr (IO ())
f6 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9996f6b2a5f115e0

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f7@
foreign import ccall unsafe "hs_bindgen_e492e14719d8bce9" hs_bindgen_e492e14719d8bce9 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f7 #-}

{-| __C declaration:__ @f7@

    __defined at:__ @attributes\/visibility_attributes.h:26:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f7 :: Ptr.FunPtr (IO ())
f7 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e492e14719d8bce9

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f8@
foreign import ccall unsafe "hs_bindgen_dc46d9b5b58c3cec" hs_bindgen_dc46d9b5b58c3cec ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f8 #-}

{-| __C declaration:__ @f8@

    __defined at:__ @attributes\/visibility_attributes.h:27:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f8 :: Ptr.FunPtr (IO ())
f8 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dc46d9b5b58c3cec

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f9@
foreign import ccall unsafe "hs_bindgen_d447735bb0d90bf0" hs_bindgen_d447735bb0d90bf0 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f9 #-}

{-| __C declaration:__ @f9@

    __defined at:__ @attributes\/visibility_attributes.h:28:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f9 :: Ptr.FunPtr (IO ())
f9 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d447735bb0d90bf0

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f10@
foreign import ccall unsafe "hs_bindgen_038431a45380fdcb" hs_bindgen_038431a45380fdcb ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f10 #-}

{-| __C declaration:__ @f10@

    __defined at:__ @attributes\/visibility_attributes.h:31:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f10 :: Ptr.FunPtr (IO ())
f10 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_038431a45380fdcb

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f11@
foreign import ccall unsafe "hs_bindgen_7e591cbd13064a7e" hs_bindgen_7e591cbd13064a7e ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f11 #-}

{-| __C declaration:__ @f11@

    __defined at:__ @attributes\/visibility_attributes.h:32:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f11 :: Ptr.FunPtr (IO ())
f11 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7e591cbd13064a7e

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f12@
foreign import ccall unsafe "hs_bindgen_b5295bfee1c2918d" hs_bindgen_b5295bfee1c2918d ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f12 #-}

{-| __C declaration:__ @f12@

    __defined at:__ @attributes\/visibility_attributes.h:33:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f12 :: Ptr.FunPtr (IO ())
f12 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b5295bfee1c2918d

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f13@
foreign import ccall unsafe "hs_bindgen_64f5e3ad765f7112" hs_bindgen_64f5e3ad765f7112 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f13 #-}

{-| __C declaration:__ @f13@

    __defined at:__ @attributes\/visibility_attributes.h:34:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f13 :: Ptr.FunPtr (IO ())
f13 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_64f5e3ad765f7112

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f14@
foreign import ccall unsafe "hs_bindgen_6b5ac9f59213d283" hs_bindgen_6b5ac9f59213d283 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f14 #-}

{-| __C declaration:__ @f14@

    __defined at:__ @attributes\/visibility_attributes.h:35:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f14 :: Ptr.FunPtr (IO ())
f14 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6b5ac9f59213d283

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f15@
foreign import ccall unsafe "hs_bindgen_41661225569065f2" hs_bindgen_41661225569065f2 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f15 #-}

{-| __C declaration:__ @f15@

    __defined at:__ @attributes\/visibility_attributes.h:38:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f15 :: Ptr.FunPtr (IO ())
f15 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_41661225569065f2

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f16@
foreign import ccall unsafe "hs_bindgen_cb3fb392b3793519" hs_bindgen_cb3fb392b3793519 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f16 #-}

{-| __C declaration:__ @f16@

    __defined at:__ @attributes\/visibility_attributes.h:39:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f16 :: Ptr.FunPtr (IO ())
f16 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cb3fb392b3793519

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f17@
foreign import ccall unsafe "hs_bindgen_7ec364e12fc11702" hs_bindgen_7ec364e12fc11702 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f17 #-}

{-| __C declaration:__ @f17@

    __defined at:__ @attributes\/visibility_attributes.h:40:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f17 :: Ptr.FunPtr (IO ())
f17 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7ec364e12fc11702

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f18@
foreign import ccall unsafe "hs_bindgen_026a0814b5f6b1e9" hs_bindgen_026a0814b5f6b1e9 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f18 #-}

{-| __C declaration:__ @f18@

    __defined at:__ @attributes\/visibility_attributes.h:41:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f18 :: Ptr.FunPtr (IO ())
f18 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_026a0814b5f6b1e9

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f19@
foreign import ccall unsafe "hs_bindgen_505d79f059ad6189" hs_bindgen_505d79f059ad6189 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f19 #-}

{-| __C declaration:__ @f19@

    __defined at:__ @attributes\/visibility_attributes.h:42:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f19 :: Ptr.FunPtr (IO ())
f19 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_505d79f059ad6189

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f20@
foreign import ccall unsafe "hs_bindgen_a939d9a54db20e26" hs_bindgen_a939d9a54db20e26 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f20 #-}

{-| __C declaration:__ @f20@

    __defined at:__ @attributes\/visibility_attributes.h:45:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f20 :: Ptr.FunPtr (IO ())
f20 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a939d9a54db20e26

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f21@
foreign import ccall unsafe "hs_bindgen_970806e1a858d637" hs_bindgen_970806e1a858d637 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f21 #-}

{-| __C declaration:__ @f21@

    __defined at:__ @attributes\/visibility_attributes.h:46:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f21 :: Ptr.FunPtr (IO ())
f21 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_970806e1a858d637

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f22@
foreign import ccall unsafe "hs_bindgen_da0c0173778e44e3" hs_bindgen_da0c0173778e44e3 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f22 #-}

{-| __C declaration:__ @f22@

    __defined at:__ @attributes\/visibility_attributes.h:47:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f22 :: Ptr.FunPtr (IO ())
f22 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_da0c0173778e44e3

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f23@
foreign import ccall unsafe "hs_bindgen_ebb72e65cbbf7f2c" hs_bindgen_ebb72e65cbbf7f2c ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f23 #-}

{-| __C declaration:__ @f23@

    __defined at:__ @attributes\/visibility_attributes.h:48:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f23 :: Ptr.FunPtr (IO ())
f23 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ebb72e65cbbf7f2c

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f24@
foreign import ccall unsafe "hs_bindgen_8615cd6d2c1b5c26" hs_bindgen_8615cd6d2c1b5c26 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f24 #-}

{-| __C declaration:__ @f24@

    __defined at:__ @attributes\/visibility_attributes.h:49:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f24 :: Ptr.FunPtr (IO ())
f24 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8615cd6d2c1b5c26

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f25@
foreign import ccall unsafe "hs_bindgen_3546f015679f9021" hs_bindgen_3546f015679f9021 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f25 #-}

{-| __C declaration:__ @f25@

    __defined at:__ @attributes\/visibility_attributes.h:52:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f25 :: Ptr.FunPtr (IO ())
f25 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3546f015679f9021

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f26@
foreign import ccall unsafe "hs_bindgen_98a7f744d6dfd7ed" hs_bindgen_98a7f744d6dfd7ed ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f26 #-}

{-| __C declaration:__ @f26@

    __defined at:__ @attributes\/visibility_attributes.h:53:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f26 :: Ptr.FunPtr (IO ())
f26 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_98a7f744d6dfd7ed

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f27@
foreign import ccall unsafe "hs_bindgen_b5c5959f7fb7f0ce" hs_bindgen_b5c5959f7fb7f0ce ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f27 #-}

{-| __C declaration:__ @f27@

    __defined at:__ @attributes\/visibility_attributes.h:54:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f27 :: Ptr.FunPtr (IO ())
f27 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b5c5959f7fb7f0ce

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f28@
foreign import ccall unsafe "hs_bindgen_19fb69eb83bce767" hs_bindgen_19fb69eb83bce767 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f28 #-}

{-| __C declaration:__ @f28@

    __defined at:__ @attributes\/visibility_attributes.h:55:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f28 :: Ptr.FunPtr (IO ())
f28 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_19fb69eb83bce767

-- __unique:__ @test_attributesvisibility_attribut_Example_get_f29@
foreign import ccall unsafe "hs_bindgen_b10ef2e4d50e78fe" hs_bindgen_b10ef2e4d50e78fe ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f29 #-}

{-| __C declaration:__ @f29@

    __defined at:__ @attributes\/visibility_attributes.h:56:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f29 :: Ptr.FunPtr (IO ())
f29 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b10ef2e4d50e78fe
