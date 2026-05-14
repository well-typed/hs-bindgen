{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo1
    , Example.FunPtr.foo2
    , Example.FunPtr.foo3
    , Example.FunPtr.foo4
    , Example.FunPtr.foo5
    , Example.FunPtr.foo6
    , Example.FunPtr.foo7
    , Example.FunPtr.foo8
    , Example.FunPtr.foo9
    , Example.FunPtr.foo10
    , Example.FunPtr.foo11
    , Example.FunPtr.foo12
    , Example.FunPtr.foo13
    , Example.FunPtr.foo14
    , Example.FunPtr.foo15
    , Example.FunPtr.foo16
    , Example.FunPtr.foo17
    , Example.FunPtr.foo18
    , Example.FunPtr.foo19
    , Example.FunPtr.foo20
    , Example.FunPtr.foo21
    , Example.FunPtr.foo22
    , Example.FunPtr.foo23
    , Example.FunPtr.foo24
    , Example.FunPtr.foo25
    , Example.FunPtr.foo26
    , Example.FunPtr.foo27
    , Example.FunPtr.foo28
    , Example.FunPtr.inline_foo
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <comprehensive/smoke.h>"
  , "/* test_comprehensivesmoke_Example_get_foo1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_abdd0c6dd73d0854 (void)) (void)"
  , "{"
  , "  return &foo1;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_95f5e8dc2a878cd6 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo2;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9970868a81b829db (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &foo3;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo4 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_c2fb72f24a5149fb (void)) (void)"
  , "{"
  , "  return &foo4;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo5 */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_3d952ee4c4ee378f (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo5;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo6 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_478897bd7cf090b7 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &foo6;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo7 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_2804b55bcc6495bd (void)) ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return &foo7;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo8 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_dc0ab953845109e0 (void)) ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return &foo8;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo9 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_6c6b48e80df5d80b (void)) ("
  , "  char *(*arg1) (void)"
  , ")"
  , "{"
  , "  return &foo9;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo10 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_fc0a56da1aa3e234 (void)) ("
  , "  char *(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo10;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo11 */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_b882a6f77db6bd0e (void)) ("
  , "  void *(*arg1) (void)"
  , ")"
  , "{"
  , "  return &foo11;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo12 */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_c219c7d6d459a7e0 (void)) ("
  , "  void *(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo12;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo13 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_caa14e75b1229a54 (void)) ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return &foo13;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo14 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_3cb093b66f7944d4 (void)) ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return &foo14;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo15 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_fe12aea8f26fdc44 (void)) ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return &foo15;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo16 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_7682897a88740d24 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo16;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo17 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a8ad868a6dc425f9 (void)) ("
  , "  char ***arg1"
  , ")"
  , "{"
  , "  return &foo17;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo18 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_245d12e0c60b976b (void)) ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return &foo18;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo19 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_81d64147225cc2ab (void)) ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return &foo19;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo20 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_b9d12fbbd2c70f4f (void)) ("
  , "  uint arg1"
  , ")"
  , "{"
  , "  return &foo20;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo21 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a3c85769041747ba (void)) ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo21;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo22 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_5fc122411b8a03ea (void)) ("
  , "  signed int *(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo22;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo23 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a7fcbb7f1b7f29cb (void)) ("
  , "  signed int **(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo23;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo24 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_9c5a2b0e6d53b7c9 (void)) ("
  , "  signed int ***(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo24;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo25 */"
  , "__attribute__ ((const))"
  , "signed int *(*hs_bindgen_5b576a9269d86a85 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo25;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo26 */"
  , "__attribute__ ((const))"
  , "signed int **(*hs_bindgen_3d968f750d7c0963 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo26;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo27 */"
  , "__attribute__ ((const))"
  , "signed int ***(*hs_bindgen_477d6debd15ec8cc (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo27;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_foo28 */"
  , "__attribute__ ((const))"
  , "signed int ***(*hs_bindgen_fe8d0bbb3e0e9949 (void)) ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return &foo28;"
  , "}"
  , "/* test_comprehensivesmoke_Example_get_inline_foo */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_3e48bdcc0c25203c (void)) ("
  , "  signed int arg1,"
  , "  signed int *arg2,"
  , "  signed int const arg3,"
  , "  signed int const *arg4,"
  , "  signed int const **arg5,"
  , "  signed int const *const *arg6,"
  , "  size_t arg7"
  , ")"
  , "{"
  , "  return &inline_foo;"
  , "}"
  ]))

-- __unique:__ @test_comprehensivesmoke_Example_get_foo1@
foreign import ccall unsafe "hs_bindgen_abdd0c6dd73d0854" hs_bindgen_abdd0c6dd73d0854_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo1@
hs_bindgen_abdd0c6dd73d0854 :: IO (RIP.FunPtr (IO ()))
hs_bindgen_abdd0c6dd73d0854 =
  RIP.fromFFIType hs_bindgen_abdd0c6dd73d0854_base

{-# NOINLINE foo1 #-}
{-| __C declaration:__ @foo1@

    __defined at:__ @comprehensive\/smoke.h 10:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo1 :: RIP.FunPtr (IO ())
foo1 =
  RIP.unsafePerformIO hs_bindgen_abdd0c6dd73d0854

-- __unique:__ @test_comprehensivesmoke_Example_get_foo2@
foreign import ccall unsafe "hs_bindgen_95f5e8dc2a878cd6" hs_bindgen_95f5e8dc2a878cd6_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo2@
hs_bindgen_95f5e8dc2a878cd6 :: IO (RIP.FunPtr (RIP.CInt -> IO ()))
hs_bindgen_95f5e8dc2a878cd6 =
  RIP.fromFFIType hs_bindgen_95f5e8dc2a878cd6_base

{-# NOINLINE foo2 #-}
{-| __C declaration:__ @foo2@

    __defined at:__ @comprehensive\/smoke.h 11:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo2 :: RIP.FunPtr (RIP.CInt -> IO ())
foo2 =
  RIP.unsafePerformIO hs_bindgen_95f5e8dc2a878cd6

-- __unique:__ @test_comprehensivesmoke_Example_get_foo3@
foreign import ccall unsafe "hs_bindgen_9970868a81b829db" hs_bindgen_9970868a81b829db_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo3@
hs_bindgen_9970868a81b829db :: IO (RIP.FunPtr (RIP.CInt -> RIP.CInt -> IO ()))
hs_bindgen_9970868a81b829db =
  RIP.fromFFIType hs_bindgen_9970868a81b829db_base

{-# NOINLINE foo3 #-}
{-| __C declaration:__ @foo3@

    __defined at:__ @comprehensive\/smoke.h 12:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo3 :: RIP.FunPtr (RIP.CInt -> RIP.CInt -> IO ())
foo3 =
  RIP.unsafePerformIO hs_bindgen_9970868a81b829db

-- __unique:__ @test_comprehensivesmoke_Example_get_foo4@
foreign import ccall unsafe "hs_bindgen_c2fb72f24a5149fb" hs_bindgen_c2fb72f24a5149fb_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo4@
hs_bindgen_c2fb72f24a5149fb :: IO (RIP.FunPtr (IO RIP.CInt))
hs_bindgen_c2fb72f24a5149fb =
  RIP.fromFFIType hs_bindgen_c2fb72f24a5149fb_base

{-# NOINLINE foo4 #-}
{-| __C declaration:__ @foo4@

    __defined at:__ @comprehensive\/smoke.h 13:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo4 :: RIP.FunPtr (IO RIP.CInt)
foo4 =
  RIP.unsafePerformIO hs_bindgen_c2fb72f24a5149fb

-- __unique:__ @test_comprehensivesmoke_Example_get_foo5@
foreign import ccall unsafe "hs_bindgen_3d952ee4c4ee378f" hs_bindgen_3d952ee4c4ee378f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo5@
hs_bindgen_3d952ee4c4ee378f :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CChar))
hs_bindgen_3d952ee4c4ee378f =
  RIP.fromFFIType hs_bindgen_3d952ee4c4ee378f_base

{-# NOINLINE foo5 #-}
{-| __C declaration:__ @foo5@

    __defined at:__ @comprehensive\/smoke.h 14:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo5 :: RIP.FunPtr (RIP.CInt -> IO RIP.CChar)
foo5 =
  RIP.unsafePerformIO hs_bindgen_3d952ee4c4ee378f

-- __unique:__ @test_comprehensivesmoke_Example_get_foo6@
foreign import ccall unsafe "hs_bindgen_478897bd7cf090b7" hs_bindgen_478897bd7cf090b7_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo6@
hs_bindgen_478897bd7cf090b7 :: IO (RIP.FunPtr (RIP.CInt -> RIP.CInt -> IO (RIP.Ptr RIP.CChar)))
hs_bindgen_478897bd7cf090b7 =
  RIP.fromFFIType hs_bindgen_478897bd7cf090b7_base

{-# NOINLINE foo6 #-}
{-| __C declaration:__ @foo6@

    __defined at:__ @comprehensive\/smoke.h 15:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo6 :: RIP.FunPtr (RIP.CInt -> RIP.CInt -> IO (RIP.Ptr RIP.CChar))
foo6 =
  RIP.unsafePerformIO hs_bindgen_478897bd7cf090b7

-- __unique:__ @test_comprehensivesmoke_Example_get_foo7@
foreign import ccall unsafe "hs_bindgen_2804b55bcc6495bd" hs_bindgen_2804b55bcc6495bd_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo7@
hs_bindgen_2804b55bcc6495bd :: IO (RIP.FunPtr (RIP.Ptr RIP.CChar -> IO (RIP.Ptr RIP.CChar)))
hs_bindgen_2804b55bcc6495bd =
  RIP.fromFFIType hs_bindgen_2804b55bcc6495bd_base

{-# NOINLINE foo7 #-}
{-| __C declaration:__ @foo7@

    __defined at:__ @comprehensive\/smoke.h 16:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo7 :: RIP.FunPtr (RIP.Ptr RIP.CChar -> IO (RIP.Ptr RIP.CChar))
foo7 =
  RIP.unsafePerformIO hs_bindgen_2804b55bcc6495bd

-- __unique:__ @test_comprehensivesmoke_Example_get_foo8@
foreign import ccall unsafe "hs_bindgen_dc0ab953845109e0" hs_bindgen_dc0ab953845109e0_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo8@
hs_bindgen_dc0ab953845109e0 :: IO (RIP.FunPtr (RIP.Ptr RIP.CChar -> IO (RIP.Ptr RIP.CChar)))
hs_bindgen_dc0ab953845109e0 =
  RIP.fromFFIType hs_bindgen_dc0ab953845109e0_base

{-# NOINLINE foo8 #-}
{-| __C declaration:__ @foo8@

    __defined at:__ @comprehensive\/smoke.h 17:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo8 :: RIP.FunPtr (RIP.Ptr RIP.CChar -> IO (RIP.Ptr RIP.CChar))
foo8 =
  RIP.unsafePerformIO hs_bindgen_dc0ab953845109e0

-- __unique:__ @test_comprehensivesmoke_Example_get_foo9@
foreign import ccall unsafe "hs_bindgen_6c6b48e80df5d80b" hs_bindgen_6c6b48e80df5d80b_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo9@
hs_bindgen_6c6b48e80df5d80b :: IO (RIP.FunPtr (RIP.FunPtr (IO (RIP.Ptr RIP.CChar)) -> IO (RIP.Ptr RIP.CChar)))
hs_bindgen_6c6b48e80df5d80b =
  RIP.fromFFIType hs_bindgen_6c6b48e80df5d80b_base

{-# NOINLINE foo9 #-}
{-| __C declaration:__ @foo9@

    __defined at:__ @comprehensive\/smoke.h 18:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo9 :: RIP.FunPtr (RIP.FunPtr (IO (RIP.Ptr RIP.CChar)) -> IO (RIP.Ptr RIP.CChar))
foo9 =
  RIP.unsafePerformIO hs_bindgen_6c6b48e80df5d80b

-- __unique:__ @test_comprehensivesmoke_Example_get_foo10@
foreign import ccall unsafe "hs_bindgen_fc0a56da1aa3e234" hs_bindgen_fc0a56da1aa3e234_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo10@
hs_bindgen_fc0a56da1aa3e234 :: IO (RIP.FunPtr (RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.CChar)) -> IO (RIP.Ptr RIP.CChar)))
hs_bindgen_fc0a56da1aa3e234 =
  RIP.fromFFIType hs_bindgen_fc0a56da1aa3e234_base

{-# NOINLINE foo10 #-}
{-| __C declaration:__ @foo10@

    __defined at:__ @comprehensive\/smoke.h 19:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo10 :: RIP.FunPtr (RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.CChar)) -> IO (RIP.Ptr RIP.CChar))
foo10 =
  RIP.unsafePerformIO hs_bindgen_fc0a56da1aa3e234

-- __unique:__ @test_comprehensivesmoke_Example_get_foo11@
foreign import ccall unsafe "hs_bindgen_b882a6f77db6bd0e" hs_bindgen_b882a6f77db6bd0e_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo11@
hs_bindgen_b882a6f77db6bd0e :: IO (RIP.FunPtr (RIP.FunPtr (IO (RIP.Ptr RIP.Void)) -> IO (RIP.Ptr RIP.Void)))
hs_bindgen_b882a6f77db6bd0e =
  RIP.fromFFIType hs_bindgen_b882a6f77db6bd0e_base

{-# NOINLINE foo11 #-}
{-| __C declaration:__ @foo11@

    __defined at:__ @comprehensive\/smoke.h 20:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo11 :: RIP.FunPtr (RIP.FunPtr (IO (RIP.Ptr RIP.Void)) -> IO (RIP.Ptr RIP.Void))
foo11 =
  RIP.unsafePerformIO hs_bindgen_b882a6f77db6bd0e

-- __unique:__ @test_comprehensivesmoke_Example_get_foo12@
foreign import ccall unsafe "hs_bindgen_c219c7d6d459a7e0" hs_bindgen_c219c7d6d459a7e0_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo12@
hs_bindgen_c219c7d6d459a7e0 :: IO (RIP.FunPtr (RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.Void)) -> IO (RIP.Ptr RIP.Void)))
hs_bindgen_c219c7d6d459a7e0 =
  RIP.fromFFIType hs_bindgen_c219c7d6d459a7e0_base

{-# NOINLINE foo12 #-}
{-| __C declaration:__ @foo12@

    __defined at:__ @comprehensive\/smoke.h 21:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo12 :: RIP.FunPtr (RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.Void)) -> IO (RIP.Ptr RIP.Void))
foo12 =
  RIP.unsafePerformIO hs_bindgen_c219c7d6d459a7e0

-- __unique:__ @test_comprehensivesmoke_Example_get_foo13@
foreign import ccall unsafe "hs_bindgen_caa14e75b1229a54" hs_bindgen_caa14e75b1229a54_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo13@
hs_bindgen_caa14e75b1229a54 :: IO (RIP.FunPtr (RIP.Ptr (IsA.Elem (IA.IncompleteArray RIP.CChar)) -> IO (RIP.Ptr RIP.CChar)))
hs_bindgen_caa14e75b1229a54 =
  RIP.fromFFIType hs_bindgen_caa14e75b1229a54_base

{-# NOINLINE foo13 #-}
{-| __C declaration:__ @foo13@

    __defined at:__ @comprehensive\/smoke.h 22:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo13 :: RIP.FunPtr (RIP.Ptr (IsA.Elem (IA.IncompleteArray RIP.CChar)) -> IO (RIP.Ptr RIP.CChar))
foo13 =
  RIP.unsafePerformIO hs_bindgen_caa14e75b1229a54

-- __unique:__ @test_comprehensivesmoke_Example_get_foo14@
foreign import ccall unsafe "hs_bindgen_3cb093b66f7944d4" hs_bindgen_3cb093b66f7944d4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo14@
hs_bindgen_3cb093b66f7944d4 :: IO (RIP.FunPtr (RIP.Ptr (IsA.Elem (IA.IncompleteArray RIP.CChar)) -> IO (RIP.Ptr RIP.CChar)))
hs_bindgen_3cb093b66f7944d4 =
  RIP.fromFFIType hs_bindgen_3cb093b66f7944d4_base

{-# NOINLINE foo14 #-}
{-| __C declaration:__ @foo14@

    __defined at:__ @comprehensive\/smoke.h 23:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo14 :: RIP.FunPtr (RIP.Ptr (IsA.Elem (IA.IncompleteArray RIP.CChar)) -> IO (RIP.Ptr RIP.CChar))
foo14 =
  RIP.unsafePerformIO hs_bindgen_3cb093b66f7944d4

-- __unique:__ @test_comprehensivesmoke_Example_get_foo15@
foreign import ccall unsafe "hs_bindgen_fe12aea8f26fdc44" hs_bindgen_fe12aea8f26fdc44_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo15@
hs_bindgen_fe12aea8f26fdc44 :: IO (RIP.FunPtr (RIP.Ptr (IsA.Elem (CA.ConstantArray 5 RIP.CChar)) -> IO (RIP.Ptr RIP.CChar)))
hs_bindgen_fe12aea8f26fdc44 =
  RIP.fromFFIType hs_bindgen_fe12aea8f26fdc44_base

{-# NOINLINE foo15 #-}
{-| __C declaration:__ @foo15@

    __defined at:__ @comprehensive\/smoke.h 24:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo15 :: RIP.FunPtr (RIP.Ptr (IsA.Elem (CA.ConstantArray 5 RIP.CChar)) -> IO (RIP.Ptr RIP.CChar))
foo15 =
  RIP.unsafePerformIO hs_bindgen_fe12aea8f26fdc44

-- __unique:__ @test_comprehensivesmoke_Example_get_foo16@
foreign import ccall unsafe "hs_bindgen_7682897a88740d24" hs_bindgen_7682897a88740d24_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo16@
hs_bindgen_7682897a88740d24 :: IO (RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.CChar)))
hs_bindgen_7682897a88740d24 =
  RIP.fromFFIType hs_bindgen_7682897a88740d24_base

{-# NOINLINE foo16 #-}
{-| __C declaration:__ @foo16@

    __defined at:__ @comprehensive\/smoke.h 25:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo16 :: RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.CChar))
foo16 =
  RIP.unsafePerformIO hs_bindgen_7682897a88740d24

-- __unique:__ @test_comprehensivesmoke_Example_get_foo17@
foreign import ccall unsafe "hs_bindgen_a8ad868a6dc425f9" hs_bindgen_a8ad868a6dc425f9_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo17@
hs_bindgen_a8ad868a6dc425f9 :: IO (RIP.FunPtr (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CChar)) -> IO RIP.CInt))
hs_bindgen_a8ad868a6dc425f9 =
  RIP.fromFFIType hs_bindgen_a8ad868a6dc425f9_base

{-# NOINLINE foo17 #-}
{-| __C declaration:__ @foo17@

    __defined at:__ @comprehensive\/smoke.h 26:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo17 :: RIP.FunPtr (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CChar)) -> IO RIP.CInt)
foo17 =
  RIP.unsafePerformIO hs_bindgen_a8ad868a6dc425f9

-- __unique:__ @test_comprehensivesmoke_Example_get_foo18@
foreign import ccall unsafe "hs_bindgen_245d12e0c60b976b" hs_bindgen_245d12e0c60b976b_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo18@
hs_bindgen_245d12e0c60b976b :: IO (RIP.FunPtr (RIP.CUInt -> IO RIP.CInt))
hs_bindgen_245d12e0c60b976b =
  RIP.fromFFIType hs_bindgen_245d12e0c60b976b_base

{-# NOINLINE foo18 #-}
{-| __C declaration:__ @foo18@

    __defined at:__ @comprehensive\/smoke.h 27:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo18 :: RIP.FunPtr (RIP.CUInt -> IO RIP.CInt)
foo18 =
  RIP.unsafePerformIO hs_bindgen_245d12e0c60b976b

-- __unique:__ @test_comprehensivesmoke_Example_get_foo19@
foreign import ccall unsafe "hs_bindgen_81d64147225cc2ab" hs_bindgen_81d64147225cc2ab_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo19@
hs_bindgen_81d64147225cc2ab :: IO (RIP.FunPtr (RIP.CUInt -> IO RIP.CInt))
hs_bindgen_81d64147225cc2ab =
  RIP.fromFFIType hs_bindgen_81d64147225cc2ab_base

{-# NOINLINE foo19 #-}
{-| __C declaration:__ @foo19@

    __defined at:__ @comprehensive\/smoke.h 28:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo19 :: RIP.FunPtr (RIP.CUInt -> IO RIP.CInt)
foo19 =
  RIP.unsafePerformIO hs_bindgen_81d64147225cc2ab

-- __unique:__ @test_comprehensivesmoke_Example_get_foo20@
foreign import ccall unsafe "hs_bindgen_b9d12fbbd2c70f4f" hs_bindgen_b9d12fbbd2c70f4f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo20@
hs_bindgen_b9d12fbbd2c70f4f :: IO (RIP.FunPtr (Uint -> IO RIP.CInt))
hs_bindgen_b9d12fbbd2c70f4f =
  RIP.fromFFIType hs_bindgen_b9d12fbbd2c70f4f_base

{-# NOINLINE foo20 #-}
{-| __C declaration:__ @foo20@

    __defined at:__ @comprehensive\/smoke.h 29:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo20 :: RIP.FunPtr (Uint -> IO RIP.CInt)
foo20 =
  RIP.unsafePerformIO hs_bindgen_b9d12fbbd2c70f4f

-- __unique:__ @test_comprehensivesmoke_Example_get_foo21@
foreign import ccall unsafe "hs_bindgen_a3c85769041747ba" hs_bindgen_a3c85769041747ba_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo21@
hs_bindgen_a3c85769041747ba :: IO (RIP.FunPtr (RIP.FunPtr (RIP.CInt -> IO RIP.CInt) -> IO RIP.CInt))
hs_bindgen_a3c85769041747ba =
  RIP.fromFFIType hs_bindgen_a3c85769041747ba_base

{-# NOINLINE foo21 #-}
{-| __C declaration:__ @foo21@

    __defined at:__ @comprehensive\/smoke.h 30:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo21 :: RIP.FunPtr (RIP.FunPtr (RIP.CInt -> IO RIP.CInt) -> IO RIP.CInt)
foo21 =
  RIP.unsafePerformIO hs_bindgen_a3c85769041747ba

-- __unique:__ @test_comprehensivesmoke_Example_get_foo22@
foreign import ccall unsafe "hs_bindgen_5fc122411b8a03ea" hs_bindgen_5fc122411b8a03ea_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo22@
hs_bindgen_5fc122411b8a03ea :: IO (RIP.FunPtr (RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.CInt)) -> IO RIP.CInt))
hs_bindgen_5fc122411b8a03ea =
  RIP.fromFFIType hs_bindgen_5fc122411b8a03ea_base

{-# NOINLINE foo22 #-}
{-| __C declaration:__ @foo22@

    __defined at:__ @comprehensive\/smoke.h 31:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo22 :: RIP.FunPtr (RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.CInt)) -> IO RIP.CInt)
foo22 =
  RIP.unsafePerformIO hs_bindgen_5fc122411b8a03ea

-- __unique:__ @test_comprehensivesmoke_Example_get_foo23@
foreign import ccall unsafe "hs_bindgen_a7fcbb7f1b7f29cb" hs_bindgen_a7fcbb7f1b7f29cb_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo23@
hs_bindgen_a7fcbb7f1b7f29cb :: IO (RIP.FunPtr (RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr (RIP.Ptr RIP.CInt))) -> IO RIP.CInt))
hs_bindgen_a7fcbb7f1b7f29cb =
  RIP.fromFFIType hs_bindgen_a7fcbb7f1b7f29cb_base

{-# NOINLINE foo23 #-}
{-| __C declaration:__ @foo23@

    __defined at:__ @comprehensive\/smoke.h 32:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo23 :: RIP.FunPtr (RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr (RIP.Ptr RIP.CInt))) -> IO RIP.CInt)
foo23 =
  RIP.unsafePerformIO hs_bindgen_a7fcbb7f1b7f29cb

-- __unique:__ @test_comprehensivesmoke_Example_get_foo24@
foreign import ccall unsafe "hs_bindgen_9c5a2b0e6d53b7c9" hs_bindgen_9c5a2b0e6d53b7c9_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo24@
hs_bindgen_9c5a2b0e6d53b7c9 :: IO (RIP.FunPtr (RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt)))) -> IO RIP.CInt))
hs_bindgen_9c5a2b0e6d53b7c9 =
  RIP.fromFFIType hs_bindgen_9c5a2b0e6d53b7c9_base

{-# NOINLINE foo24 #-}
{-| __C declaration:__ @foo24@

    __defined at:__ @comprehensive\/smoke.h 33:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo24 :: RIP.FunPtr (RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt)))) -> IO RIP.CInt)
foo24 =
  RIP.unsafePerformIO hs_bindgen_9c5a2b0e6d53b7c9

-- __unique:__ @test_comprehensivesmoke_Example_get_foo25@
foreign import ccall unsafe "hs_bindgen_5b576a9269d86a85" hs_bindgen_5b576a9269d86a85_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo25@
hs_bindgen_5b576a9269d86a85 :: IO (RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.CInt)))
hs_bindgen_5b576a9269d86a85 =
  RIP.fromFFIType hs_bindgen_5b576a9269d86a85_base

{-# NOINLINE foo25 #-}
{-| __C declaration:__ @foo25@

    __defined at:__ @comprehensive\/smoke.h 34:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo25 :: RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.CInt))
foo25 =
  RIP.unsafePerformIO hs_bindgen_5b576a9269d86a85

-- __unique:__ @test_comprehensivesmoke_Example_get_foo26@
foreign import ccall unsafe "hs_bindgen_3d968f750d7c0963" hs_bindgen_3d968f750d7c0963_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo26@
hs_bindgen_3d968f750d7c0963 :: IO (RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr (RIP.Ptr RIP.CInt))))
hs_bindgen_3d968f750d7c0963 =
  RIP.fromFFIType hs_bindgen_3d968f750d7c0963_base

{-# NOINLINE foo26 #-}
{-| __C declaration:__ @foo26@

    __defined at:__ @comprehensive\/smoke.h 35:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo26 :: RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr (RIP.Ptr RIP.CInt)))
foo26 =
  RIP.unsafePerformIO hs_bindgen_3d968f750d7c0963

-- __unique:__ @test_comprehensivesmoke_Example_get_foo27@
foreign import ccall unsafe "hs_bindgen_477d6debd15ec8cc" hs_bindgen_477d6debd15ec8cc_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo27@
hs_bindgen_477d6debd15ec8cc :: IO (RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt)))))
hs_bindgen_477d6debd15ec8cc =
  RIP.fromFFIType hs_bindgen_477d6debd15ec8cc_base

{-# NOINLINE foo27 #-}
{-| __C declaration:__ @foo27@

    __defined at:__ @comprehensive\/smoke.h 36:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo27 :: RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt))))
foo27 =
  RIP.unsafePerformIO hs_bindgen_477d6debd15ec8cc

-- __unique:__ @test_comprehensivesmoke_Example_get_foo28@
foreign import ccall unsafe "hs_bindgen_fe8d0bbb3e0e9949" hs_bindgen_fe8d0bbb3e0e9949_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_foo28@
hs_bindgen_fe8d0bbb3e0e9949 :: IO (RIP.FunPtr (Size_t -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt)))))
hs_bindgen_fe8d0bbb3e0e9949 =
  RIP.fromFFIType hs_bindgen_fe8d0bbb3e0e9949_base

{-# NOINLINE foo28 #-}
{-| __C declaration:__ @foo28@

    __defined at:__ @comprehensive\/smoke.h 37:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo28 :: RIP.FunPtr (Size_t -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt))))
foo28 =
  RIP.unsafePerformIO hs_bindgen_fe8d0bbb3e0e9949

-- __unique:__ @test_comprehensivesmoke_Example_get_inline_foo@
foreign import ccall unsafe "hs_bindgen_3e48bdcc0c25203c" hs_bindgen_3e48bdcc0c25203c_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_inline_foo@
hs_bindgen_3e48bdcc0c25203c :: IO (RIP.FunPtr (RIP.CInt -> RIP.Ptr RIP.CInt -> RIP.CInt -> PtrConst.PtrConst RIP.CInt -> RIP.Ptr (PtrConst.PtrConst RIP.CInt) -> PtrConst.PtrConst (PtrConst.PtrConst RIP.CInt) -> Size_t -> IO RIP.CInt))
hs_bindgen_3e48bdcc0c25203c =
  RIP.fromFFIType hs_bindgen_3e48bdcc0c25203c_base

{-# NOINLINE inline_foo #-}
{-| __C declaration:__ @inline_foo@

    __defined at:__ @comprehensive\/smoke.h 80:12@

    __exported by:__ @comprehensive\/smoke.h@
-}
inline_foo :: RIP.FunPtr (RIP.CInt -> RIP.Ptr RIP.CInt -> RIP.CInt -> PtrConst.PtrConst RIP.CInt -> RIP.Ptr (PtrConst.PtrConst RIP.CInt) -> PtrConst.PtrConst (PtrConst.PtrConst RIP.CInt) -> Size_t -> IO RIP.CInt)
inline_foo =
  RIP.unsafePerformIO hs_bindgen_3e48bdcc0c25203c
