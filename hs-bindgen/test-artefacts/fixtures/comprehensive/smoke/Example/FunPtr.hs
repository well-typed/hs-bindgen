{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <comprehensive/smoke.h>"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fcfa6579e0ba2271 (void)) (void)"
  , "{"
  , "  return &foo1;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cb57998df41ad580 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo2;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5dad78083c41f652 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &foo3;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo4 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_3865c78e6bc9fb45 (void)) (void)"
  , "{"
  , "  return &foo4;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo5 */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_8e51686c83838a3e (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo5;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo6 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_451a98dbee1701e2 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &foo6;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo7 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_26af17dc4a954efe (void)) ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return &foo7;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo8 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_f64b5cfee7bd3d5b (void)) ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return &foo8;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo9 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_85859676b9436dc7 (void)) ("
  , "  char *(*arg1) (void)"
  , ")"
  , "{"
  , "  return &foo9;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo10 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_23fcd63a8d46d946 (void)) ("
  , "  char *(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo10;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo11 */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_badf78d91b778f6c (void)) ("
  , "  void *(*arg1) (void)"
  , ")"
  , "{"
  , "  return &foo11;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo12 */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_7986d501cebbbf1f (void)) ("
  , "  void *(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo12;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo13 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_093562e6f3902cb6 (void)) ("
  , "  char arg1[]"
  , ")"
  , "{"
  , "  return &foo13;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo14 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_b8fe15eaaca70fd7 (void)) ("
  , "  char arg1[]"
  , ")"
  , "{"
  , "  return &foo14;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo15 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_93638e4068d54cd3 (void)) ("
  , "  char arg1[5]"
  , ")"
  , "{"
  , "  return &foo15;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo16 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_dd010e66d1d3c479 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo16;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo17 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_2aa0c6e3157e3fb5 (void)) ("
  , "  char ***arg1"
  , ")"
  , "{"
  , "  return &foo17;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo18 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_df49822f2f8014f2 (void)) ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return &foo18;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo19 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_c1756a051c3a3dc5 (void)) ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return &foo19;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo20 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_3ba81c54cbb5b026 (void)) ("
  , "  uint arg1"
  , ")"
  , "{"
  , "  return &foo20;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo21 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_6331297e0ee31729 (void)) ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo21;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo22 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_77f9e7c0dd874715 (void)) ("
  , "  signed int *(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo22;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo23 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_70f97050968bd401 (void)) ("
  , "  signed int **(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo23;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo24 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_76cd25b8906ab98d (void)) ("
  , "  signed int ***(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo24;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo25 */"
  , "__attribute__ ((const))"
  , "signed int *(*hs_bindgen_ac1f9ab6c1c9e435 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo25;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo26 */"
  , "__attribute__ ((const))"
  , "signed int **(*hs_bindgen_9d28fb15cc98f191 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo26;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo27 */"
  , "__attribute__ ((const))"
  , "signed int ***(*hs_bindgen_8154db24514a2a02 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo27;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_foo28 */"
  , "__attribute__ ((const))"
  , "signed int ***(*hs_bindgen_85a15f2a3e53042b (void)) ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return &foo28;"
  , "}"
  , "/* test_comprehensivesmoke\NULExample\NULget_inline_foo */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_8930d89ec757c95c (void)) ("
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

-- __unique:__ @test_comprehensivesmoke Example get_foo1@
foreign import ccall unsafe "hs_bindgen_fcfa6579e0ba2271" hs_bindgen_fcfa6579e0ba2271_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo1@
hs_bindgen_fcfa6579e0ba2271 :: IO (BG.FunPtr (IO ()))
hs_bindgen_fcfa6579e0ba2271 =
  fmap BG.fromFFIType hs_bindgen_fcfa6579e0ba2271_base

{-# NOINLINE foo1 #-}
{-| __C declaration:__ @foo1@

    __defined at:__ @comprehensive\/smoke.h 10:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo1 :: BG.FunPtr (IO ())
foo1 = BG.unsafePerformIO hs_bindgen_fcfa6579e0ba2271

-- __unique:__ @test_comprehensivesmoke Example get_foo2@
foreign import ccall unsafe "hs_bindgen_cb57998df41ad580" hs_bindgen_cb57998df41ad580_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo2@
hs_bindgen_cb57998df41ad580 :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_cb57998df41ad580 =
  fmap BG.fromFFIType hs_bindgen_cb57998df41ad580_base

{-# NOINLINE foo2 #-}
{-| __C declaration:__ @foo2@

    __defined at:__ @comprehensive\/smoke.h 11:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo2 :: BG.FunPtr (BG.CInt -> IO ())
foo2 = BG.unsafePerformIO hs_bindgen_cb57998df41ad580

-- __unique:__ @test_comprehensivesmoke Example get_foo3@
foreign import ccall unsafe "hs_bindgen_5dad78083c41f652" hs_bindgen_5dad78083c41f652_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo3@
hs_bindgen_5dad78083c41f652 :: IO (BG.FunPtr (BG.CInt -> BG.CInt -> IO ()))
hs_bindgen_5dad78083c41f652 =
  fmap BG.fromFFIType hs_bindgen_5dad78083c41f652_base

{-# NOINLINE foo3 #-}
{-| __C declaration:__ @foo3@

    __defined at:__ @comprehensive\/smoke.h 12:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo3 :: BG.FunPtr (BG.CInt -> BG.CInt -> IO ())
foo3 = BG.unsafePerformIO hs_bindgen_5dad78083c41f652

-- __unique:__ @test_comprehensivesmoke Example get_foo4@
foreign import ccall unsafe "hs_bindgen_3865c78e6bc9fb45" hs_bindgen_3865c78e6bc9fb45_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo4@
hs_bindgen_3865c78e6bc9fb45 :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_3865c78e6bc9fb45 =
  fmap BG.fromFFIType hs_bindgen_3865c78e6bc9fb45_base

{-# NOINLINE foo4 #-}
{-| __C declaration:__ @foo4@

    __defined at:__ @comprehensive\/smoke.h 13:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo4 :: BG.FunPtr (IO BG.CInt)
foo4 = BG.unsafePerformIO hs_bindgen_3865c78e6bc9fb45

-- __unique:__ @test_comprehensivesmoke Example get_foo5@
foreign import ccall unsafe "hs_bindgen_8e51686c83838a3e" hs_bindgen_8e51686c83838a3e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo5@
hs_bindgen_8e51686c83838a3e :: IO (BG.FunPtr (BG.CInt -> IO BG.CChar))
hs_bindgen_8e51686c83838a3e =
  fmap BG.fromFFIType hs_bindgen_8e51686c83838a3e_base

{-# NOINLINE foo5 #-}
{-| __C declaration:__ @foo5@

    __defined at:__ @comprehensive\/smoke.h 14:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo5 :: BG.FunPtr (BG.CInt -> IO BG.CChar)
foo5 = BG.unsafePerformIO hs_bindgen_8e51686c83838a3e

-- __unique:__ @test_comprehensivesmoke Example get_foo6@
foreign import ccall unsafe "hs_bindgen_451a98dbee1701e2" hs_bindgen_451a98dbee1701e2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo6@
hs_bindgen_451a98dbee1701e2 :: IO (BG.FunPtr (BG.CInt -> BG.CInt -> IO (BG.Ptr BG.CChar)))
hs_bindgen_451a98dbee1701e2 =
  fmap BG.fromFFIType hs_bindgen_451a98dbee1701e2_base

{-# NOINLINE foo6 #-}
{-| __C declaration:__ @foo6@

    __defined at:__ @comprehensive\/smoke.h 15:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo6 :: BG.FunPtr (BG.CInt -> BG.CInt -> IO (BG.Ptr BG.CChar))
foo6 = BG.unsafePerformIO hs_bindgen_451a98dbee1701e2

-- __unique:__ @test_comprehensivesmoke Example get_foo7@
foreign import ccall unsafe "hs_bindgen_26af17dc4a954efe" hs_bindgen_26af17dc4a954efe_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo7@
hs_bindgen_26af17dc4a954efe :: IO (BG.FunPtr (BG.Ptr BG.CChar -> IO (BG.Ptr BG.CChar)))
hs_bindgen_26af17dc4a954efe =
  fmap BG.fromFFIType hs_bindgen_26af17dc4a954efe_base

{-# NOINLINE foo7 #-}
{-| __C declaration:__ @foo7@

    __defined at:__ @comprehensive\/smoke.h 16:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo7 :: BG.FunPtr (BG.Ptr BG.CChar -> IO (BG.Ptr BG.CChar))
foo7 = BG.unsafePerformIO hs_bindgen_26af17dc4a954efe

-- __unique:__ @test_comprehensivesmoke Example get_foo8@
foreign import ccall unsafe "hs_bindgen_f64b5cfee7bd3d5b" hs_bindgen_f64b5cfee7bd3d5b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo8@
hs_bindgen_f64b5cfee7bd3d5b :: IO (BG.FunPtr (BG.Ptr BG.CChar -> IO (BG.Ptr BG.CChar)))
hs_bindgen_f64b5cfee7bd3d5b =
  fmap BG.fromFFIType hs_bindgen_f64b5cfee7bd3d5b_base

{-# NOINLINE foo8 #-}
{-| __C declaration:__ @foo8@

    __defined at:__ @comprehensive\/smoke.h 17:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo8 :: BG.FunPtr (BG.Ptr BG.CChar -> IO (BG.Ptr BG.CChar))
foo8 = BG.unsafePerformIO hs_bindgen_f64b5cfee7bd3d5b

-- __unique:__ @test_comprehensivesmoke Example get_foo9@
foreign import ccall unsafe "hs_bindgen_85859676b9436dc7" hs_bindgen_85859676b9436dc7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo9@
hs_bindgen_85859676b9436dc7 :: IO (BG.FunPtr (BG.FunPtr (IO (BG.Ptr BG.CChar)) -> IO (BG.Ptr BG.CChar)))
hs_bindgen_85859676b9436dc7 =
  fmap BG.fromFFIType hs_bindgen_85859676b9436dc7_base

{-# NOINLINE foo9 #-}
{-| __C declaration:__ @foo9@

    __defined at:__ @comprehensive\/smoke.h 18:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo9 :: BG.FunPtr (BG.FunPtr (IO (BG.Ptr BG.CChar)) -> IO (BG.Ptr BG.CChar))
foo9 = BG.unsafePerformIO hs_bindgen_85859676b9436dc7

-- __unique:__ @test_comprehensivesmoke Example get_foo10@
foreign import ccall unsafe "hs_bindgen_23fcd63a8d46d946" hs_bindgen_23fcd63a8d46d946_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo10@
hs_bindgen_23fcd63a8d46d946 :: IO (BG.FunPtr (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CChar)) -> IO (BG.Ptr BG.CChar)))
hs_bindgen_23fcd63a8d46d946 =
  fmap BG.fromFFIType hs_bindgen_23fcd63a8d46d946_base

{-# NOINLINE foo10 #-}
{-| __C declaration:__ @foo10@

    __defined at:__ @comprehensive\/smoke.h 19:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo10 :: BG.FunPtr (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CChar)) -> IO (BG.Ptr BG.CChar))
foo10 =
  BG.unsafePerformIO hs_bindgen_23fcd63a8d46d946

-- __unique:__ @test_comprehensivesmoke Example get_foo11@
foreign import ccall unsafe "hs_bindgen_badf78d91b778f6c" hs_bindgen_badf78d91b778f6c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo11@
hs_bindgen_badf78d91b778f6c :: IO (BG.FunPtr (BG.FunPtr (IO (BG.Ptr BG.Void)) -> IO (BG.Ptr BG.Void)))
hs_bindgen_badf78d91b778f6c =
  fmap BG.fromFFIType hs_bindgen_badf78d91b778f6c_base

{-# NOINLINE foo11 #-}
{-| __C declaration:__ @foo11@

    __defined at:__ @comprehensive\/smoke.h 20:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo11 :: BG.FunPtr (BG.FunPtr (IO (BG.Ptr BG.Void)) -> IO (BG.Ptr BG.Void))
foo11 =
  BG.unsafePerformIO hs_bindgen_badf78d91b778f6c

-- __unique:__ @test_comprehensivesmoke Example get_foo12@
foreign import ccall unsafe "hs_bindgen_7986d501cebbbf1f" hs_bindgen_7986d501cebbbf1f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo12@
hs_bindgen_7986d501cebbbf1f :: IO (BG.FunPtr (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.Void)) -> IO (BG.Ptr BG.Void)))
hs_bindgen_7986d501cebbbf1f =
  fmap BG.fromFFIType hs_bindgen_7986d501cebbbf1f_base

{-# NOINLINE foo12 #-}
{-| __C declaration:__ @foo12@

    __defined at:__ @comprehensive\/smoke.h 21:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo12 :: BG.FunPtr (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.Void)) -> IO (BG.Ptr BG.Void))
foo12 =
  BG.unsafePerformIO hs_bindgen_7986d501cebbbf1f

-- __unique:__ @test_comprehensivesmoke Example get_foo13@
foreign import ccall unsafe "hs_bindgen_093562e6f3902cb6" hs_bindgen_093562e6f3902cb6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo13@
hs_bindgen_093562e6f3902cb6 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CChar)) -> IO (BG.Ptr BG.CChar)))
hs_bindgen_093562e6f3902cb6 =
  fmap BG.fromFFIType hs_bindgen_093562e6f3902cb6_base

{-# NOINLINE foo13 #-}
{-| __C declaration:__ @foo13@

    __defined at:__ @comprehensive\/smoke.h 22:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo13 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CChar)) -> IO (BG.Ptr BG.CChar))
foo13 =
  BG.unsafePerformIO hs_bindgen_093562e6f3902cb6

-- __unique:__ @test_comprehensivesmoke Example get_foo14@
foreign import ccall unsafe "hs_bindgen_b8fe15eaaca70fd7" hs_bindgen_b8fe15eaaca70fd7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo14@
hs_bindgen_b8fe15eaaca70fd7 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CChar)) -> IO (BG.Ptr BG.CChar)))
hs_bindgen_b8fe15eaaca70fd7 =
  fmap BG.fromFFIType hs_bindgen_b8fe15eaaca70fd7_base

{-# NOINLINE foo14 #-}
{-| __C declaration:__ @foo14@

    __defined at:__ @comprehensive\/smoke.h 23:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo14 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CChar)) -> IO (BG.Ptr BG.CChar))
foo14 =
  BG.unsafePerformIO hs_bindgen_b8fe15eaaca70fd7

-- __unique:__ @test_comprehensivesmoke Example get_foo15@
foreign import ccall unsafe "hs_bindgen_93638e4068d54cd3" hs_bindgen_93638e4068d54cd3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo15@
hs_bindgen_93638e4068d54cd3 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CChar)) -> IO (BG.Ptr BG.CChar)))
hs_bindgen_93638e4068d54cd3 =
  fmap BG.fromFFIType hs_bindgen_93638e4068d54cd3_base

{-# NOINLINE foo15 #-}
{-| __C declaration:__ @foo15@

    __defined at:__ @comprehensive\/smoke.h 24:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo15 :: BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CChar)) -> IO (BG.Ptr BG.CChar))
foo15 =
  BG.unsafePerformIO hs_bindgen_93638e4068d54cd3

-- __unique:__ @test_comprehensivesmoke Example get_foo16@
foreign import ccall unsafe "hs_bindgen_dd010e66d1d3c479" hs_bindgen_dd010e66d1d3c479_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo16@
hs_bindgen_dd010e66d1d3c479 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CChar)))
hs_bindgen_dd010e66d1d3c479 =
  fmap BG.fromFFIType hs_bindgen_dd010e66d1d3c479_base

{-# NOINLINE foo16 #-}
{-| __C declaration:__ @foo16@

    __defined at:__ @comprehensive\/smoke.h 25:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo16 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CChar))
foo16 =
  BG.unsafePerformIO hs_bindgen_dd010e66d1d3c479

-- __unique:__ @test_comprehensivesmoke Example get_foo17@
foreign import ccall unsafe "hs_bindgen_2aa0c6e3157e3fb5" hs_bindgen_2aa0c6e3157e3fb5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo17@
hs_bindgen_2aa0c6e3157e3fb5 :: IO (BG.FunPtr (BG.Ptr (BG.Ptr (BG.Ptr BG.CChar)) -> IO BG.CInt))
hs_bindgen_2aa0c6e3157e3fb5 =
  fmap BG.fromFFIType hs_bindgen_2aa0c6e3157e3fb5_base

{-# NOINLINE foo17 #-}
{-| __C declaration:__ @foo17@

    __defined at:__ @comprehensive\/smoke.h 26:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo17 :: BG.FunPtr (BG.Ptr (BG.Ptr (BG.Ptr BG.CChar)) -> IO BG.CInt)
foo17 =
  BG.unsafePerformIO hs_bindgen_2aa0c6e3157e3fb5

-- __unique:__ @test_comprehensivesmoke Example get_foo18@
foreign import ccall unsafe "hs_bindgen_df49822f2f8014f2" hs_bindgen_df49822f2f8014f2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo18@
hs_bindgen_df49822f2f8014f2 :: IO (BG.FunPtr (BG.CUInt -> IO BG.CInt))
hs_bindgen_df49822f2f8014f2 =
  fmap BG.fromFFIType hs_bindgen_df49822f2f8014f2_base

{-# NOINLINE foo18 #-}
{-| __C declaration:__ @foo18@

    __defined at:__ @comprehensive\/smoke.h 27:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo18 :: BG.FunPtr (BG.CUInt -> IO BG.CInt)
foo18 =
  BG.unsafePerformIO hs_bindgen_df49822f2f8014f2

-- __unique:__ @test_comprehensivesmoke Example get_foo19@
foreign import ccall unsafe "hs_bindgen_c1756a051c3a3dc5" hs_bindgen_c1756a051c3a3dc5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo19@
hs_bindgen_c1756a051c3a3dc5 :: IO (BG.FunPtr (BG.CUInt -> IO BG.CInt))
hs_bindgen_c1756a051c3a3dc5 =
  fmap BG.fromFFIType hs_bindgen_c1756a051c3a3dc5_base

{-# NOINLINE foo19 #-}
{-| __C declaration:__ @foo19@

    __defined at:__ @comprehensive\/smoke.h 28:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo19 :: BG.FunPtr (BG.CUInt -> IO BG.CInt)
foo19 =
  BG.unsafePerformIO hs_bindgen_c1756a051c3a3dc5

-- __unique:__ @test_comprehensivesmoke Example get_foo20@
foreign import ccall unsafe "hs_bindgen_3ba81c54cbb5b026" hs_bindgen_3ba81c54cbb5b026_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo20@
hs_bindgen_3ba81c54cbb5b026 :: IO (BG.FunPtr (Uint -> IO BG.CInt))
hs_bindgen_3ba81c54cbb5b026 =
  fmap BG.fromFFIType hs_bindgen_3ba81c54cbb5b026_base

{-# NOINLINE foo20 #-}
{-| __C declaration:__ @foo20@

    __defined at:__ @comprehensive\/smoke.h 29:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo20 :: BG.FunPtr (Uint -> IO BG.CInt)
foo20 =
  BG.unsafePerformIO hs_bindgen_3ba81c54cbb5b026

-- __unique:__ @test_comprehensivesmoke Example get_foo21@
foreign import ccall unsafe "hs_bindgen_6331297e0ee31729" hs_bindgen_6331297e0ee31729_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo21@
hs_bindgen_6331297e0ee31729 :: IO (BG.FunPtr (BG.FunPtr (BG.CInt -> IO BG.CInt) -> IO BG.CInt))
hs_bindgen_6331297e0ee31729 =
  fmap BG.fromFFIType hs_bindgen_6331297e0ee31729_base

{-# NOINLINE foo21 #-}
{-| __C declaration:__ @foo21@

    __defined at:__ @comprehensive\/smoke.h 30:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo21 :: BG.FunPtr (BG.FunPtr (BG.CInt -> IO BG.CInt) -> IO BG.CInt)
foo21 =
  BG.unsafePerformIO hs_bindgen_6331297e0ee31729

-- __unique:__ @test_comprehensivesmoke Example get_foo22@
foreign import ccall unsafe "hs_bindgen_77f9e7c0dd874715" hs_bindgen_77f9e7c0dd874715_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo22@
hs_bindgen_77f9e7c0dd874715 :: IO (BG.FunPtr (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt)) -> IO BG.CInt))
hs_bindgen_77f9e7c0dd874715 =
  fmap BG.fromFFIType hs_bindgen_77f9e7c0dd874715_base

{-# NOINLINE foo22 #-}
{-| __C declaration:__ @foo22@

    __defined at:__ @comprehensive\/smoke.h 31:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo22 :: BG.FunPtr (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt)) -> IO BG.CInt)
foo22 =
  BG.unsafePerformIO hs_bindgen_77f9e7c0dd874715

-- __unique:__ @test_comprehensivesmoke Example get_foo23@
foreign import ccall unsafe "hs_bindgen_70f97050968bd401" hs_bindgen_70f97050968bd401_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo23@
hs_bindgen_70f97050968bd401 :: IO (BG.FunPtr (BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr BG.CInt))) -> IO BG.CInt))
hs_bindgen_70f97050968bd401 =
  fmap BG.fromFFIType hs_bindgen_70f97050968bd401_base

{-# NOINLINE foo23 #-}
{-| __C declaration:__ @foo23@

    __defined at:__ @comprehensive\/smoke.h 32:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo23 :: BG.FunPtr (BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr BG.CInt))) -> IO BG.CInt)
foo23 =
  BG.unsafePerformIO hs_bindgen_70f97050968bd401

-- __unique:__ @test_comprehensivesmoke Example get_foo24@
foreign import ccall unsafe "hs_bindgen_76cd25b8906ab98d" hs_bindgen_76cd25b8906ab98d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo24@
hs_bindgen_76cd25b8906ab98d :: IO (BG.FunPtr (BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr (BG.Ptr BG.CInt)))) -> IO BG.CInt))
hs_bindgen_76cd25b8906ab98d =
  fmap BG.fromFFIType hs_bindgen_76cd25b8906ab98d_base

{-# NOINLINE foo24 #-}
{-| __C declaration:__ @foo24@

    __defined at:__ @comprehensive\/smoke.h 33:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo24 :: BG.FunPtr (BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr (BG.Ptr BG.CInt)))) -> IO BG.CInt)
foo24 =
  BG.unsafePerformIO hs_bindgen_76cd25b8906ab98d

-- __unique:__ @test_comprehensivesmoke Example get_foo25@
foreign import ccall unsafe "hs_bindgen_ac1f9ab6c1c9e435" hs_bindgen_ac1f9ab6c1c9e435_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo25@
hs_bindgen_ac1f9ab6c1c9e435 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt)))
hs_bindgen_ac1f9ab6c1c9e435 =
  fmap BG.fromFFIType hs_bindgen_ac1f9ab6c1c9e435_base

{-# NOINLINE foo25 #-}
{-| __C declaration:__ @foo25@

    __defined at:__ @comprehensive\/smoke.h 34:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo25 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt))
foo25 =
  BG.unsafePerformIO hs_bindgen_ac1f9ab6c1c9e435

-- __unique:__ @test_comprehensivesmoke Example get_foo26@
foreign import ccall unsafe "hs_bindgen_9d28fb15cc98f191" hs_bindgen_9d28fb15cc98f191_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo26@
hs_bindgen_9d28fb15cc98f191 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr BG.CInt))))
hs_bindgen_9d28fb15cc98f191 =
  fmap BG.fromFFIType hs_bindgen_9d28fb15cc98f191_base

{-# NOINLINE foo26 #-}
{-| __C declaration:__ @foo26@

    __defined at:__ @comprehensive\/smoke.h 35:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo26 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr BG.CInt)))
foo26 =
  BG.unsafePerformIO hs_bindgen_9d28fb15cc98f191

-- __unique:__ @test_comprehensivesmoke Example get_foo27@
foreign import ccall unsafe "hs_bindgen_8154db24514a2a02" hs_bindgen_8154db24514a2a02_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo27@
hs_bindgen_8154db24514a2a02 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr (BG.Ptr BG.CInt)))))
hs_bindgen_8154db24514a2a02 =
  fmap BG.fromFFIType hs_bindgen_8154db24514a2a02_base

{-# NOINLINE foo27 #-}
{-| __C declaration:__ @foo27@

    __defined at:__ @comprehensive\/smoke.h 36:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo27 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr (BG.Ptr BG.CInt))))
foo27 =
  BG.unsafePerformIO hs_bindgen_8154db24514a2a02

-- __unique:__ @test_comprehensivesmoke Example get_foo28@
foreign import ccall unsafe "hs_bindgen_85a15f2a3e53042b" hs_bindgen_85a15f2a3e53042b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_foo28@
hs_bindgen_85a15f2a3e53042b :: IO (BG.FunPtr (Size_t -> IO (BG.Ptr (BG.Ptr (BG.Ptr BG.CInt)))))
hs_bindgen_85a15f2a3e53042b =
  fmap BG.fromFFIType hs_bindgen_85a15f2a3e53042b_base

{-# NOINLINE foo28 #-}
{-| __C declaration:__ @foo28@

    __defined at:__ @comprehensive\/smoke.h 37:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo28 :: BG.FunPtr (Size_t -> IO (BG.Ptr (BG.Ptr (BG.Ptr BG.CInt))))
foo28 =
  BG.unsafePerformIO hs_bindgen_85a15f2a3e53042b

-- __unique:__ @test_comprehensivesmoke Example get_inline_foo@
foreign import ccall unsafe "hs_bindgen_8930d89ec757c95c" hs_bindgen_8930d89ec757c95c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivesmoke Example get_inline_foo@
hs_bindgen_8930d89ec757c95c :: IO (BG.FunPtr (BG.CInt -> BG.Ptr BG.CInt -> BG.CInt -> PtrConst.PtrConst BG.CInt -> BG.Ptr (PtrConst.PtrConst BG.CInt) -> PtrConst.PtrConst (PtrConst.PtrConst BG.CInt) -> Size_t -> IO BG.CInt))
hs_bindgen_8930d89ec757c95c =
  fmap BG.fromFFIType hs_bindgen_8930d89ec757c95c_base

{-# NOINLINE inline_foo #-}
{-| __C declaration:__ @inline_foo@

    __defined at:__ @comprehensive\/smoke.h 80:12@

    __exported by:__ @comprehensive\/smoke.h@
-}
inline_foo :: BG.FunPtr (BG.CInt -> BG.Ptr BG.CInt -> BG.CInt -> PtrConst.PtrConst BG.CInt -> BG.Ptr (PtrConst.PtrConst BG.CInt) -> PtrConst.PtrConst (PtrConst.PtrConst BG.CInt) -> Size_t -> IO BG.CInt)
inline_foo =
  BG.unsafePerformIO hs_bindgen_8930d89ec757c95c
