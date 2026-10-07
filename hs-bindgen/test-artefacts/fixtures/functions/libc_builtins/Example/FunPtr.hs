{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.malloc
    , Example.FunPtr.strlen
    , Example.FunPtr.my_strlen
    )
  where

import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/libc_builtins.h>"
  , "/* test_functionslibc_builtins_Example_get_malloc */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_d52eebfcdc653f31 (void)) ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return &malloc;"
  , "}"
  , "/* test_functionslibc_builtins_Example_get_strlen */"
  , "__attribute__ ((const))"
  , "unsigned long (*hs_bindgen_7c713081d7e72a10 (void)) ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return &strlen;"
  , "}"
  , "/* test_functionslibc_builtins_Example_get_my_strlen */"
  , "__attribute__ ((const))"
  , "size_t (*hs_bindgen_a92eb31aeb03f8c0 (void)) ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return &my_strlen;"
  , "}"
  ]))

-- __unique:__ @test_functionslibc_builtins_Example_get_malloc@
foreign import ccall unsafe "hs_bindgen_d52eebfcdc653f31" hs_bindgen_d52eebfcdc653f31_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionslibc_builtins_Example_get_malloc@
hs_bindgen_d52eebfcdc653f31 :: IO (BG.FunPtr (BG.CULong -> IO (BG.Ptr BG.Void)))
hs_bindgen_d52eebfcdc653f31 =
  fmap BG.fromFFIType hs_bindgen_d52eebfcdc653f31_base

{-# NOINLINE malloc #-}
{-| __C declaration:__ @malloc@

    __defined at:__ @functions\/libc_builtins.h 7:7@

    __exported by:__ @functions\/libc_builtins.h@
-}
malloc :: BG.FunPtr (BG.CULong -> IO (BG.Ptr BG.Void))
malloc =
  BG.unsafePerformIO hs_bindgen_d52eebfcdc653f31

-- __unique:__ @test_functionslibc_builtins_Example_get_strlen@
foreign import ccall unsafe "hs_bindgen_7c713081d7e72a10" hs_bindgen_7c713081d7e72a10_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionslibc_builtins_Example_get_strlen@
hs_bindgen_7c713081d7e72a10 :: IO (BG.FunPtr (PtrConst.PtrConst BG.CChar -> IO BG.CULong))
hs_bindgen_7c713081d7e72a10 =
  fmap BG.fromFFIType hs_bindgen_7c713081d7e72a10_base

{-# NOINLINE strlen #-}
{-| __C declaration:__ @strlen@

    __defined at:__ @functions\/libc_builtins.h 8:8@

    __exported by:__ @functions\/libc_builtins.h@
-}
strlen :: BG.FunPtr (PtrConst.PtrConst BG.CChar -> IO BG.CULong)
strlen =
  BG.unsafePerformIO hs_bindgen_7c713081d7e72a10

-- __unique:__ @test_functionslibc_builtins_Example_get_my_strlen@
foreign import ccall unsafe "hs_bindgen_a92eb31aeb03f8c0" hs_bindgen_a92eb31aeb03f8c0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionslibc_builtins_Example_get_my_strlen@
hs_bindgen_a92eb31aeb03f8c0 :: IO (BG.FunPtr (PtrConst.PtrConst BG.CChar -> IO HsBindgen.Runtime.LibC.CSize))
hs_bindgen_a92eb31aeb03f8c0 =
  fmap BG.fromFFIType hs_bindgen_a92eb31aeb03f8c0_base

{-# NOINLINE my_strlen #-}
{-| __C declaration:__ @my_strlen@

    __defined at:__ @functions\/libc_builtins.h 11:8@

    __exported by:__ @functions\/libc_builtins.h@
-}
my_strlen :: BG.FunPtr (PtrConst.PtrConst BG.CChar -> IO HsBindgen.Runtime.LibC.CSize)
my_strlen =
  BG.unsafePerformIO hs_bindgen_a92eb31aeb03f8c0
