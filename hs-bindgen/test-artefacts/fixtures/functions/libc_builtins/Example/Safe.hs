{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.malloc
    , Example.Safe.strlen
    , Example.Safe.my_strlen
    )
  where

import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/libc_builtins.h>"
  , "void *hs_bindgen_eb5883c88daf674e ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return (malloc)(arg1);"
  , "}"
  , "unsigned long hs_bindgen_be2c0f129cd05083 ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return (strlen)(arg1);"
  , "}"
  , "size_t hs_bindgen_988f2c37e046701b ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return (my_strlen)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionslibc_builtins_Example_Safe_malloc@
foreign import ccall safe "hs_bindgen_eb5883c88daf674e" hs_bindgen_eb5883c88daf674e_base ::
     BG.CULong
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_functionslibc_builtins_Example_Safe_malloc@
hs_bindgen_eb5883c88daf674e ::
     BG.CULong
  -> IO (BG.Ptr BG.Void)
hs_bindgen_eb5883c88daf674e =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_eb5883c88daf674e_base (BG.toFFIType x0))

{-| __C declaration:__ @malloc@

    __defined at:__ @functions\/libc_builtins.h 7:7@

    __exported by:__ @functions\/libc_builtins.h@
-}
malloc ::
     BG.CULong
     -- ^ __C declaration:__ @size@
  -> IO (BG.Ptr BG.Void)
malloc = hs_bindgen_eb5883c88daf674e

-- __unique:__ @test_functionslibc_builtins_Example_Safe_strlen@
foreign import ccall safe "hs_bindgen_be2c0f129cd05083" hs_bindgen_be2c0f129cd05083_base ::
     BG.Ptr BG.Void
  -> IO BG.CULong

-- __unique:__ @test_functionslibc_builtins_Example_Safe_strlen@
hs_bindgen_be2c0f129cd05083 ::
     PtrConst.PtrConst BG.CChar
  -> IO BG.CULong
hs_bindgen_be2c0f129cd05083 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_be2c0f129cd05083_base (BG.toFFIType x0))

{-| __C declaration:__ @strlen@

    __defined at:__ @functions\/libc_builtins.h 8:8@

    __exported by:__ @functions\/libc_builtins.h@
-}
strlen ::
     PtrConst.PtrConst BG.CChar
     -- ^ __C declaration:__ @s@
  -> IO BG.CULong
strlen = hs_bindgen_be2c0f129cd05083

-- __unique:__ @test_functionslibc_builtins_Example_Safe_my_strlen@
foreign import ccall safe "hs_bindgen_988f2c37e046701b" hs_bindgen_988f2c37e046701b_base ::
     BG.Ptr BG.Void
  -> IO HsBindgen.Runtime.LibC.CSize

-- __unique:__ @test_functionslibc_builtins_Example_Safe_my_strlen@
hs_bindgen_988f2c37e046701b ::
     PtrConst.PtrConst BG.CChar
  -> IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_988f2c37e046701b =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_988f2c37e046701b_base (BG.toFFIType x0))

{-| __C declaration:__ @my_strlen@

    __defined at:__ @functions\/libc_builtins.h 11:8@

    __exported by:__ @functions\/libc_builtins.h@
-}
my_strlen ::
     PtrConst.PtrConst BG.CChar
     -- ^ __C declaration:__ @s@
  -> IO HsBindgen.Runtime.LibC.CSize
my_strlen = hs_bindgen_988f2c37e046701b
