{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.malloc
    , Example.Unsafe.strlen
    , Example.Unsafe.my_strlen
    )
  where

import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/libc_builtins.h>"
  , "void *hs_bindgen_89545f6dff974fe2 ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return (malloc)(arg1);"
  , "}"
  , "unsigned long hs_bindgen_246dfd0630c4cbb6 ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return (strlen)(arg1);"
  , "}"
  , "size_t hs_bindgen_9674282b2423839e ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return (my_strlen)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionslibc_builtins_Example_Unsafe_malloc@
foreign import ccall unsafe "hs_bindgen_89545f6dff974fe2" hs_bindgen_89545f6dff974fe2_base ::
     BG.CULong
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_functionslibc_builtins_Example_Unsafe_malloc@
hs_bindgen_89545f6dff974fe2 ::
     BG.CULong
  -> IO (BG.Ptr BG.Void)
hs_bindgen_89545f6dff974fe2 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_89545f6dff974fe2_base (BG.toFFIType x0))

{-| __C declaration:__ @malloc@

    __defined at:__ @functions\/libc_builtins.h 7:7@

    __exported by:__ @functions\/libc_builtins.h@
-}
malloc ::
     BG.CULong
     -- ^ __C declaration:__ @size@
  -> IO (BG.Ptr BG.Void)
malloc = hs_bindgen_89545f6dff974fe2

-- __unique:__ @test_functionslibc_builtins_Example_Unsafe_strlen@
foreign import ccall unsafe "hs_bindgen_246dfd0630c4cbb6" hs_bindgen_246dfd0630c4cbb6_base ::
     BG.Ptr BG.Void
  -> IO BG.CULong

-- __unique:__ @test_functionslibc_builtins_Example_Unsafe_strlen@
hs_bindgen_246dfd0630c4cbb6 ::
     PtrConst.PtrConst BG.CChar
  -> IO BG.CULong
hs_bindgen_246dfd0630c4cbb6 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_246dfd0630c4cbb6_base (BG.toFFIType x0))

{-| __C declaration:__ @strlen@

    __defined at:__ @functions\/libc_builtins.h 8:8@

    __exported by:__ @functions\/libc_builtins.h@
-}
strlen ::
     PtrConst.PtrConst BG.CChar
     -- ^ __C declaration:__ @s@
  -> IO BG.CULong
strlen = hs_bindgen_246dfd0630c4cbb6

-- __unique:__ @test_functionslibc_builtins_Example_Unsafe_my_strlen@
foreign import ccall unsafe "hs_bindgen_9674282b2423839e" hs_bindgen_9674282b2423839e_base ::
     BG.Ptr BG.Void
  -> IO HsBindgen.Runtime.LibC.CSize

-- __unique:__ @test_functionslibc_builtins_Example_Unsafe_my_strlen@
hs_bindgen_9674282b2423839e ::
     PtrConst.PtrConst BG.CChar
  -> IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_9674282b2423839e =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_9674282b2423839e_base (BG.toFFIType x0))

{-| __C declaration:__ @my_strlen@

    __defined at:__ @functions\/libc_builtins.h 11:8@

    __exported by:__ @functions\/libc_builtins.h@
-}
my_strlen ::
     PtrConst.PtrConst BG.CChar
     -- ^ __C declaration:__ @s@
  -> IO HsBindgen.Runtime.LibC.CSize
my_strlen = hs_bindgen_9674282b2423839e
