{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module FFmpeg.LibAVUtil.Mem where

import Data.Void (Void)
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(CAPI.addCSource "#include \"libavutil/mem.h\"\nvoid *FFmpegLibAVUtilMem_av_malloc (size_t arg1) { return av_malloc(arg1); }\nvoid *FFmpegLibAVUtilMem_av_mallocz (size_t arg1) { return av_mallocz(arg1); }\nvoid *FFmpegLibAVUtilMem_av_malloc_array (size_t arg1, size_t arg2) { return av_malloc_array(arg1, arg2); }\nvoid *FFmpegLibAVUtilMem_av_calloc (size_t arg1, size_t arg2) { return av_calloc(arg1, arg2); }\nvoid *FFmpegLibAVUtilMem_av_realloc (void *arg1, size_t arg2) { return av_realloc(arg1, arg2); }\nsigned int FFmpegLibAVUtilMem_av_reallocp (void *arg1, size_t arg2) { return av_reallocp(arg1, arg2); }\nvoid *FFmpegLibAVUtilMem_av_realloc_f (void *arg1, size_t arg2, size_t arg3) { return av_realloc_f(arg1, arg2, arg3); }\nvoid *FFmpegLibAVUtilMem_av_realloc_array (void *arg1, size_t arg2, size_t arg3) { return av_realloc_array(arg1, arg2, arg3); }\nsigned int FFmpegLibAVUtilMem_av_reallocp_array (void *arg1, size_t arg2, size_t arg3) { return av_reallocp_array(arg1, arg2, arg3); }\nvoid *FFmpegLibAVUtilMem_av_fast_realloc (void *arg1, unsigned int *arg2, size_t arg3) { return av_fast_realloc(arg1, arg2, arg3); }\nvoid FFmpegLibAVUtilMem_av_fast_malloc (void *arg1, unsigned int *arg2, size_t arg3) { av_fast_malloc(arg1, arg2, arg3); }\nvoid FFmpegLibAVUtilMem_av_fast_mallocz (void *arg1, unsigned int *arg2, size_t arg3) { av_fast_mallocz(arg1, arg2, arg3); }\nvoid FFmpegLibAVUtilMem_av_free (void *arg1) { av_free(arg1); }\nvoid FFmpegLibAVUtilMem_av_freep (void *arg1) { av_freep(arg1); }\nchar *FFmpegLibAVUtilMem_av_strdup (char *arg1) { return av_strdup(arg1); }\nchar *FFmpegLibAVUtilMem_av_strndup (char *arg1, size_t arg2) { return av_strndup(arg1, arg2); }\nvoid *FFmpegLibAVUtilMem_av_memdup (void *arg1, size_t arg2) { return av_memdup(arg1, arg2); }\nvoid FFmpegLibAVUtilMem_av_memcpy_backptr (uint8_t *arg1, signed int arg2, signed int arg3) { av_memcpy_backptr(arg1, arg2, arg3); }\nvoid FFmpegLibAVUtilMem_av_dynarray_add (void *arg1, signed int *arg2, void *arg3) { av_dynarray_add(arg1, arg2, arg3); }\nsigned int FFmpegLibAVUtilMem_av_dynarray_add_nofree (void *arg1, signed int *arg2, void *arg3) { return av_dynarray_add_nofree(arg1, arg2, arg3); }\nvoid *FFmpegLibAVUtilMem_av_dynarray2_add (void **arg1, signed int *arg2, size_t arg3, uint8_t *arg4) { return av_dynarray2_add(arg1, arg2, arg3, arg4); }\nsigned int FFmpegLibAVUtilMem_av_size_mult (size_t arg1, size_t arg2, size_t *arg3) { return av_size_mult(arg1, arg2, arg3); }\nvoid FFmpegLibAVUtilMem_av_max_alloc (size_t arg1) { av_max_alloc(arg1); }\n")

foreign import ccall safe "FFmpegLibAVUtilMem_av_malloc" av_malloc :: HsBindgen.Runtime.Prelude.CSize -> IO (F.Ptr Void)

foreign import ccall safe "FFmpegLibAVUtilMem_av_mallocz" av_mallocz :: HsBindgen.Runtime.Prelude.CSize -> IO (F.Ptr Void)

foreign import ccall safe "FFmpegLibAVUtilMem_av_malloc_array" av_malloc_array :: HsBindgen.Runtime.Prelude.CSize -> HsBindgen.Runtime.Prelude.CSize -> IO (F.Ptr Void)

foreign import ccall safe "FFmpegLibAVUtilMem_av_calloc" av_calloc :: HsBindgen.Runtime.Prelude.CSize -> HsBindgen.Runtime.Prelude.CSize -> IO (F.Ptr Void)

foreign import ccall safe "FFmpegLibAVUtilMem_av_realloc" av_realloc :: (F.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> IO (F.Ptr Void)

foreign import ccall safe "FFmpegLibAVUtilMem_av_reallocp" av_reallocp :: (F.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CInt

foreign import ccall safe "FFmpegLibAVUtilMem_av_realloc_f" av_realloc_f :: (F.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> HsBindgen.Runtime.Prelude.CSize -> IO (F.Ptr Void)

foreign import ccall safe "FFmpegLibAVUtilMem_av_realloc_array" av_realloc_array :: (F.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> HsBindgen.Runtime.Prelude.CSize -> IO (F.Ptr Void)

foreign import ccall safe "FFmpegLibAVUtilMem_av_reallocp_array" av_reallocp_array :: (F.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CInt

foreign import ccall safe "FFmpegLibAVUtilMem_av_fast_realloc" av_fast_realloc :: (F.Ptr Void) -> (F.Ptr FC.CUInt) -> HsBindgen.Runtime.Prelude.CSize -> IO (F.Ptr Void)

foreign import ccall safe "FFmpegLibAVUtilMem_av_fast_malloc" av_fast_malloc :: (F.Ptr Void) -> (F.Ptr FC.CUInt) -> HsBindgen.Runtime.Prelude.CSize -> IO ()

foreign import ccall safe "FFmpegLibAVUtilMem_av_fast_mallocz" av_fast_mallocz :: (F.Ptr Void) -> (F.Ptr FC.CUInt) -> HsBindgen.Runtime.Prelude.CSize -> IO ()

foreign import ccall safe "FFmpegLibAVUtilMem_av_free" av_free :: (F.Ptr Void) -> IO ()

foreign import ccall safe "FFmpegLibAVUtilMem_av_freep" av_freep :: (F.Ptr Void) -> IO ()

foreign import ccall safe "FFmpegLibAVUtilMem_av_strdup" av_strdup :: (F.Ptr FC.CChar) -> IO (F.Ptr FC.CChar)

foreign import ccall safe "FFmpegLibAVUtilMem_av_strndup" av_strndup :: (F.Ptr FC.CChar) -> HsBindgen.Runtime.Prelude.CSize -> IO (F.Ptr FC.CChar)

foreign import ccall safe "FFmpegLibAVUtilMem_av_memdup" av_memdup :: (F.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> IO (F.Ptr Void)

foreign import ccall safe "FFmpegLibAVUtilMem_av_memcpy_backptr" av_memcpy_backptr :: (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> FC.CInt -> FC.CInt -> IO ()

foreign import ccall safe "FFmpegLibAVUtilMem_av_dynarray_add" av_dynarray_add :: (F.Ptr Void) -> (F.Ptr FC.CInt) -> (F.Ptr Void) -> IO ()

foreign import ccall safe "FFmpegLibAVUtilMem_av_dynarray_add_nofree" av_dynarray_add_nofree :: (F.Ptr Void) -> (F.Ptr FC.CInt) -> (F.Ptr Void) -> IO FC.CInt

foreign import ccall safe "FFmpegLibAVUtilMem_av_dynarray2_add" av_dynarray2_add :: (F.Ptr (F.Ptr Void)) -> (F.Ptr FC.CInt) -> HsBindgen.Runtime.Prelude.CSize -> (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> IO (F.Ptr Void)

foreign import ccall safe "FFmpegLibAVUtilMem_av_size_mult" av_size_mult :: HsBindgen.Runtime.Prelude.CSize -> HsBindgen.Runtime.Prelude.CSize -> (F.Ptr HsBindgen.Runtime.Prelude.CSize) -> IO FC.CInt

foreign import ccall safe "FFmpegLibAVUtilMem_av_max_alloc" av_max_alloc :: HsBindgen.Runtime.Prelude.CSize -> IO ()
