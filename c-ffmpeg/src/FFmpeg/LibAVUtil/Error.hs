{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module FFmpeg.LibAVUtil.Error where

import qualified C.Expr.HostPlatform as C
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(CAPI.addCSource "#include \"libavutil/error.h\"\nsigned int FFmpegLibAVUtilError_av_strerror (signed int arg1, char *arg2, size_t arg3) { return av_strerror(arg1, arg2, arg3); }\nchar *FFmpegLibAVUtilError_av_make_error_string (char *arg1, size_t arg2, signed int arg3) { return av_make_error_string(arg1, arg2, arg3); }\n")

aVERROR :: forall a0. C.Minus a0 => a0 -> C.MinusRes a0
aVERROR = \e0 -> C.negate e0

aVUNERROR :: forall a0. C.Minus a0 => a0 -> C.MinusRes a0
aVUNERROR = \e0 -> C.negate e0

aVERROR_EXPERIMENTAL :: FC.CInt
aVERROR_EXPERIMENTAL =
  C.negate (733130664 :: FC.CInt)

aVERROR_INPUT_CHANGED :: FC.CInt
aVERROR_INPUT_CHANGED =
  C.negate (1668179713 :: FC.CInt)

aVERROR_OUTPUT_CHANGED :: FC.CInt
aVERROR_OUTPUT_CHANGED =
  C.negate (1668179714 :: FC.CInt)

aV_ERROR_MAX_STRING_SIZE :: FC.CInt
aV_ERROR_MAX_STRING_SIZE = (64 :: FC.CInt)

foreign import ccall safe "FFmpegLibAVUtilError_av_strerror" av_strerror :: FC.CInt -> (F.Ptr FC.CChar) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CInt

foreign import ccall safe "FFmpegLibAVUtilError_av_make_error_string" av_make_error_string :: (F.Ptr FC.CChar) -> HsBindgen.Runtime.Prelude.CSize -> FC.CInt -> IO (F.Ptr FC.CChar)
