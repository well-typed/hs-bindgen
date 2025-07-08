{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module FFmpeg.LibAVUtil.IntFloat where

import qualified Data.Array.Byte
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.Prelude
import qualified HsBindgen.Runtime.SizedByteArray
import Prelude (IO)

$(CAPI.addCSource "#include \"libavutil/intfloat.h\"\nfloat FFmpegLibAVUtilIntFloat_av_int2float (uint32_t arg1) { return av_int2float(arg1); }\nuint32_t FFmpegLibAVUtilIntFloat_av_float2int (float arg1) { return av_float2int(arg1); }\ndouble FFmpegLibAVUtilIntFloat_av_int2double (uint64_t arg1) { return av_int2double(arg1); }\nuint64_t FFmpegLibAVUtilIntFloat_av_double2int (double arg1) { return av_double2int(arg1); }\n")

newtype Av_intfloat32 = Av_intfloat32
  { un_Av_intfloat32 :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable Av_intfloat32

get_av_intfloat32_i :: Av_intfloat32 -> HsBindgen.Runtime.Prelude.Word32
get_av_intfloat32_i =
  HsBindgen.Runtime.ByteArray.getUnionPayload

set_av_intfloat32_i :: HsBindgen.Runtime.Prelude.Word32 -> Av_intfloat32
set_av_intfloat32_i =
  HsBindgen.Runtime.ByteArray.setUnionPayload

get_av_intfloat32_f :: Av_intfloat32 -> FC.CFloat
get_av_intfloat32_f =
  HsBindgen.Runtime.ByteArray.getUnionPayload

set_av_intfloat32_f :: FC.CFloat -> Av_intfloat32
set_av_intfloat32_f =
  HsBindgen.Runtime.ByteArray.setUnionPayload

newtype Av_intfloat64 = Av_intfloat64
  { un_Av_intfloat64 :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 8) 8 instance F.Storable Av_intfloat64

get_av_intfloat64_i :: Av_intfloat64 -> HsBindgen.Runtime.Prelude.Word64
get_av_intfloat64_i =
  HsBindgen.Runtime.ByteArray.getUnionPayload

set_av_intfloat64_i :: HsBindgen.Runtime.Prelude.Word64 -> Av_intfloat64
set_av_intfloat64_i =
  HsBindgen.Runtime.ByteArray.setUnionPayload

get_av_intfloat64_f :: Av_intfloat64 -> FC.CDouble
get_av_intfloat64_f =
  HsBindgen.Runtime.ByteArray.getUnionPayload

set_av_intfloat64_f :: FC.CDouble -> Av_intfloat64
set_av_intfloat64_f =
  HsBindgen.Runtime.ByteArray.setUnionPayload

foreign import ccall safe "FFmpegLibAVUtilIntFloat_av_int2float" av_int2float :: HsBindgen.Runtime.Prelude.Word32 -> IO FC.CFloat

foreign import ccall safe "FFmpegLibAVUtilIntFloat_av_float2int" av_float2int :: FC.CFloat -> IO HsBindgen.Runtime.Prelude.Word32

foreign import ccall safe "FFmpegLibAVUtilIntFloat_av_int2double" av_int2double :: HsBindgen.Runtime.Prelude.Word64 -> IO FC.CDouble

foreign import ccall safe "FFmpegLibAVUtilIntFloat_av_double2int" av_double2int :: FC.CDouble -> IO HsBindgen.Runtime.Prelude.Word64
