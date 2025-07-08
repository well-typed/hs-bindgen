{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module FFmpeg.LibAVUtil.Rational where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.Prelude
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

$(CAPI.addCSource "#include \"libavutil/rational.h\"\nvoid FFmpegLibAVUtilRational_av_make_q (signed int arg1, signed int arg2, AVRational *arg3) { *arg3 = av_make_q(arg1, arg2); }\nsigned int FFmpegLibAVUtilRational_av_cmp_q (AVRational *arg1, AVRational *arg2) { return av_cmp_q(*arg1, *arg2); }\ndouble FFmpegLibAVUtilRational_av_q2d (AVRational *arg1) { return av_q2d(*arg1); }\nsigned int FFmpegLibAVUtilRational_av_reduce (signed int *arg1, signed int *arg2, int64_t arg3, int64_t arg4, int64_t arg5) { return av_reduce(arg1, arg2, arg3, arg4, arg5); }\nvoid FFmpegLibAVUtilRational_av_mul_q (AVRational *arg1, AVRational *arg2, AVRational *arg3) { *arg3 = av_mul_q(*arg1, *arg2); }\nvoid FFmpegLibAVUtilRational_av_div_q (AVRational *arg1, AVRational *arg2, AVRational *arg3) { *arg3 = av_div_q(*arg1, *arg2); }\nvoid FFmpegLibAVUtilRational_av_add_q (AVRational *arg1, AVRational *arg2, AVRational *arg3) { *arg3 = av_add_q(*arg1, *arg2); }\nvoid FFmpegLibAVUtilRational_av_sub_q (AVRational *arg1, AVRational *arg2, AVRational *arg3) { *arg3 = av_sub_q(*arg1, *arg2); }\nvoid FFmpegLibAVUtilRational_av_inv_q (AVRational *arg1, AVRational *arg2) { *arg2 = av_inv_q(*arg1); }\nvoid FFmpegLibAVUtilRational_av_d2q (double arg1, signed int arg2, AVRational *arg3) { *arg3 = av_d2q(arg1, arg2); }\nsigned int FFmpegLibAVUtilRational_av_nearer_q (AVRational *arg1, AVRational *arg2, AVRational *arg3) { return av_nearer_q(*arg1, *arg2, *arg3); }\nsigned int FFmpegLibAVUtilRational_av_find_nearest_q_idx (AVRational *arg1, AVRational *arg2) { return av_find_nearest_q_idx(*arg1, arg2); }\nuint32_t FFmpegLibAVUtilRational_av_q2intfloat (AVRational *arg1) { return av_q2intfloat(*arg1); }\nvoid FFmpegLibAVUtilRational_av_gcd_q (AVRational *arg1, AVRational *arg2, signed int arg3, AVRational *arg4, AVRational *arg5) { *arg5 = av_gcd_q(*arg1, *arg2, arg3, *arg4); }\n")

data AVRational = AVRational
  { aVRational_num :: FC.CInt
  , aVRational_den :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable AVRational where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure AVRational
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          AVRational aVRational_num2 aVRational_den3 ->
               F.pokeByteOff ptr0 (0 :: Int) aVRational_num2
            >> F.pokeByteOff ptr0 (4 :: Int) aVRational_den3

foreign import ccall safe "FFmpegLibAVUtilRational_av_make_q" av_make_q_wrapper :: FC.CInt -> FC.CInt -> (F.Ptr AVRational) -> IO ()

av_make_q :: FC.CInt -> FC.CInt -> IO AVRational
av_make_q =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.CAPI.allocaAndPeek (\z2 ->
                                              av_make_q_wrapper x0 x1 z2)

foreign import ccall safe "FFmpegLibAVUtilRational_av_cmp_q" av_cmp_q_wrapper :: (F.Ptr AVRational) -> (F.Ptr AVRational) -> IO FC.CInt

av_cmp_q :: AVRational -> AVRational -> IO FC.CInt
av_cmp_q =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   F.with x0 (\y3 -> av_cmp_q_wrapper y3 y2))

foreign import ccall safe "FFmpegLibAVUtilRational_av_q2d" av_q2d_wrapper :: (F.Ptr AVRational) -> IO FC.CDouble

av_q2d :: AVRational -> IO FC.CDouble
av_q2d = \x0 -> F.with x0 (\y1 -> av_q2d_wrapper y1)

foreign import ccall safe "FFmpegLibAVUtilRational_av_reduce" av_reduce :: (F.Ptr FC.CInt) -> (F.Ptr FC.CInt) -> HsBindgen.Runtime.Prelude.Int64 -> HsBindgen.Runtime.Prelude.Int64 -> HsBindgen.Runtime.Prelude.Int64 -> IO FC.CInt

foreign import ccall safe "FFmpegLibAVUtilRational_av_mul_q" av_mul_q_wrapper :: (F.Ptr AVRational) -> (F.Ptr AVRational) -> (F.Ptr AVRational) -> IO ()

av_mul_q :: AVRational -> AVRational -> IO AVRational
av_mul_q =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   F.with x0 (\y3 ->
                                HsBindgen.Runtime.CAPI.allocaAndPeek (\z4 ->
                                                                        av_mul_q_wrapper y3 y2 z4)))

foreign import ccall safe "FFmpegLibAVUtilRational_av_div_q" av_div_q_wrapper :: (F.Ptr AVRational) -> (F.Ptr AVRational) -> (F.Ptr AVRational) -> IO ()

av_div_q :: AVRational -> AVRational -> IO AVRational
av_div_q =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   F.with x0 (\y3 ->
                                HsBindgen.Runtime.CAPI.allocaAndPeek (\z4 ->
                                                                        av_div_q_wrapper y3 y2 z4)))

foreign import ccall safe "FFmpegLibAVUtilRational_av_add_q" av_add_q_wrapper :: (F.Ptr AVRational) -> (F.Ptr AVRational) -> (F.Ptr AVRational) -> IO ()

av_add_q :: AVRational -> AVRational -> IO AVRational
av_add_q =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   F.with x0 (\y3 ->
                                HsBindgen.Runtime.CAPI.allocaAndPeek (\z4 ->
                                                                        av_add_q_wrapper y3 y2 z4)))

foreign import ccall safe "FFmpegLibAVUtilRational_av_sub_q" av_sub_q_wrapper :: (F.Ptr AVRational) -> (F.Ptr AVRational) -> (F.Ptr AVRational) -> IO ()

av_sub_q :: AVRational -> AVRational -> IO AVRational
av_sub_q =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   F.with x0 (\y3 ->
                                HsBindgen.Runtime.CAPI.allocaAndPeek (\z4 ->
                                                                        av_sub_q_wrapper y3 y2 z4)))

foreign import ccall safe "FFmpegLibAVUtilRational_av_inv_q" av_inv_q_wrapper :: (F.Ptr AVRational) -> (F.Ptr AVRational) -> IO ()

av_inv_q :: AVRational -> IO AVRational
av_inv_q =
  \x0 ->
    F.with x0 (\y1 ->
                 HsBindgen.Runtime.CAPI.allocaAndPeek (\z2 ->
                                                         av_inv_q_wrapper y1 z2))

foreign import ccall safe "FFmpegLibAVUtilRational_av_d2q" av_d2q_wrapper :: FC.CDouble -> FC.CInt -> (F.Ptr AVRational) -> IO ()

av_d2q :: FC.CDouble -> FC.CInt -> IO AVRational
av_d2q =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.CAPI.allocaAndPeek (\z2 ->
                                              av_d2q_wrapper x0 x1 z2)

foreign import ccall safe "FFmpegLibAVUtilRational_av_nearer_q" av_nearer_q_wrapper :: (F.Ptr AVRational) -> (F.Ptr AVRational) -> (F.Ptr AVRational) -> IO FC.CInt

av_nearer_q :: AVRational -> AVRational -> AVRational -> IO FC.CInt
av_nearer_q =
  \x0 ->
    \x1 ->
      \x2 ->
        F.with x2 (\y3 ->
                     F.with x1 (\y4 ->
                                  F.with x0 (\y5 ->
                                               av_nearer_q_wrapper y5 y4 y3)))

foreign import ccall safe "FFmpegLibAVUtilRational_av_find_nearest_q_idx" av_find_nearest_q_idx_wrapper :: (F.Ptr AVRational) -> (F.Ptr AVRational) -> IO FC.CInt

av_find_nearest_q_idx :: AVRational -> (F.Ptr AVRational) -> IO FC.CInt
av_find_nearest_q_idx =
  \x0 ->
    \x1 ->
      F.with x0 (\y2 ->
                   av_find_nearest_q_idx_wrapper y2 x1)

foreign import ccall safe "FFmpegLibAVUtilRational_av_q2intfloat" av_q2intfloat_wrapper :: (F.Ptr AVRational) -> IO HsBindgen.Runtime.Prelude.Word32

av_q2intfloat :: AVRational -> IO HsBindgen.Runtime.Prelude.Word32
av_q2intfloat =
  \x0 -> F.with x0 (\y1 -> av_q2intfloat_wrapper y1)

foreign import ccall safe "FFmpegLibAVUtilRational_av_gcd_q" av_gcd_q_wrapper :: (F.Ptr AVRational) -> (F.Ptr AVRational) -> FC.CInt -> (F.Ptr AVRational) -> (F.Ptr AVRational) -> IO ()

av_gcd_q :: AVRational -> AVRational -> FC.CInt -> AVRational -> IO AVRational
av_gcd_q =
  \x0 ->
    \x1 ->
      \x2 ->
        \x3 ->
          F.with x3 (\y4 ->
                       F.with x1 (\y5 ->
                                    F.with x0 (\y6 ->
                                                 HsBindgen.Runtime.CAPI.allocaAndPeek (\z7 ->
                                                                                         av_gcd_q_wrapper y6 y5 x2 y4 z7))))
