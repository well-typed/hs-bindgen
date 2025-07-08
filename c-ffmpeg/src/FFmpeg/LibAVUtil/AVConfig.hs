{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NoImplicitPrelude #-}

module FFmpeg.LibAVUtil.AVConfig where

import qualified Foreign.C as FC

aV_HAVE_FAST_UNALIGNED :: FC.CInt
aV_HAVE_FAST_UNALIGNED = (1 :: FC.CInt)
