{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module FFmpeg.LibAVUtil.Version where

import C.Expr.HostPlatform ((.&.), (<), (>>))
import qualified C.Expr.HostPlatform as C
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Syntax as HsBindgen
import Prelude ((~))

aV_VERSION_MAJOR :: forall a0. (HsBindgen.IntLike a0) -> C.ShiftRes (HsBindgen.IntLike a0)
aV_VERSION_MAJOR = \a0 -> (>>) a0 (16 :: FC.CInt)

aV_VERSION_MINOR :: forall a0. ((~) FC.CInt) ((C.BitsRes (HsBindgen.IntLike a0)) FC.CInt) => (HsBindgen.IntLike a0) -> C.ShiftRes ((C.BitsRes (HsBindgen.IntLike a0)) FC.CInt)
aV_VERSION_MINOR =
  \a0 ->
    (>>) ((.&.) a0 (65280 :: FC.CInt)) (8 :: FC.CInt)

aV_VERSION_MICRO :: forall a0. (HsBindgen.IntLike a0) -> (C.BitsRes (HsBindgen.IntLike a0)) FC.CInt
aV_VERSION_MICRO = \a0 -> (.&.) a0 (255 :: FC.CInt)

lIBAVUTIL_VERSION_MAJOR :: FC.CInt
lIBAVUTIL_VERSION_MAJOR = (60 :: FC.CInt)

lIBAVUTIL_VERSION_MINOR :: FC.CInt
lIBAVUTIL_VERSION_MINOR = (4 :: FC.CInt)

lIBAVUTIL_VERSION_MICRO :: FC.CInt
lIBAVUTIL_VERSION_MICRO = (101 :: FC.CInt)

fF_API_MOD_UINTP2 :: FC.CInt
fF_API_MOD_UINTP2 =
  (<) lIBAVUTIL_VERSION_MAJOR (61 :: FC.CInt)

fF_API_RISCV_FD_ZBA :: FC.CInt
fF_API_RISCV_FD_ZBA =
  (<) lIBAVUTIL_VERSION_MAJOR (61 :: FC.CInt)

fF_API_VULKAN_FIXED_QUEUES :: FC.CInt
fF_API_VULKAN_FIXED_QUEUES =
  (<) lIBAVUTIL_VERSION_MAJOR (61 :: FC.CInt)

fF_API_OPT_INT_LIST :: FC.CInt
fF_API_OPT_INT_LIST =
  (<) lIBAVUTIL_VERSION_MAJOR (61 :: FC.CInt)

fF_API_OPT_PTR :: FC.CInt
fF_API_OPT_PTR =
  (<) lIBAVUTIL_VERSION_MAJOR (61 :: FC.CInt)
