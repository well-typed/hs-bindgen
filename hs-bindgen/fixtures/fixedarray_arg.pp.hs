{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.ConstantArray
import Prelude (Eq, IO, Show)

$(CAPI.addCSource "#include \"fixedarray_arg.h\"\nsigned int testmodule_fun_1 (signed int arg1, signed int *arg2) { return fun_1(arg1, arg2); }\nsigned int testmodule_fun_2 (signed int *arg1) { return fun_2(arg1); }\n")

foreign import ccall safe "testmodule_fun_1" fun_1_wrapper :: FC.CInt -> (F.Ptr FC.CInt) -> IO FC.CInt

fun_1 :: FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt
fun_1 =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x1 (\ptr2 -> fun_1_wrapper x0 ptr2)

newtype Triple = Triple
  { un_Triple :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }

deriving newtype instance F.Storable Triple

deriving stock instance Eq Triple

deriving stock instance Show Triple

foreign import ccall safe "testmodule_fun_2" fun_2_wrapper :: (F.Ptr FC.CInt) -> IO FC.CInt

fun_2 :: Triple -> IO FC.CInt
fun_2 =
  \x0 -> HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr1 -> fun_2_wrapper ptr1)
