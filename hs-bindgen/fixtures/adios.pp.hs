{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (Bounded, Enum, Eq, IO, Integral, Num, Ord, Read, Real, Show)

$(CAPI.addCSource "#include <adios.h>\nvoid testmodule_\978 (void) { \978(); }\nvoid testmodule_\25308\25308 (void) { \25308\25308(); }\nvoid testmodule_Say\25308\25308 (void) { Say\25308\25308(); }\n")

newtype Adio'0301s = Adio'0301s
  { un_Adio'0301s :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype C数字 = C数字
  { un_C数字 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

foreign import ccall safe "testmodule_ϒ" cϒ :: IO ()

foreign import ccall safe "testmodule_拜拜" 拜拜 :: IO ()

foreign import ccall safe "testmodule_Say拜拜" say拜拜 :: IO ()
